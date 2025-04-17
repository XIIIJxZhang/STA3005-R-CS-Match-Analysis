# FaceitMatchPrediction.R - 完整可用版
# 加载必要的包
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(glue)
library(stringi)
library(stringr)
library(rlang)

# 如果rlang包未提供 %||% 运算符，手动定义
`%||%` <- function(x, y) if (is.null(x)) y else x

# -------------------- 增强型API访问函数 --------------------
get_player_data <- function(nickname, auth_key) {
  tryCatch({
    # 编码特殊字符
    encoded_nick <- URLencode(nickname, reserved = TRUE)
    url <- glue("https://open.faceit.com/data/v4/players?nickname={encoded_nick}")

    response <- GET(
      url,
      add_headers(Authorization = auth_key),
      timeout(10)
    )

    # 处理HTTP状态码
    if (status_code(response) != 200) {
      return(list(
        valid = FALSE,
        type = "API_ERROR",
        message = glue("API请求失败 [{nickname}] 状态码: {status_code(response)}")
      ))
    }

    data <- content(response, "parsed")

    # 数据验证
    validation_checks <- list(
      player_id = is.null(data$player_id),
      cs2_data = is.null(data$games$cs2),
      elo_data = is.null(data$games$cs2$faceit_elo)
    )

    if (any(unlist(validation_checks))) {
      error_type <- case_when(
        validation_checks$player_id ~ "PLAYER_NOT_FOUND",
        validation_checks$cs2_data ~ "NO_CS2_PROFILE",
        validation_checks$elo_data ~ "NO_ELO_DATA",
        TRUE ~ "UNKNOWN_ERROR"
      )
      return(list(
        valid = FALSE,
        type = error_type,
        message = glue("数据验证失败 [{nickname}]: {switch(error_type,
          'PLAYER_NOT_FOUND' = '玩家不存在',
          'NO_CS2_PROFILE' = '无CS2游戏资料',
          'NO_ELO_DATA' = '无ELO数据',
          '未知错误')}"
        )
      ))
    }

    # 返回标准化数据结构
    list(
      valid = TRUE,
      data = list(
        player_id = data$player_id,
        nickname = nickname,
        elo = data$games$cs2$faceit_elo,
        game_profile = data$games$cs2
      )
    )
  }, error = function(e) {
    return(list(
      valid = FALSE,
      type = "SYSTEM_ERROR",
      message = glue("系统错误 [{nickname}]: {e$message}")
    ))
  })
}

# -------------------- 比赛数据获取模块 --------------------
get_player_stats <- function(player_id, nickname, auth_key) {
  tryCatch({
    url <- glue("https://open.faceit.com/data/v4/players/{player_id}/games/cs2/stats")

    response <- GET(
      url,
      add_headers(Authorization = auth_key),
      query = list(limit = 100),
      timeout(10)
    )

    if (status_code(response) != 200) {
      return(list(
        valid = FALSE,
        type = "API_ERROR",
        message = glue("比赛数据获取失败 [{nickname}] 状态码: {status_code(response)}")
      ))
    }

    stats_data <- content(response, "parsed")

    # 数据结构验证
    if (length(stats_data$items) == 0) {
      return(list(
        valid = FALSE,
        type = "NO_RECENT_MATCHES",
        message = glue("无近期比赛记录 [{nickname}]")
      ))
    }

    # 处理数据
    processed_data <- stats_data$items %>%
      map_df(~{
        list(
          match_id = .x$match_id %||% NA_character_,
          map = .x$stats$Map %||% NA_character_,
          result = as.numeric(.x$stats$Result %||% NA_real_),
          kd_ratio = as.numeric(.x$stats$`K/D Ratio` %||% NA_real_)
        )
      })

    # 返回处理后的数据
    list(
      valid = TRUE,
      data = processed_data,
      metadata = list(
        nickname = nickname,
        matches_processed = nrow(processed_data)
      )
    )
  }, error = function(e) {
    return(list(
      valid = FALSE,
      type = "SYSTEM_ERROR",
      message = glue("数据处理失败 [{nickname}]: {e$message}")
    ))
  })
}

# -------------------- 高级数据分析模块 --------------------
calculate_map_performance <- function(stats_df, target_map) {
  tryCatch({
    # 输入验证
    required_columns <- c("map", "result", "kd_ratio")
    missing_cols <- setdiff(required_columns, names(stats_df))
    if (length(missing_cols) > 0) {
      stop(glue("缺少必要字段: {paste(missing_cols, collapse=', ')}"))
    }

    # 地图数据过滤
    map_data <- stats_df %>%
      filter(tolower(map) == tolower(target_map))

    if (nrow(map_data) == 0) {
      return(list(
        valid = FALSE,
        type = "NO_MAP_DATA",
        message = glue("无{target_map}比赛数据")
      ))
    }

    # 计算性能指标
    performance_metrics <- list(
      win_rate = mean(map_data$result, na.rm = TRUE) * 100,
      kd_ratio = mean(map_data$kd_ratio, na.rm = TRUE),
      consistency = sd(map_data$result, na.rm = TRUE),
      matches_analyzed = nrow(map_data)
    )

    list(
      valid = TRUE,
      metrics = performance_metrics
    )
  }, error = function(e) {
    return(list(
      valid = FALSE,
      type = "ANALYSIS_ERROR",
      message = glue("分析失败: {e$message}")
    ))
  })
}

# -------------------- 预测引擎 --------------------
predict_match_outcome <- function(team_a, team_b, map_name, auth_key) {
  # 初始化数据存储
  analysis_results <- list()
  error_log <- list()

  # 定义队伍处理函数
  process_team <- function(team, team_name) {
    team_results <- list()

    for (player in team) {
      # 获取玩家基础数据
      player_data <- get_player_data(player, auth_key)

      # 验证数据完整性
      if (!player_data$valid) {
        error_log[[glue("{team_name}_{player}")]] <<- player_data$message
        next
      }

      # 获取比赛数据
      stats <- get_player_stats(player_data$data$player_id, player, auth_key)
      if (!stats$valid) {
        error_log[[glue("{team_name}_{player}")]] <<- stats$message
        next
      }

      # 计算地图表现
      map_perf <- calculate_map_performance(stats$data, map_name)
      if (!map_perf$valid) {
        error_log[[glue("{team_name}_{player}")]] <<- map_perf$message
        next
      }

      # 存储结果
      team_results[[player]] <- map_perf$metrics
    }

    # 计算队伍指标
    if (length(team_results) == 0) return(NULL)

    result <- tibble(
      player = names(team_results),
      win_rate = map_dbl(team_results, "win_rate"),
      kd_ratio = map_dbl(team_results, "kd_ratio")
    ) %>%
      summarise(
        avg_win = mean(win_rate, na.rm = TRUE),
        avg_kd = mean(kd_ratio, na.rm = TRUE),
        team_consistency = sd(win_rate, na.rm = TRUE)
      )

    return(result)
  }

  # 处理两支队伍
  team_a_stats <- process_team(team_a, "A")
  team_b_stats <- process_team(team_b, "B")

  # 生成最终报告
  if (is.null(team_a_stats) || is.null(team_b_stats)) {
    return(list(
      valid = FALSE,
      errors = error_log
    ))
  }

  # 返回预测结果
  list(
    valid = TRUE,
    prediction = list(
      team_a = team_a_stats,
      team_b = team_b_stats,
      map = map_name,
      win_diff = abs(team_a_stats$avg_win - team_b_stats$avg_win)
    ),
    errors = error_log,
    metadata = list(
      total_players = length(c(team_a, team_b)),
      players_analyzed = length(names(team_a_stats)) + length(names(team_b_stats))
    )
  )
}

# -------------------- 交互界面增强版 --------------------
advanced_prediction_ui <- function() {
  cat("\033[34m=== Faceit高级比赛预测系统 ===\033[0m\n")

  # API密钥输入
  auth_key <- readline("请输入Faceit API密钥: ")
  if (nchar(auth_key) < 10) {
    cat("\033[31m警告: API密钥看起来可能无效，请确认密钥格式\033[0m\n")
    continue <- tolower(readline("是否继续? (y/n): "))
    if (continue != "y") return(invisible(NULL))
  }

  # 队伍输入增强
  input_team <- function(team_name) {
    cat(glue("\n\033[32m[输入{team_name}队伍成员]\033[0m\n"))
    players <- map(1:5, ~{
      repeat {
        input <- trimws(readline(glue("玩家{.x}: ")))
        if (nchar(input) > 0) break
        cat("\033[31m输入不能为空！\033[0m\n")
      }
      input
    })
    return(players)
  }

  # 获取队伍数据
  team_a <- input_team("A")
  team_b <- input_team("B")

  # 地图输入验证
  repeat {
    map_name <- tolower(trimws(readline("比赛地图 (示例: de_dust2): ")))
    if (grepl("^de_[a-z0-9_]{2,}$", map_name)) break
    cat("\033[33m无效地图格式！请使用官方地图名称（如de_dust2）\033[0m\n")
  }

  # 显示处理信息
  cat("\n\033[36m正在分析玩家数据...\033[0m\n")

  # 执行预测
  prediction <- predict_match_outcome(team_a, team_b, map_name, auth_key)

  # 结果展示
  if (prediction$valid) {
    cat("\n\033[36m=== 预测结果 ===\033[0m\n")
    cat(glue("地图: \033[1m{prediction$prediction$map}\033[0m\n"))
    cat(glue("队伍A平均胜率: \033[34m{round(prediction$prediction$team_a$avg_win, 1)}%\033[0m\n"))
    cat(glue("队伍B平均胜率: \033[31m{round(prediction$prediction$team_b$avg_win, 1)}%\033[0m\n"))
    cat(glue("胜率差距: \033[33m{round(prediction$prediction$win_diff, 1)}%\033[0m\n"))

    # 队伍A K/D比率
    cat(glue("队伍A平均K/D比率: \033[34m{round(prediction$prediction$team_a$avg_kd, 2)}\033[0m\n"))
    # 队伍B K/D比率
    cat(glue("队伍B平均K/D比率: \033[31m{round(prediction$prediction$team_b$avg_kd, 2)}\033[0m\n"))

    # 预测结论
    if (prediction$prediction$win_diff < 5) {
      cat("\033[33m预测结论: 势均力敌的比赛\033[0m\n")
    } else {
      winner <- ifelse(prediction$prediction$team_a$avg_win > prediction$prediction$team_b$avg_win,
                       "队伍A", "队伍B")
      cat(glue("\033[35m预测结论: {winner} 具有明显优势\033[0m\n"))
    }

    # 显示任何错误信息
    if (length(prediction$errors) > 0) {
      cat("\n\033[33m注意: 部分玩家数据分析失败，结果可能不完全准确\033[0m\n")
    }
  } else {
    cat("\n\033[31m=== 错误报告 ===\033[0m\n")
    if (length(prediction$errors) > 0) {
      walk(names(prediction$errors), ~{
        parts <- str_split(.x, "_", 2)[[1]]
        team_name <- if (length(parts) >= 2) parts[1] else "未知"
        player_name <- if (length(parts) >= 2) parts[2] else parts[1]
        cat(glue("队伍{team_name}玩家 [{player_name}]: {prediction$errors[[.x]]}\n"))
      })
    } else {
      cat("无法获取足够的玩家数据进行预测\n")
    }
  }

  # 询问是否要继续进行新的预测
  cat("\n")
  continue <- tolower(readline("是否进行新的预测? (y/n): "))
  if (continue == "y") {
    advanced_prediction_ui()
  } else {
    cat("\033[36m谢谢使用！\033[0m\n")
  }
}

# -------------------- 执行主程序 --------------------
# 错误捕捉和处理
tryCatch({
  advanced_prediction_ui()
}, error = function(e) {
  cat("\n\033[31m程序发生错误:\033[0m\n")
  cat(paste(e$message, "\n"))
  cat("\033[33m可能的原因:\033[0m\n")
  cat("1. API密钥无效或格式错误\n")
  cat("2. 网络连接问题\n")
  cat("3. Faceit API服务器暂时不可用\n")
  cat("4. 玩家昵称不存在或包含特殊字符\n")
  cat("\n尝试重新运行程序或稍后再试\n")
})
