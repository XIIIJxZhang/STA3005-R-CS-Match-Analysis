# 导入必要的库
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# 获取玩家数据（ID和ELO）的函数
get_player_data <- function(nickname, auth_key) {
  # API请求参数
  parameters <- list(
    nickname = nickname
  )

  # FACEIT URL获取玩家ID数据
  url_player_id <- "https://open.faceit.com/data/v4/players"

  # API密钥
  headers <- add_headers(
    Authorization = auth_key
  )

  # 从API请求玩家信息
  response <- GET(url_player_id,
                  headers = headers,
                  query = parameters)

  # 检查请求错误
  if (status_code(response) == 404) {
    cat(sprintf("Error 404: Resource not found. Requested URL: %s\n", response$url))

    # 返回空数据框
    stats <- data.frame()
    return(stats)
  } else if (status_code(response) == 200) {
    player_data <- content(response, "parsed")

    # 使用tryCatch捕获可能的错误
    tryCatch({
      player_data_list <- list(
        player_id = player_data$player_id,
        faceit_elo = player_data$games$cs2$faceit_elo
      )
      player_data_df <- as.data.frame(player_data_list)

      return(player_data_df)
    }, error = function(e) {
      # 错误通常是由于CSGO匹配ID导致的
      cat("match_id for CSGO\n")

      # 返回空数据框
      stats <- data.frame()
      return(stats)
    })
  } else {
    # 处理不常见的请求错误
    cat(sprintf("Error %d: %s\n", status_code(response), content(response, "text")))
    stats <- data.frame()
    return(stats)
  }
}

# 获取玩家最近100场比赛的统计数据
get_player_stats <- function(player_id, auth_key) {
  # API请求所需的参数
  parameters <- list(
    player_id = player_id,
    limit = 100
  )

  # 用于API请求的URL
  game_id <- "cs2"
  url_stats <- sprintf("https://open.faceit.com/data/v4/players/%s/games/%s/stats", player_id, game_id)

  headers <- add_headers(
    Authorization = auth_key
  )

  # 请求玩家最近100场比赛的数据
  response <- GET(url_stats,
                  headers = headers,
                  query = parameters)

  # 将嵌套的JSON转换为数据框
  data <- content(response, "parsed")

  # R中处理嵌套JSON
  stats_list <- lapply(data$items, function(item) {
    # 从各个层级提取数据，创建扁平的列表
    stats <- list()

    # 假定stats为在"item"内的对象
    if (!is.null(item$stats)) {
      for (name in names(item$stats)) {
        stats[[paste0("stats_", name)]] <- item$stats[[name]]
      }
    }

    # 添加其他重要字段
    for (field in names(item)) {
      if (field != "stats") {
        stats[[field]] <- item[[field]]
      }
    }

    return(stats)
  })

  # 将列表转换为数据框
  stats_df <- bind_rows(stats_list)

  # 返回玩家统计数据框
  return(stats_df)
}

# 计算玩家在特定地图上的统计数据
player_stats_calc <- function(df, map) {
  # 测试地图是否存在
  tryCatch({
    test_df <- df[df$stats_Map == map, ]

    # 如果找不到地图，返回空数据框
    if (nrow(test_df) == 0) {
      stats <- data.frame()
      return(stats)
    }

    # 转换Result和K/R比率列为数值型
    test_df$stats_Result <- as.numeric(test_df$stats_Result)
    test_df$`stats_K/R Ratio` <- as.numeric(test_df$`stats_K/R Ratio`)

    tryCatch({
      # 计算胜率
      map_win_percentage <- (sum(test_df$stats_Result) / nrow(test_df)) * 100

      # 计算平均K/R比率
      average_KR_ratio <- mean(test_df$`stats_K/R Ratio`)

      # 合并统计并重命名数据框的轴
      stats <- data.frame(
        "Win Percentage" = map_win_percentage,
        "Average K/R Ratio" = average_KR_ratio
      )

      # 返回统计数据框
      return(stats)
    }, error = function(e) {
      stats <- data.frame()
      cat("Error in calculation\n")
      return(stats)
    })
  }, error = function(e) {
    stats <- data.frame()
    return(stats)
  })
}

# 从排行榜获取玩家ID
get_ranking_player_ids <- function(offset, auth_key) {
  # 请求参数
  parameters <- list(
    offset = offset,
    limit = 100
  )

  region <- "NA"
  game_id <- "cs2"
  url_leaderboard <- sprintf("https://open.faceit.com/data/v4/rankings/games/%s/regions/%s", game_id, region)

  headers <- add_headers(
    Authorization = auth_key
  )

  # 从排行榜请求100名玩家的数据
  response <- GET(url_leaderboard,
                  headers = headers,
                  query = parameters)

  # 条件判断以捕获API请求错误
  if (status_code(response) == 404) {
    cat("Error 404: Resource not found.\n")
    cat(sprintf("Requested URL: %s\n", response$url))
    return(NULL)
  } else if (status_code(response) == 200) {
    data <- content(response, "parsed")

    # 提取玩家ID列表
    player_ids <- sapply(data$items, function(item) item$player_id)
    return(player_ids)
  } else {
    cat(sprintf("Error %d: %s\n", status_code(response), content(response, "text")))
    return(NULL)
  }
}

# 使用玩家ID从匹配历史中提取3个match_id
match_data_selection <- function(player_id, auth_key) {
  # API请求的参数
  parameters <- list(
    limit = 3
  )

  url_match_history <- sprintf("https://open.faceit.com/data/v4/players/%s/history", player_id)

  headers <- add_headers(
    Authorization = auth_key
  )

  # 从API请求玩家的比赛历史
  response <- GET(url_match_history,
                  headers = headers,
                  query = parameters)

  # 检查请求错误的条件
  if (status_code(response) == 200) {
    # 将JSON转换为数据框
    data <- content(response, "parsed")

    # 提取match_id
    match_ids <- sapply(data$items, function(item) item$match_id)
    return(match_ids)
  } else {
    cat(sprintf("Error %d: %s\n", status_code(response), content(response, "text")))
    return(NULL)
  }
}

# 使用match_ID计算每队的比赛统计数据框
calculate_match_stats <- function(match_id, auth_key, count) {
  # API请求的参数
  headers <- add_headers(
    Authorization = auth_key
  )

  url_match_id <- sprintf("https://open.faceit.com/data/v4/matches/%s/stats", match_id)

  # API请求比赛统计数据
  response <- GET(url_match_id, headers = headers)

  # 展平嵌套的JSON
  match_data <- content(response, "parsed")

  # 尝试展平嵌套的JSON
  tryCatch({
    if (length(match_data$rounds) == 0) {
      stats <- data.frame()
      return(stats)
    }

    # 创建比赛数据框的列
    columns <- c("win", "map", "Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                 "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo", "Match ID")

    # 创建比赛数据框
    data_df <- data.frame(matrix(ncol = length(columns), nrow = 1))
    colnames(data_df) <- columns

    # 检查比赛数据是否存在，否则返回空数据框
    tryCatch({
      # 获取回合数据
      round_data <- match_data$rounds[[1]]

      # 获取队伍数据
      teams_data <- round_data$teams

      if (teams_data[[1]]$team_stats$`Team Win` == "1") {
        data_df$win <- "team a"
      } else {
        data_df$win <- "team b"
      }

      data_df$map <- round_data$round_stats$Map
      data_df$`Match ID` <- match_id

      # 处理Team A统计数据
      team_a_players <- teams_data[[1]]$players
      team_a_nicknames <- sapply(team_a_players, function(player) player$nickname)

      team_a_id_elo_list <- list()

      # 循环遍历昵称列并添加A队玩家ID列表
      for (nickname in team_a_nicknames) {
        id_elo <- get_player_data(nickname, auth_key)

        # 如果玩家ELO不存在，则返回空数据框
        if (nrow(id_elo) == 0) {
          cat(count, "\n")
          return(id_elo)
        } else {
          team_a_id_elo_list[[length(team_a_id_elo_list) + 1]] <- id_elo
        }
      }

      # 从列表创建玩家ID列
      team_a_id_elo_df <- bind_rows(team_a_id_elo_list)

      # 将队伍ELO的平均值添加到比赛数据df
      data_df$Team_A_avg_elo <- mean(team_a_id_elo_df$faceit_elo)

      # 使用玩家ID计算单个玩家统计数据(K/R和胜率)
      team_a_stats_list <- list()

      for (i in 1:length(team_a_players)) {
        id <- team_a_players[[i]]$player_id

        # 使用get_player_stats计算单个统计数据
        stats_df <- get_player_stats(id, auth_key)
        stats <- player_stats_calc(stats_df, data_df$map)

        # 如果返回的df为空，则返回空比赛数据框
        if (nrow(stats) == 0) {
          # 用于跟踪缺失/无效数据
          cat(count, "\n")
          cat("Unable to compute stats, may be error with map used\n")
          return(stats)
        } else {
          # 将单个统计数据添加到列表
          team_a_stats_list[[length(team_a_stats_list) + 1]] <- stats
        }
      }

      # 为A队创建胜率和K/R比率列
      team_a_stats_df <- bind_rows(team_a_stats_list)
      data_df$Team_A_avg_win_percentage <- mean(team_a_stats_df$`Win Percentage`)
      data_df$Team_A_avg_KR <- mean(team_a_stats_df$`Average K/R Ratio`)

      # Team B统计数据，重复用于A队统计数据的相同过程
      team_b_players <- teams_data[[2]]$players
      team_b_nicknames <- sapply(team_b_players, function(player) player$nickname)
      team_b_id_elo_list <- list()

      for (nickname in team_b_nicknames) {
        id_elo <- get_player_data(nickname, auth_key)

        if (nrow(id_elo) == 0) {
          cat(count, "\n")
          return(id_elo)
        } else {
          team_b_id_elo_list[[length(team_b_id_elo_list) + 1]] <- id_elo
        }
      }

      team_b_id_elo_df <- bind_rows(team_b_id_elo_list)
      data_df$Team_B_avg_elo <- mean(team_b_id_elo_df$faceit_elo)

      team_b_stats_list <- list()

      for (i in 1:length(team_b_players)) {
        id <- team_b_players[[i]]$player_id

        stats_df <- get_player_stats(id, auth_key)
        stats <- player_stats_calc(stats_df, data_df$map)

        if (nrow(stats) == 0) {
          cat(count, "\n")
          cat("Unable to compute stats, may be error with map used\n")
          return(stats)
        } else {
          team_b_stats_list[[length(team_b_stats_list) + 1]] <- stats
        }
      }

      team_b_stats_df <- bind_rows(team_b_stats_list)
      data_df$Team_B_avg_win_percentage <- mean(team_b_stats_df$`Win Percentage`)
      data_df$Team_B_avg_KR <- mean(team_b_stats_df$`Average K/R Ratio`)

      # 返回给定match_id的比赛数据行
      return(data_df)

    }, error = function(e) {
      cat("Error processing match data:", e$message, "\n")
      stats <- data.frame()
      return(stats)
    })

  }, error = function(e) {
    cat("KeyError: 'rounds' may not exist\n")
    stats <- data.frame()
    return(stats)
  })
}
