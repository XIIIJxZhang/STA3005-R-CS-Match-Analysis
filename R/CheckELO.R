# Faceit玩家数据采集工具
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# 获取玩家基础信息
get_player_data <- function(nickname, auth_key) {
  url_player_id <- "https://open.faceit.com/data/v4/players"
  response <- GET(
    url_player_id,
    add_headers(Authorization = auth_key),
    query = list(nickname = nickname)
  )

  if (status_code(response) == 404) {
    cat("Error 404: Resource not found. Requested URL:", response$url, "\n")
    return(data.frame())
  } else if (status_code(response) == 200) {
    player_data <- content(response, "parsed")
    if (!is.null(player_data$player_id) &&
        !is.null(player_data$games$cs2$faceit_elo)) {
      data.frame(
        player_id = player_data$player_id,
        faceit_elo = player_data$games$cs2$faceit_elo
      )
    } else {
      cat("Error: Missing match data for the player.\n")
      data.frame()
    }
  } else {
    cat("Error", status_code(response), ":", content(response, "text"), "\n")
    data.frame()
  }
}

# 获取比赛统计数据
get_player_stats <- function(player_id, auth_key) {
  url_stats <- paste0(
    "https://open.faceit.com/data/v4/players/",
    player_id,
    "/games/cs2/stats"
  )
  response <- GET(
    url_stats,
    add_headers(Authorization = auth_key),
    query = list(player_id = player_id, limit = 10)
  )

  if (status_code(response) != 200) {
    cat("获取比赛数据失败，状态码：", status_code(response), "\n")
    return(data.frame())
  }

  tryCatch({
    data <- content(response, "parsed")
    if (!is.null(data$items)) {
      map_df(data$items, flatten) %>% as.data.frame()
    } else {
      data.frame()
    }
  }, error = function(e) {
    cat("解析数据时出错：", conditionMessage(e), "\n")
    data.frame()
  })
}

# 主程序
main_check_elo <- function() {
  auth_key <- readline("请输入Faceit API密钥: ")
  nickname <- trimws(readline("请输入玩家昵称: "))

  player_data <- get_player_data(nickname, auth_key)
  if (nrow(player_data) == 0) {
    cat("获取玩家数据失败\n")
  } else {
    cat("\n玩家信息:\n")
    print(player_data)

    player_id <- player_data$player_id[1]
    player_stats <- get_player_stats(player_id, auth_key)

    if (nrow(player_stats) > 0) {
      safe_nickname <- gsub("[^A-Za-z0-9]", "_", nickname)
      filename <- paste0(safe_nickname, ".csv")
      write.csv(player_stats, filename, row.names = FALSE, fileEncoding = "UTF-8")
      cat("\n成功保存", nrow(player_stats), "场比赛数据到", filename, "\n")
    } else {
      cat("\n获取比赛数据失败\n")
    }
  }
}

# 执行主程序
main_check_elo()
