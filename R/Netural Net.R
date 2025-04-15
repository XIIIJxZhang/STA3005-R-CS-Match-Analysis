# 加载必要库
library(torch)
library(dplyr)
library(caret)
library(fastDummies)
library(coro)
library(ggplot2)
library(gridExtra)

# 设置随机种子
set.seed(42)
torch_manual_seed(42)

# 数据准备 --------------------------------------------------------------
data <- read.csv("win_prediction_data_clean.csv")

# 创建虚拟变量
dummy_map <- dummy_cols(data, select_columns = "map", remove_selected_columns = TRUE)

# 分离特征和标签
features <- dummy_map %>%
  select(-win, -Match.ID) %>%
  mutate_all(as.numeric)

response <- dummy_map$win %>%
  factor() %>%
  as.numeric() - 1  # 转换为0/1

# 数据分割
train_idx <- createDataPartition(response, p = 0.8, list = FALSE)
X_train <- features[train_idx, ]
X_test <- features[-train_idx, ]
y_train <- response[train_idx]
y_test <- response[-train_idx]

# 张量转换 --------------------------------------------------------------
device <- if (cuda_is_available()) "cuda" else "cpu"

# 转换特征张量
X_train_tensor <- torch_tensor(
  as.matrix(X_train),
  dtype = torch_float32(),
  device = device
)

X_test_tensor <- torch_tensor(
  as.matrix(X_test),
  dtype = torch_float32(),
  device = device
)

# 转换标签张量 - 与Python代码保持一致，不使用view操作
y_train_tensor <- torch_tensor(y_train, dtype = torch_float32(), device = device)
y_test_tensor <- torch_tensor(y_test, dtype = torch_float32(), device = device)

# 创建数据加载器 --------------------------------------------------------
train_dataset <- tensor_dataset(X_train_tensor, y_train_tensor)
train_loader <- dataloader(train_dataset, batch_size = 32, shuffle = TRUE)

test_dataset <- tensor_dataset(X_test_tensor, y_test_tensor)
val_loader <- dataloader(test_dataset, batch_size = 32, shuffle = FALSE)

# 置信区间计算函数 ------------------------------------------------------
confidence_interval <- function(accuracy, n = length(y_test), confidence = 0.95) {
  p_hat <- accuracy / 100
  z <- qnorm(1 - (1 - confidence) / 2)
  margin <- z * sqrt((p_hat * (1 - p_hat)) / n)
  lower <- p_hat - margin
  upper <- p_hat + margin
  cat(sprintf("Confidence Interval: %.2f | %.2f | %.2f\n",
              lower * 100, accuracy, upper * 100))
}

# 模型定义
model_def <- nn_module(
  "BinaryClassifier",
  initialize = function(input_dim) {
    self$seq <- nn_sequential(
      nn_linear(input_dim, 16),
      nn_batch_norm1d(16),
      nn_relu(),
      nn_dropout(0.3),
      nn_linear(16, 1)
    )
  },
  forward = function(x) {
    x <- self$seq(x)
    # 添加squeeze操作，与Python代码保持一致，输出形状为 [batch_size]
    x$squeeze(2)
  }
)

# 训练函数 --------------------------------------------------------------
train_model <- function() {
  # 初始化模型
  model <- model_def$new(input_dim = ncol(X_train))$to(device = device)
  criterion <- nn_bce_with_logits_loss()
  optimizer <- optim_adam(model$parameters, lr = 0.0001, weight_decay = 1e-4)

  # 跟踪指标
  train_losses <- numeric()
  val_accuracies <- numeric()
  best_acc <- 0
  patience <- 0
  best_epoch <- 0
  best_weights <- NULL  # 初始化best_weights

  # 训练循环
  coro::loop(for (epoch in 1:100) {
    model$train()
    epoch_loss <- 0
    batch_count <- 0

    # 训练批次
    coro::loop(for (batch in train_loader) {
      x <- batch[[1]]$to(device = device)
      y <- batch[[2]]$to(device = device)

      optimizer$zero_grad()
      output <- model(x)

      # 现在output和y都是形状 [batch_size]，与Python版本一致
      loss <- criterion(output, y)

      loss$backward()
      optimizer$step()
      epoch_loss <- epoch_loss + loss$item()
      batch_count <- batch_count + 1
    })

    # 验证阶段
    model$eval()
    correct <- 0
    total <- 0

    # 使用with_no_grad()避免计算梯度
    with_no_grad({
      coro::loop(for (batch in val_loader) {
        x <- batch[[1]]$to(device = device)
        y <- batch[[2]]$to(device = device)

        output <- model(x)
        # 使用0.5而不是0作为阈值，R中索引从1开始
        pred <- (output > 0.5)$to(dtype = torch_float32())
        correct <- correct + (pred == y)$sum()$item()
        total <- total + y$size(1)  # 在R中使用1而不是0
      })
    })

    # 计算指标
    # 修改这里：将loss除以样本总数，而不是批次数
    avg_loss <- epoch_loss / length(train_loader$dataset)  # 样本总数
    acc <- correct / total * 100

    # 记录指标
    train_losses <- c(train_losses, avg_loss)
    val_accuracies <- c(val_accuracies, acc)

    # 早停机制
    if (acc > best_acc) {
      best_acc <- acc
      patience <- 0
      best_epoch <- epoch
      best_weights <- model$state_dict()
    } else {
      patience <- patience + 1
      if (patience >= 10) break
    }

    # 打印进度
    if (epoch %% 5 == 0) {
      cat(sprintf("Epoch [%03d] | Loss: %.4f | Acc: %.2f%%\n",
                  epoch, avg_loss, acc))
    }
  })

  # 确保best_weights存在
  if (!is.null(best_weights)) {
    # 加载最佳权重
    model$load_state_dict(best_weights)
  } else {
    cat("Warning: No best weights found. Using final model state.\n")
  }

  # 最终报告
  cat(sprintf("\nBest Accuracy: %.2f%% at Epoch [%d]\n", best_acc, best_epoch))

  # 计算置信区间
  confidence_interval(best_acc)

  # 可视化
  plot_data <- data.frame(
    Epoch = seq_along(train_losses),
    Loss = train_losses,
    Accuracy = val_accuracies
  )

  p1 <- ggplot(plot_data, aes(Epoch, Loss)) +
    geom_line(color = "steelblue") +
    labs(title = "Training Loss", y = "Loss")

  p2 <- ggplot(plot_data, aes(Epoch, Accuracy)) +
    geom_line(color = "forestgreen") +
    labs(title = "Validation Accuracy", y = "Accuracy (%)")

  grid.arrange(p1, p2, ncol = 2)

  return(model)
}

# 执行训练 --------------------------------------------------------------
model <- train_model()
