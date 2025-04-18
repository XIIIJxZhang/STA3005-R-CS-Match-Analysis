# 导入所需的R包
library(dplyr)
library(readxl)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(ggplot2)
library(stringr)

# ============================
# 1. 导入所需的库
# ============================

# 这里导入的包包括数据处理、模型训练、评估等所需要的库
# dplyr：用于数据处理
# readxl：读取Excel文件
# caret：用于模型训练和交叉验证
# randomForest：随机森林算法
# e1071：支持向量机（SVM）
# xgboost：XGBoost算法
# ggplot2：用于可视化（如果需要）

# ============================
# 2. 读取数据
# ============================

# 2.1 读取 `data_1` 数据
data_1 <- read_excel("data_win_prediction_1.xlsx")
head(data_1)
colnames(data_1) <- c("win", "map", "Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                      "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo", "Match ID")

# 2.2 读取 `data_2`、`data_3`、`data_4`、`data_5`、`data_6` 数据
# 读取 data_2 数据
data_2 <- read_excel("data_win_prediction_2.xlsx")
colnames(data_2) <- c("win", "map", "Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                      "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo", "Match ID")
col_name <- colnames(data_2)[1]
data_2 <- data_2[data_2[[1]] != col_name, ]
head(data_2)
# 确保 data_2 中的列与 data_1 完全一致
data_2 <- data_2 %>%
  mutate(across(c("win", "map", "Match ID"), as.character)) %>%
  mutate(across(c("Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                  "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo"), as.numeric))

data_3 <- read_excel("data_win_prediction_3.xlsx")
head(data_3)
colnames(data_3) <- c("win", "map", "Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                      "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo", "Match ID")

data_4 <- read_excel("data_win_prediction_4.xlsx")
head(data_4)
colnames(data_4) <- c("win", "map", "Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                      "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo", "Match ID")

data_5 <- read_excel("data_win_prediction_5.xlsx")
head(data_5)
colnames(data_5) <- c("win", "map", "Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                      "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo", "Match ID")

data_6 <- read_excel("data_win_prediction_6.xlsx")
head(data_6)
colnames(data_6) <- c("win", "map", "Team_A_avg_win_percentage", "Team_A_avg_KR", "Team_A_avg_elo",
                      "Team_B_avg_win_percentage", "Team_B_avg_KR", "Team_B_avg_elo", "Match ID")

# ============================
# 3. 数据清理
# ============================

# 3.1 合并所有数据集
data_combined <- bind_rows(data_1, data_2, data_3, data_4, data_5, data_6)

# 3.2 过滤不以“de”开头的地图
data_combined <- data_combined %>% filter(str_starts(map, "de"))

# 3.3 过滤出至少50场比赛的地图
filtered_maps <- data_combined %>% count(map) %>% filter(n >= 50) %>% pull(map)
data_combined <- data_combined %>% filter(map %in% filtered_maps)

# 3.4 删除低Elo评分的比赛
data_combined <- data_combined %>% filter(Team_A_avg_elo > 800 & Team_B_avg_elo > 800)

# 3.5 检查缺失数据
sum(is.na(data_combined))

# 3.6 删除缺失数据
data_combined <- na.omit(data_combined)

# 3.7 删除重复数据
data_combined <- distinct(data_combined)

# 3.8 保存清理后的数据
write.csv(data_combined, "win_prediction_data_clean.csv", row.names = FALSE)

# ============================
# 4. 特征工程
# ============================

# 4.1 创建虚拟变量（正确方式）
data_encoded <- data_combined %>%
  select(-`Match ID`) %>%
  mutate(win = factor(win))  # win强制转换为factor（必须！）

# 用dummyVars正确处理虚拟变量
dummy <- dummyVars(win ~ ., data = data_encoded)
features <- predict(dummy, newdata = data_encoded) %>% as.data.frame()

# 4.2 定义response（唯一正确的定义）
response <- data_encoded$win  # 必须是factor

# ============================
# 5. 逻辑回归模型
# ============================

# 5.1 交叉验证
train_control <- trainControl(method = "cv", number = 10)

# 5.2 训练逻辑回归模型
lr_model <- train(features, response, method = "glm", trControl = train_control, family = "binomial")

# 5.3 输出模型的准确度
lr_accuracy <- lr_model$results$Accuracy
print(lr_accuracy)

# 5.4 计算准确度的置信区间
ci <- (1.96 * sqrt((lr_accuracy * (1 - lr_accuracy)) / 8104)) * 100
print(paste("Confidence Interval:", round(ci, 2)))

# ============================
# 6. 随机森林模型（正确版）
# ============================

# 6.1 数据拆分
set.seed(50)
train_index <- createDataPartition(response, p = 0.7, list = FALSE)

X_train <- features[train_index, ]
X_test <- features[-train_index, ]
y_train <- response[train_index]  # response此时已确定为factor
y_test <- response[-train_index]

# 强制检查y_train是否是因子
y_train <- factor(y_train)
stopifnot(is.factor(y_train))

# 检查维度一致性
stopifnot(nrow(X_train) == length(y_train))

# 检查y_train的类别数
print(length(unique(y_train)))

# 6.2 训练随机森林模型
rf_model <- randomForest(
  x = X_train,
  y = y_train,  # 已确认是factor
  ntree = 500,
  importance = TRUE
)

# 输出模型信息
print(rf_model)

# 6.3 预测并评估准确度
rf_pred <- predict(rf_model, X_test)
rf_accuracy <- mean(rf_pred == y_test)  # 计算预测准确度
print(paste("Accuracy:", round(rf_accuracy, 4)))

# 6.4 随机森林的超参数调优
param_grid <- expand.grid(mtry = c(2, 3, 4))

rf_tune <- train(
  x = X_train,
  y = y_train,
  method = "rf",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = param_grid,
  ntree = 200
)
print(rf_tune$bestTune)

# ============================
# 7. 支持向量机 (SVM)
# ============================

# 7.1 训练基本SVM模型
svm_model <- svm(x = X_train, y = y_train, kernel = "linear", cost = 1)

# 7.2 预测并评估准确度
svm_pred <- predict(svm_model, X_test)
svm_accuracy <- mean(svm_pred == y_test)
print(paste("SVM Base Accuracy:", round(svm_accuracy, 4)))

# 7.3 SVM超参数调优 - 修复参数网格
# 定义正确的参数网格，与svmRadial方法兼容
param_grid_svm <- expand.grid(
  sigma = c(0.1, 0.01, 1),  # 对应Python中的gamma
  C = c(0.1, 1, 5, 10)      # 对应Python中的C
)

# 执行网格搜索
set.seed(50)
svm_tune <- train(
  x = X_train,
  y = y_train,
  method = "svmRadial",  # 使用径向基核函数(对应Python中的'rbf')
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = param_grid_svm,
  metric = "Accuracy"
)

# 输出最佳参数
print("SVM Best Parameters:")
print(svm_tune$bestTune)

# 7.4 使用最佳参数重新拟合SVM模型
best_svm_model <- svm(
  x = X_train,
  y = y_train,
  kernel = "radial",  # 对应Python中的'rbf'
  cost = svm_tune$bestTune$C,
  gamma = svm_tune$bestTune$sigma
)

# 预测并评估准确度
best_svm_pred <- predict(best_svm_model, X_test)
best_svm_accuracy <- mean(best_svm_pred == y_test)
print(paste("SVM Best Model Accuracy:", round(best_svm_accuracy, 4)))

# 7.5 计算SVM模型准确率的置信区间
svm_ci <- (1.96 * sqrt((best_svm_accuracy * (1 - best_svm_accuracy)) / length(y_test))) * 100
print(paste("SVM Confidence Interval:", round(svm_ci, 2)))

# ============================
# 8. XGBoost模型
# ============================

# 8.1 转换数据格式，XGBoost要求标签为0/1整数
# 确保标签编码为0/1
# 将因子标签转换为0/1数值
response_encoded <- as.numeric(response) - 1
table(response_encoded)  # 检查编码是否正确

# 分割数据
set.seed(50)
xgb_train_index <- createDataPartition(response_encoded, p = 0.7, list = FALSE)
xgb_X_train <- as.matrix(features[xgb_train_index, ])
xgb_X_test <- as.matrix(features[-xgb_train_index, ])
xgb_y_train <- response_encoded[xgb_train_index]
xgb_y_test <- response_encoded[-xgb_train_index]

# 创建DMatrix对象
dtrain <- xgb.DMatrix(data = xgb_X_train, label = xgb_y_train)
dtest <- xgb.DMatrix(data = xgb_X_test, label = xgb_y_test)

# 8.2 设置基本参数
params <- list(
  objective = "binary:logistic",
  max_depth = 3,
  eta = 0.1,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1
)

# 8.3 训练基本XGBoost模型
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 10,
  watchlist = list(train = dtrain),
  verbose = 0
)

# 预测并评估基本模型
xgb_pred <- predict(xgb_model, dtest)
xgb_pred_binary <- ifelse(xgb_pred > 0.5, 1, 0)
xgb_accuracy <- mean(xgb_pred_binary == xgb_y_test)
print(paste("XGBoost Baseline Accuracy:", round(xgb_accuracy, 4)))

# 8.4 绘制特征重要性
importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix, top_n = 10)

# 8.5 交叉验证找最佳轮数
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 40,
  nfold = 3,
  early_stopping_rounds = 10,
  metrics = "logloss",
  verbose = 0
)

# 输出最佳轮数
best_nrounds <- which.min(cv_results$evaluation_log$test_logloss_mean)
print(paste("Best number of rounds:", best_nrounds))

# 8.6 超参数调优 - 使用随机参数网格而不是完整网格
# 定义参数采样范围 - 类似Python的RandomizedSearchCV
set.seed(123)
param_combinations <- list(
  max_depth = sample(3:11, 4),
  eta = sample(c(0.001, 0.01, 0.1, 0.2, 0.3), 3),
  gamma = sample(c(0, 0.1, 0.5, 1), 2),
  subsample = sample(c(0.5, 0.75, 1), 2),
  colsample_bytree = sample(c(0.5, 0.75, 1), 2),
  min_child_weight = sample(c(1, 3, 5), 2),
  lambda = sample(c(0, 0.1, 1), 2),
  alpha = sample(c(0, 0.001, 0.1), 2)
)

# 初始化最佳参数和分数
best_accuracy <- 0
best_params <- NULL
best_nrounds <- 25  # 使用之前发现的最佳轮数

# 创建参数网格
param_grid <- expand.grid(
  max_depth = param_combinations$max_depth,
  eta = param_combinations$eta,
  gamma = param_combinations$gamma,
  subsample = param_combinations$subsample,
  colsample_bytree = param_combinations$colsample_bytree,
  min_child_weight = param_combinations$min_child_weight,
  lambda = param_combinations$lambda,
  alpha = param_combinations$alpha
)

# 随机抽样12个组合
sampled_indices <- sample(1:nrow(param_grid), min(12, nrow(param_grid)))
sampled_params <- param_grid[sampled_indices, ]

# 对每个采样的参数组合进行评估
for (i in 1:nrow(sampled_params)) {
  current_params <- list(
    objective = "binary:logistic",
    max_depth = sampled_params$max_depth[i],
    eta = sampled_params$eta[i],
    gamma = sampled_params$gamma[i],
    subsample = sampled_params$subsample[i],
    colsample_bytree = sampled_params$colsample_bytree[i],
    min_child_weight = sampled_params$min_child_weight[i],
    lambda = sampled_params$lambda[i],
    alpha = sampled_params$alpha[i]
  )

  # 使用交叉验证评估当前参数
  set.seed(456)
  cv_result <- xgb.cv(
    params = current_params,
    data = dtrain,
    nrounds = 25,
    nfold = 5,
    metrics = "error",
    verbose = 0
  )

  # 获取最后一轮的准确率
  current_accuracy <- 1 - cv_result$evaluation_log$test_error_mean[nrow(cv_result$evaluation_log)]

  # 更新最佳参数
  if (current_accuracy > best_accuracy) {
    best_accuracy <- current_accuracy
    best_params <- current_params
  }
}

# 输出最佳参数和准确率
print(paste("Best XGBoost CV Accuracy:", round(best_accuracy, 4)))
print("Best XGBoost Parameters:")
print(best_params)

# 8.7 使用最佳参数训练最终模型
final_xgb_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 25,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 0
)

# 预测并评估最终模型
final_xgb_pred <- predict(final_xgb_model, dtest)
final_xgb_pred_binary <- ifelse(final_xgb_pred > 0.5, 1, 0)
final_xgb_accuracy <- mean(final_xgb_pred_binary == xgb_y_test)
print(paste("Final XGBoost Accuracy:", round(final_xgb_accuracy, 4)))

# 8.8 模型变量重要性可视化
final_importance <- xgb.importance(model = final_xgb_model)
print(final_importance)
xgb.plot.importance(final_importance, top_n = 10)

# 8.9 输出最终XGBoost模型的所有准确率指标
conf_matrix <- table(Predicted = final_xgb_pred_binary, Actual = xgb_y_test)
print("Confusion Matrix:")
print(conf_matrix)

precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
f1_score <- 2 * precision * recall / (precision + recall)

print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("F1 Score:", round(f1_score, 4)))

# ============================
# 9. 模型比较
# ============================

# 9.1 汇总各模型准确率（确保变量名完全匹配）
model_accuracies <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "SVM", "XGBoost"),
  Accuracy = c(
    mean(lr_model$results$Accuracy),  # 逻辑回归使用10折交叉验证平均准确率
    rf_accuracy,                      # 随机森林测试集准确率（30% holdout）
    best_svm_accuracy,                # SVM调优后测试集准确率
    final_xgb_accuracy                # XGBoost调优后测试集准确率
  ),
  Type = c("Cross-Validated", "Holdout", "Holdout", "Holdout")  # 标识评估类型
)

# 9.2 可视化（添加置信区间和评估类型说明）
library(scales)  # 确保加载scales包用于百分比格式

ggplot(model_accuracies, aes(x = reorder(Model, -Accuracy),
                             y = Accuracy,
                             fill = Model,
                             alpha = Type)) +  # 用透明度区分评估类型
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = Accuracy - ci/100,  # 使用逻辑回归的置信区间
        ymax = Accuracy + ci/100),
    width = 0.2,
    position = position_dodge(0.8),
    color = "gray30"
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", Accuracy*100)),  # 显示百分比格式
    vjust = -1.2,
    size = 4,
    position = position_dodge(0.8)
  ) +
  scale_alpha_manual(
    values = c("Cross-Validated" = 0.9, "Holdout" = 0.6),  # 设置透明度
    guide = guide_legend(title = "Evaluation Type")
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Model Performance Comparison",
    subtitle = "With 95% Confidence Interval for Logistic Regression",
    x = "",
    y = "Accuracy"
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),  # 使用百分比格式
    limits = c(0.5, 0.8),  # 根据实际数据范围调整
    breaks = seq(0.5, 0.8, by = 0.05)
  ) +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#59A14F"))  # 定制颜色

# 9.3 输出关键统计信息
cat("\n=== 模型评估关键指标 ===\n")
cat(sprintf("逻辑回归置信区间: ±%.2f%%\n", ci))
cat(sprintf("随机森林测试样本量: %d\n", length(y_test)))
cat(sprintf("XGBoost F1分数: %.3f\n", f1_score))
