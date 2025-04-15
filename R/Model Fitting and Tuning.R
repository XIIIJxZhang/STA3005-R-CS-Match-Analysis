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

# 7.1 训练支持向量机模型
svm_model <- svm(features, response, kernel = "linear", cost = 1)

# 7.2 预测并评估准确度
svm_pred <- predict(svm_model, X_test)
svm_accuracy <- mean(svm_pred == y_test)
print(svm_accuracy)

# ============================
# 8. XGBoost模型
# ============================

# 8.1 转换数据格式为DMatrix，XGBoost要求标签为0/1整数
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(y_train) - 1)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = as.numeric(y_test) - 1)

# 8.2 设置参数
params <- list(objective = "binary:logistic", max_depth = 3)

# 8.3 训练XGBoost模型
xgb_model <- xgb.train(params, dtrain, nrounds = 25)

# 8.4 预测并评估准确度
xgb_pred <- predict(xgb_model, dtest)

# 确保 y_test 是数值型（0/1），然后计算准确度
y_test_numeric <- as.numeric(y_test) - 1  # 将y_test从因子转换为0/1

# 计算准确度
xgb_accuracy <- mean((xgb_pred > 0.5) == y_test_numeric)
print(xgb_accuracy)
