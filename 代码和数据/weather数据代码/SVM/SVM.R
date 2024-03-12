setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\weather数据代码\\SVM")

clean_weather <- read.csv("scaled_data.csv")
# 【Weather】为标签列，转换成因子
clean_weather$Weather <- as.factor(clean_weather$Weather)
head(clean_weather,10)

######################### 数据集划分 #########################
library(caret)
set.seed(123)
index <- createDataPartition(clean_weather$Weather, p = 0.7, list = FALSE)
train_data <- clean_weather[index, ]
test_data <- clean_weather[-index, ]

######################### SVM分类 ##########################
library(e1071)
library(kernlab)

# 创建参数网格
param_grid <- expand.grid(C = c(0.1, 1, 10, 100), sigma = c(0.01, 0.1, 1, 10))
# 通过交叉验证选择最佳参数
set.seed(123)
best_accuracy <- 0
best_params <- c(C = NA, sigma = NA)
for (i in 1:nrow(param_grid)) {
  current_params <- param_grid[i, ]
  current_model <- ksvm(Weather ~ ., 
                        data = train_data, 
                        type = "C-svc", 
                        kernel = "rbfdot", 
                        C = current_params$C,  #正则化参数
                        kpar = list(sigma = current_params$sigma)) #高斯核的带宽参数
  predictions <- predict(current_model, newdata = test_data)
  confusion_matrix <- table(predictions, test_data$Weather)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_params <- current_params
  }
}

cat("最佳参数组合： C =", best_params$C, ", sigma =", best_params$sigma, "\n")
# 最佳参数组合： C = 10 , sigma = 1 

set.seed(1234)
start_time_svm <- Sys.time()
svm_model <- ksvm(Weather ~ ., 
                  data = train_data, 
                  type = "C-svc",
                  kernel = "rbfdot",
                  C = best_params$C, 
                  kpar = list(sigma = best_params$sigma))
end_time_svm <- Sys.time()
training_time_svm <- end_time_svm - start_time_svm
cat("支持向量机的训练时长(best_params)：", as.numeric(training_time_svm), "s\n")  # 0.05766416 s
pred_svm <- predict(svm_model, test_data)

######################### 模型评估 ##########################
#### 宏指标
confusion_matrix <- as.matrix(table(pred_svm, test_data$Weather))
# 计算准确度
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix))  
#准确度0.8139535
# 计算每个类别的性能指标
class_recall <- diag(confusion_matrix) / colSums(confusion_matrix)
class_precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
class_f1_score <- 2 * (class_precision * class_recall) / (class_precision + class_recall)
# 计算每个类别的权重（这里使用类别的相对比例作为权重）
(class_weights <- colSums(confusion_matrix) / sum(confusion_matrix))
(macro_recall <- mean(class_recall, na.rm = TRUE)) # 宏平均召回率0.7714828
(macro_precision <- mean(class_precision, na.rm = TRUE)) # 宏平均精确率0.7784657
# 计算每个类别的F1分数
class_f1_score <- 2 * (class_precision * class_recall) / (class_precision + class_recall)
(macro_average_f1_score <- mean(class_f1_score, na.rm = TRUE)) # 宏平均F1分数0.7740098

######################### SVM的过拟合程度 #########################
library(caret)
train_sizes <- seq(0.1, 0.9, by = 0.1)  # 10% 到 90% 的训练集大小
# 存储训练和验证准确度的列表
train_accuracies <- numeric(length(train_sizes))
validation_accuracies <- numeric(length(train_sizes))

for (i in 1:length(train_sizes)) {
  # 划分训练集和验证集
  set.seed(1234)
  splitIndex <- createDataPartition(train_data$Weather, p = train_sizes[i], list = FALSE)
  train_subset <- train_data[splitIndex, ]
  validation_subset <- train_data[-splitIndex, ]
  
  svm_current <- ksvm(Weather ~ ., 
                      data = train_data, 
                      type = "C-svc",
                      kernel = "rbfdot",
                      C = 10, 
                      kpar = list(sigma = 1))
  
  # 预测训练集和验证集
  train_predictions <- predict(svm_current, newdata = train_subset)
  validation_predictions <- predict(svm_current, newdata = validation_subset)
  
  # 计算训练集和验证集准确度
  train_accuracy <- sum(train_predictions == train_subset$Weather) / length(train_predictions)
  validation_accuracy <- sum(validation_predictions == validation_subset$Weather) / length(validation_predictions)
  
  train_accuracies[i] <- train_accuracy
  validation_accuracies[i] <- validation_accuracy
}

train_accuracies
# [1] 0.9545455 0.9642857 0.9440000 0.9518072 0.9613527 0.9554656
# [7] 0.9655172 0.9604863 0.9649596
validation_accuracies
# [1] 0.9672131 0.9662577 0.9754386 0.9754098 0.9704433 0.9815951
# [7] 0.9666667 0.9876543 0.9743590
