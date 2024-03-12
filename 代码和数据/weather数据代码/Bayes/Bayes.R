setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\weather数据代码\\Bayes")

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

######################### 贝叶斯分类 ##########################
library(e1071)
set.seed(1234)
start_time_bayes <- Sys.time()
naive_bayes_model <- naiveBayes(Weather ~ ., data = train_data)
end_time_bayes <- Sys.time()
training_time_bayes <- end_time_bayes - start_time_bayes
cat("贝叶斯分类的的训练时长：", as.numeric(training_time_bayes), "s\n") #0.005825996 s

pred_bayes <- predict(naive_bayes_model, newdata = test_data)

######################### 模型评估 ##########################
#### 宏指标
confusion_matrix <- as.matrix(table(pred_bayes, test_data$Weather))
# 计算准确度
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix))  
#准确度0.7209302
# 计算每个类别的性能指标
class_recall <- diag(confusion_matrix) / colSums(confusion_matrix)
class_precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
class_f1_score <- 2 * (class_precision * class_recall) / (class_precision + class_recall)
# 计算每个类别的权重（这里使用类别的相对比例作为权重）
(class_weights <- colSums(confusion_matrix) / sum(confusion_matrix))
(macro_recall <- mean(class_recall, na.rm = TRUE)) # 宏平均召回率0.6991558
(macro_precision <- mean(class_precision, na.rm = TRUE)) # 宏平均精确率0.6798667
# 计算每个类别的F1分数
class_f1_score <- 2 * (class_precision * class_recall) / (class_precision + class_recall)
(macro_average_f1_score <- mean(class_f1_score, na.rm = TRUE)) # 宏平均F1分数0.6850926

######################### 贝叶斯的过拟合程度 #########################
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
  
  # 训练贝叶斯分类模型
  bayes_model <- naiveBayes(Weather ~ ., data = train_data)
  
  # 预测训练集和验证集
  train_predictions <- predict(bayes_model, newdata = train_subset)
  validation_predictions <- predict(bayes_model, newdata = validation_subset)
  
  # 计算训练集和验证集准确度
  train_accuracy <- sum(train_predictions == train_subset$Weather) / length(train_predictions)
  validation_accuracy <- sum(validation_predictions == validation_subset$Weather) / length(validation_predictions)
  
  train_accuracies[i] <- train_accuracy
  validation_accuracies[i] <- validation_accuracy
}

train_accuracies
# [1] 0.7727273 0.7738095 0.7120000 0.7530120 0.7874396 0.7854251 0.7551724
# [8] 0.7629179 0.7628032
validation_accuracies
# [1] 0.7650273 0.7638037 0.7894737 0.7745902 0.7438424 0.7361963 0.7916667
# [8] 0.7777778 0.7948718


