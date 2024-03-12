setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\weather数据代码\\bagging")

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

######################### bagging #########################
library(rpart)
library(rpart.plot)
Ctl <- rpart.control(minsplit = 5,  #预剪枝
                     maxcompete = 4,
                     maxdepth = 30,  #预剪枝
                     cp = 0.01,  # 后剪枝
                     xval = 10)  # 交叉验证

#### 【bagging袋装技术】
install.packages("adabag")
library("adabag")
set.seed(1234)
start_time_bag_2 <- Sys.time()
bag_tree_2 <- bagging(Weather ~ .,
                      data = train_data,
                      control = Ctl,
                      mfinal = 25)
end_time_bag_2 <- Sys.time()
training_time_bag_2 <- end_time_bag_2 - start_time_bag_2
cat("bagging组合分类树的训练时长(2)：", as.numeric(training_time_bag_2), "s") # 0.8199708 s
bag_tree_2$importance #输出特征的重要性
pred_tree_bag_2 <- predict.bagging(bag_tree_2, test_data)
pred_tree_bag_2$confusion
pred_tree_bag_2$error 
# 测试集上的错误率0.2325581

######################### bagging模型的评估 #########################

#### 宏指标
confusion_matrix <- as.matrix(table(pred_tree_bag_2$class, test_data$Weather))
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix))  
# 宏准确度0.7674419
# 计算每个类别的性能指标
class_recall <- diag(confusion_matrix) / colSums(confusion_matrix)
class_precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
class_f1_score <- 2 * (class_precision * class_recall) / (class_precision + class_recall)
# 计算每个类别的权重（这里使用类别的相对比例作为权重）
(class_weights <- colSums(confusion_matrix) / sum(confusion_matrix))
(macro_recall <- mean(class_recall, na.rm = TRUE)) # 宏平均召回率0.7461123
(macro_precision <- mean(class_precision, na.rm = TRUE)) # 宏平均精确率0.7423518
# 计算每个类别的F1分数
class_f1_score <- 2 * (class_precision * class_recall) / (class_precision + class_recall)
(macro_average_f1_score <- mean(class_f1_score, na.rm = TRUE)) # 宏平均F1分数0.742595

######################### bagging的过拟合程度 #########################
library(caret)
library(adabag)

# 设置随机种子
set.seed(1234)

# 创建不同大小的训练子集
train_sizes <- seq(0.1, 0.9, by = 0.1)  # 10% 到 90% 的训练数据比例
train_accuracies <- numeric(length(train_sizes))
valid_accuracies <- numeric(length(train_sizes))

# 迭代不同的训练数据比例
for (i in 1:length(train_sizes)) {
  # 创建训练子集
  index <- createDataPartition(train_data$Weather, p = train_sizes[i], list = FALSE)
  train_subset <- train_data[index, ]
  valid_subset <- train_data[-index, ]
  
  # 训练 Bagging 模型
  bag_model <- bagging(Weather ~ ., 
                       data = train_subset, 
                       control = Ctl, 
                       mfinal = 25)
  
  # 预测训练集和验证集
  train_preds <- predict(bag_model, newdata = train_subset)
  valid_preds <- predict(bag_model, newdata = valid_subset)
  
  train_accuracy <- sum(train_preds == train_subset$Weather) / length(train_preds)
  valid_accuracy <- sum(valid_preds == valid_subset$Weather) / length(valid_preds)
  
  train_accuracies[i] <- train_accuracy
  valid_accuracies[i] <- valid_accuracy
}



