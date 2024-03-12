setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\fifa_sales_loan数据代码")

loan <- read.csv("10_loan.csv")

# 查看是否存在缺失值
( missing_values_loan <- sapply(loan, function(x) sum(is.na(x) | x=="")) )
# 查看数据的摘要信息
summary(loan)

library(ggplot2)
non_empty_counts <- nrow(loan)-missing_values_loan
result <- data.frame(column_name = names(non_empty_counts), non_empty_count = non_empty_counts)
fifa_png <- ggplot(result, 
                   aes(x = column_name, y = non_empty_count)) + geom_bar(stat = "identity", fill = "orange") + labs( x = " ", y = "每列的个数") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fifa_png <- fifa_png + 
  geom_text(aes(label = non_empty_count), vjust = -0.5, color = "darkgray")
ggsave("loan的数据预览.png", plot = fifa_png, width = 6, height = 5)

# 剔除含有缺失值的行
loan[loan == ""] <- NA
clean_loan <- na.omit(loan)
( missing_values_loan <- sapply(clean_loan, function(x) sum(is.na(x) | x=="")) )
# 剔除Loan_ID列
clean_loan <- subset(clean_loan, select = -c(Loan_ID))
# 字符型转数值型
Gender_mapping <- unique(clean_loan$Gender)
Married_mapping <- unique(clean_loan$Married)
Education_mapping <- unique(clean_loan$Education)
Self_Employed_mapping <- unique(clean_loan$Self_Employed)
Property_Area_mapping <- unique(clean_loan$Property_Area)
clean_loan$Gender <- as.integer(factor(clean_loan$Gender, levels = Gender_mapping))
clean_loan$Married <- as.integer(factor(clean_loan$Married, levels = Married_mapping))
clean_loan$Education <- as.integer(factor(clean_loan$Education, levels = Education_mapping))
clean_loan$Self_Employed <- as.integer(factor(clean_loan$Self_Employed, levels = Self_Employed_mapping))
clean_loan$Property_Area <- as.integer(factor(clean_loan$Property_Area, levels = Property_Area_mapping))
# Dependents单独处理“3+”
clean_loan$Dependents <- as.character(clean_loan$Dependents)
clean_loan$Dependents <- ifelse(clean_loan$Dependents == "3+", 3, clean_loan$Dependents)
clean_loan$Dependents <- as.numeric(clean_loan$Dependents)
# 标签列Loan_Status转换成因子
clean_loan$Loan_Status <- as.factor(clean_loan$Loan_Status)

head(clean_loan,10)

library(randomForest)
library(caret)
library(pROC)

# 划分训练集和测试集 8:2
set.seed(1234)
train_index <- createDataPartition(clean_loan$Loan_Status, p = 0.8, list = FALSE)
train_loan <- clean_loan[train_index,]
test_loan <- clean_loan[-train_index,]

# 构建随机森林模型
set.seed(1234)
start_time_loan <- Sys.time()
(randForest_loan <- randomForest(train_loan$Loan_Status~., data = train_loan, importance = TRUE))
end_time_loan <- Sys.time()
training_time_loan <- end_time_loan - start_time_loan
cat("loan数据集的的训练时长：", as.numeric(training_time_loan), "s\n") #0.242789 s

pred_rand_loan <- predict(randForest_loan, test_loan)

## 评估模型的性能
(confusion_matrix_rand_loan <- confusionMatrix(pred_rand_loan, test_loan$Loan_Status))
(precision_rand_loan <- confusion_matrix_rand_loan$overall['Accuracy'])  # 准确率0.7894737
(kappa_rand_loan <- confusion_matrix_rand_loan$overall['Kappa'])  # Kappa系数0.4385343
# AUC
roc_rand_loan <- multiclass.roc(test_loan$Loan_Status, as.numeric(pred_rand_loan))
cat("AUC for each class:\n")
print(roc_rand_loan$auc)  # AUC的值0.6938
# ROC曲线
roc_curves <- roc_rand_loan[['rocs']]
plot(roc_curves[[1]], col = "red"  ,main = "loan - ROC Curves")

DrawL <- par()
plot(randForest_loan,  main = "随机森林的OOB判错率和决策树棵树（loan）", lwd = 3)

#### 交叉验证
ctrl <- trainControl(method = "cv", number = 5)  # 使用5折交叉验证

set.seed(1234)
cv_model <- train(
  x = train_loan[, -which(names(train_loan) == "Loan_Status")],  # 特征
  y = train_loan$Loan_Status,  # 目标
  method = "rf",  # 随机森林方法
  trControl = ctrl
)

cv_results <- cv_model$results
cat("Mean Accuracy: ", mean(cv_results$Accuracy), "\n") #0.793104 
test_predictions <- predict(cv_model, newdata = test_loan)
test_accuracy <- mean(test_predictions == test_loan$Loan_Status)
cat("Test Set Accuracy: ", test_accuracy, "\n") #0.7578947 
# 交叉验证通过，模型不存在过拟合


