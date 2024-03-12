setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\fifa_sales_loan数据代码")

sales <- read.csv("08_sales.csv")

# 查看是否存在缺失值
(missing_values_sales <- sapply(sales, function(x) sum(is.na(x) | x=="" )))
sales[sales == ""] <- NA
clean_sales <- na.omit(sales)
# 查看数据的摘要信息
summary(sales)

library(ggplot2)
non_empty_counts <- nrow(sales)-missing_values_sales
result <- data.frame(column_name = names(non_empty_counts), non_empty_count = non_empty_counts)
fifa_png <- ggplot(result, 
                   aes(x = column_name, y = non_empty_count)) + geom_bar(stat = "identity", fill = "pink") + labs( x = " ", y = "每列的个数") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fifa_png <- fifa_png + 
  geom_text(aes(label = non_empty_count), vjust = -0.5, color = "darkgray")
ggsave("sales的数据预览.png", plot = fifa_png, width = 6, height = 5)

## 数据处理的大纲：
# 剔除所有缺失值所在的行
# 剔除order_ID列
# 将Order_Date和Ship_Date这两列转换成数字
# 将含有字符型的列转换成数值
# 选定【Order_Priority】列作为标签列

clean_sales <- subset(clean_sales, select = -c(Order_ID)) #剔Order_ID列

# 说明：
# 在R中，as.numeric()函数将Date对象转换为日期时间戳
# 其中整数部分表示从1970年1月1日开始的天数，小数部分表示小时、分钟和秒。
# 这是因为1970年1月1日通常被用作Unix时间戳的起始点。

# Order_Date日期转换
clean_sales$Order_Date <- gsub("-", "/", clean_sales$Order_Date)
clean_sales$Order_Date <- as.Date(clean_sales$Order_Date, format = "%m/%d/%Y")
clean_sales$Order_Date <- as.numeric(clean_sales$Order_Date)
# Ship_Date日期转换
clean_sales$Ship_Date <- gsub("-", "/", clean_sales$Ship_Date)
clean_sales$Ship_Date <- as.Date(clean_sales$Ship_Date, format = "%m/%d/%Y")
clean_sales$Ship_Date <- as.numeric(clean_sales$Ship_Date)
# 字符转数值
Region_mapping <- unique(clean_sales$Region)
Country_mapping <- unique(clean_sales$Country)
Item_Type_mapping <- unique(clean_sales$Item_Type)
Sales_Channel_mapping <- unique(clean_sales$Sales_Channel)
clean_sales$Region <- as.integer(factor(clean_sales$Region, levels = Region_mapping))
clean_sales$Country <- as.integer(factor(clean_sales$Country, levels = Country_mapping))
clean_sales$Item_Type <- as.integer(factor(clean_sales$Item_Type, levels = Item_Type_mapping))
clean_sales$Sales_Channel <- as.integer(factor(clean_sales$Sales_Channel, levels = Sales_Channel_mapping))
# Order_Priority列转换成因子
clean_sales$Order_Priority <- as.factor(clean_sales$Order_Priority)

head(clean_sales,10)

library(randomForest)
library(caret)
library(pROC)

# 划分训练集和测试集 8:2
set.seed(1234)
train_index <- createDataPartition(clean_sales$Order_Priority, p = 0.8, list = FALSE)
train_sales <- clean_sales[train_index,]
test_sales <- clean_sales[-train_index,]

# 构建随机森林模型
set.seed(1234)
start_time_sales <- Sys.time()
(randForest_sales <- randomForest(train_sales$Order_Priority~., data = train_sales, importance = TRUE))
end_time_sales <- Sys.time()
training_time_sales <- end_time_sales - start_time_sales
cat("sales数据集的的训练时长：", as.numeric(training_time_sales), "s\n") #5.092771 s

pred_rand_sales <- predict(randForest_sales, test_sales)

## 评估模型的性能
(confusion_matrix_rand_sales <- confusionMatrix(pred_rand_sales, test_sales$Order_Priority))
(precision_rand_sales <- confusion_matrix_rand_sales$overall['Accuracy'])  # 准确率0.2462312
(kappa_rand_sales <- confusion_matrix_rand_sales$overall['Kappa'])  # Kappa系数-0.007763663 
# AUC
roc_rand_sales <- multiclass.roc(test_sales$Order_Priority, as.numeric(pred_rand_sales))
cat("AUC for each class:\n")
print(roc_rand_sales$auc)  # AUC的值0.494
# ROC曲线
roc_curves <- roc_rand_sales[['rocs']]
# 绘制每个类别的ROC曲线
plot(roc_curves[[1]], col = "red", main = "sales - ROC Curves")

# 添加其他类别的ROC曲线
for (i in 2:length(roc_curves)) {
  lines(roc_curves[[i]], col = i)
}

DrawL <- par()
plot(randForest_sales,  main = "随机森林的OOB判错率和决策树棵树（sales）", lwd = 3)

#### 交叉验证
ctrl <- trainControl(method = "cv", number = 5)  # 使用5折交叉验证

set.seed(1234)
cv_model <- train(
  x = train_sales[, -which(names(train_sales) == "Order_Priority")],  # 特征
  y = train_sales$Order_Priority,  # 目标
  method = "rf",  # 随机森林方法
  trControl = ctrl
)

cv_results <- cv_model$results
cat("Mean Accuracy: ", mean(cv_results$Accuracy), "\n") #0.2468765 
test_predictions <- predict(cv_model, newdata = test_sales)
test_accuracy <- mean(test_predictions == test_sales$Order_Priority)
cat("Test Set Accuracy: ", test_accuracy, "\n") #0.2512563 
# 交叉验证通过，模型不存在过拟合

