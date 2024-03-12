setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\fifa_sales_loan数据代码")

fifa <- read.csv("04_fifa.csv")

# 查看是否存在缺失值
( missing_values_fifa <- sapply(fifa, function(x) sum(is.na(x))) )
# 查看数据的摘要信息
summary(fifa)

library(ggplot2)
non_empty_counts <- sapply(fifa, function(x) sum(!is.na(x)))
result <- data.frame(column_name = names(non_empty_counts), non_empty_count = non_empty_counts)
fifa_png <- ggplot(result, 
       aes(x = column_name, y = non_empty_count)) + geom_bar(stat = "identity", fill = "skyblue") + labs( x = " ", y = "每列的个数") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
fifa_png <- fifa_png + 
  geom_text(aes(label = non_empty_count), vjust = -0.5, color = "darkgray")
ggsave("fifa的数据预览.png", plot = fifa_png, width = 6, height = 5)

## 数据处理的大纲：
# 由上面的结果可知，数据集不存在缺失值。
# 以【position】为标签列，通过查阅相关的背景知识,position代表足球比赛的位置标识
# GK表示守门员，DF表示后卫，MF表示中场，FW表示前锋

# 另外，birth_date与age表达的意思雷同，此处仅保留age列，剔除birth_date
# shirt_name和name都相当于是id号，对建模没有实际意义，剔除
# club和shirt_name的作用类似，剔除

clean_fifa <- subset(fifa, select = -c(birth_date,shirt_name,club,name))
head(clean_fifa,10)
# 将team和league列映射成数值
team_mapping <- unique(clean_fifa$team)
league_mapping <- unique(clean_fifa$league)
clean_fifa$team <- as.integer(factor(clean_fifa$team, levels = team_mapping))
clean_fifa$league <- as.integer(factor(clean_fifa$league, levels = league_mapping))
head(clean_fifa,10)
# 把position转换成因子
clean_fifa$position <- as.factor(clean_fifa$position)

install.packages("randomForest")
install.packages("ROCR")
library(randomForest)
library(caret)
library(pROC)

# 划分训练集和测试集 8:2
set.seed(1234)
train_index <- createDataPartition(clean_fifa$position, p = 0.8, list = FALSE)
train_fifa <- clean_fifa[train_index,]
test_fifa <- clean_fifa[-train_index,]

# 构建随机森林模型
set.seed(1234)
start_time_fifa <- Sys.time()
(ranForest_model <- randomForest(train_fifa$position~., data = train_fifa, importance = TRUE))
end_time_fifa <- Sys.time()
training_time_fifa <- end_time_fifa - start_time_fifa
cat("fifa数据集的的训练时长：", as.numeric(training_time_fifa), "s\n") #0.3617029 s

# 测试集上预测
pred_rand <- predict(ranForest_model, test_fifa)

## 评估模型的性能
(confusion_matrix_rand <- confusionMatrix(pred_rand, test_fifa$position))
(precision_rand <- confusion_matrix_rand$overall['Accuracy'])  # 准确率0.5586207 
(kappa_rand <- confusion_matrix_rand$overall['Kappa'])  # Kappa系数0.377223
# AUC
roc_rand <- multiclass.roc(test_fifa$position, as.numeric(pred_rand))
cat("AUC for each class:\n")
print(roc_rand$auc)  # AUC的值0.5929
# ROC曲线
roc_curves <- roc_rand[['rocs']]
# 绘制每个类别的ROC曲线
plot(roc_curves[[1]], col = "red", main = "fifa - ROC Curves")

# 添加其他类别的ROC曲线
for (i in 2:length(roc_curves)) {
  lines(roc_curves[[i]], col = i)
}

DrawL <- par()
plot(ranForest_model,  main = "随机森林的OOB判错率和决策树棵树（fifa）", lwd = 3)

#### 交叉验证
ctrl <- trainControl(method = "cv", number = 5)  # 使用5折交叉验证

set.seed(1234)
cv_model <- train(
  x = train_fifa[, -which(names(train_fifa) == "position")],  # 特征
  y = train_fifa$position,  # 目标
  method = "rf",  # 随机森林方法
  trControl = ctrl
)

cv_results <- cv_model$results
cat("Mean Accuracy: ", mean(cv_results$Accuracy), "\n") #0.5431196 
test_predictions <- predict(cv_model, newdata = test_fifa)
test_accuracy <- mean(test_predictions == test_fifa$position)
cat("Test Set Accuracy: ", test_accuracy, "\n") #0.5793103 
# 交叉验证通过，模型不存在过拟合
