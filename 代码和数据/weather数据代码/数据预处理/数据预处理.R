setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\weather数据代码\\数据预处理")

weather <- read.csv("01_weather.csv")

library(openxlsx)
######################### 缺失值 #########################
# 查看是否存在缺失值
( miss_weather <- sapply(weather, function(x) sum(is.na(x) | x=="")) )
# 输出结果显示：不存在缺失值
# 查看数据的摘要信息
summary(weather)

######################### 无关列 #########################
# 剔除Date/Time列
clean_weather <- subset(weather, select = -c(Date.Time))

######################### 异常值 #########################
### 检测是否存在异常值 
class_labels <- c("Temp_C", "Dew_Point_Temp_C", "Rel_Hum", "Wind_Speed_km_h", "Visibility_km", "Press_kPa", "Weather")
# 绘制箱线图
par(mfrow = c(2, 3), mar = c(5, 5, 3, 3))
# 循环绘制每个数值列的箱线图
for (i in 1:ncol(clean_weather[,-7])) {
  boxplot(clean_weather[[i]], 
          col = "lightblue",
          ylab = "", 
          xlab = class_labels[i])
}

# 选择第二列到第七列进行异常值检测
numeric_columns <- clean_weather[, 1:6]
# 定义函数删除异常值并保持行对应
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers <- x < lower_bound | x > upper_bound
  return(list(values = x[!outliers], outliers = outliers))
}
# 对每列进行异常值检测和删除
outlier_indices <- list()
for (col in numeric_columns) {
  result <- remove_outliers(col)
  clean_weather <- clean_weather[!result$outliers, ]
  outlier_indices <- append(outlier_indices, which(result$outliers))
}

# 删除包含异常值的行
clean_weather <- clean_weather[complete.cases(clean_weather), ]

######################### weather列类别 #########################
# 统计异常值处理后中Weather列的类别数量：37个
(unique_categories_before <- unique(clean_weather$Weather))
# 统计Weather列的各类别数量
(weather_counts <- table(clean_weather$Weather))
write.xlsx(weather_counts, "异常值处理后weather列的类别数量.xlsx")

# 找到类别数量小于等于50的类别
(remove_categories_less50 <- names(weather_counts[weather_counts <= 50]))
# 剔除类别数小于等于500的行
clean_weather <- clean_weather[!(clean_weather$Weather %in% remove_categories_less50), ]
(weather_counts <- table(clean_weather$Weather))
# 找到类别数量小于等于500的类别
(categories_less500 <- names(weather_counts[weather_counts <= 500]))
# 保存数量在50到500之间的类别
little_clean_weather <- clean_weather[(clean_weather$Weather %in% categories_less500), ]
(weather_counts_little <- table(little_clean_weather$Weather))

# 把【Weather】设定为标签列，转换成因子
little_clean_weather$Weather <- as.factor(little_clean_weather$Weather)

head(little_clean_weather,10)
summary(little_clean_weather)

######################### 相关系数 #########################
library(ggplot2)
library(reshape2)

corr_matrix_little <- cor(little_clean_weather[,-ncol(little_clean_weather)])
cor_melted <- melt(corr_matrix_little)

ggplot(data = cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = " ", x = " ", y = " ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

little_clean_weather <- little_clean_weather[,-2]

numeric_cols <- little_clean_weather[, c("Temp_C", "Rel.Hum_.", "Wind.Speed_km.h", "Visibility_km", "Press_kPa")]
# 使用scale函数进行标准化
scaled_data <- as.data.frame(scale(numeric_cols))
scaled_data['Weather'] <- little_clean_weather['Weather']

#保存处理后的数据为CSV文件
write.csv(scaled_data, file = "scaled_data.csv", row.names = FALSE)

