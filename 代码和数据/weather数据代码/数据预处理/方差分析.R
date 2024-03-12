setwd("D:\\OneDrive\\桌面\\R_code\\2023-2024（1）R语言结课论文要求\\代码和数据\\weather数据代码\\数据预处理")

little_weather <- read.csv("scaled_data.csv")

little_weather$Weather <- as.numeric(as.factor(little_weather$Weather))

little_aov <- aov(Weather ~., data = little_weather)
summary(little_aov)
#                    Df Sum Sq Mean Sq F value   Pr(>F)    
#   Temp_C            1  366.7   366.7  292.62  < 2e-16 ***
#   Rel.Hum_.         1   43.4    43.4   34.62 6.79e-09 ***
#   Wind.Speed_km.h   1   69.7    69.7   55.60 3.28e-13 ***
#   Visibility_km     1   15.0    15.0   12.01  0.00057 ***
#   Press_kPa         1    4.9     4.9    3.87  0.04962 *  
#   Residuals       576  721.9     1.3                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



