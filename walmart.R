library(tidyverse)
library(lubridate)

library(modelr)

stores <- read_csv("C:/Users/taoya/Desktop/MSCS6510/Final project/stores.csv")
trains <- read_csv("C:/Users/taoya/Desktop/MSCS6510/Final project/trains.csv")
features <- read_csv("C:/Users/taoya/Desktop/MSCS6510/Final project/features.csv")

walmart <- read_csv("C:/Users/taoya/Desktop/MSCS6510/Final project/final_walmart.csv")

write_csv(walmart, "walmart.csv")
## change type of values
walmart <- mutate(walmart, store = as.factor(walmart$store))
walmart <- mutate(walmart, type = as.factor(walmart$type))
walmart <- mutate(walmart, is_holiday = as.factor(walmart$is_holiday))

# create columns: month_of_year, week_of_year
walmart<- mutate(walmart, date = ymd(walmart$date))
walmart <- mutate(walmart, month_of_year = as.factor(month(walmart$date)))
walmart <- mutate(walmart, week_of_year = as.factor(week(walmart$date)))

## processing
library(lmtest)

model_1 <- lm(weekly_sales ~ stores_size, data=walmart)
summary(model_1)

walmart <- mutate(walmart, pred_1 = predict(model_1, walmart))

ggplot(data = walmart) + geom_point(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_point(mapping = aes(x = stores_size, y = pred_1, color="red"))
ggsave("model_1_point.jpg")

ggplot(data = walmart) + geom_smooth(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_smooth(mapping = aes(x = stores_size, y = pred_1, color="red"))
ggsave("model_1_smooth.jpg")

ggplot(data = walmart) + geom_point(mapping = aes(x = weekly_sales, y = pred_1)) +
  geom_path(x = seq(from = 0, to = 23000, length = 6435), y = seq(from = 0, to =23000, length = 6435), color = "red")
ggsave("model_1_compare.jpg")




model_2 <- lm(weekly_sales ~ stores_size + date, data=walmart)
summary(model_2)
lrtest(model_1, model_2)
# p = 0.9431 model_1 is better




model_3 <- lm(weekly_sales ~ stores_size + is_holiday, data=walmart)
summary(model_3)
lrtest(model_1, model_3)
# p = 3.898e-06 model_3 is better

walmart <- mutate(walmart, pred_3 = predict(model_3, walmart))

ggplot(data = walmart) + geom_point(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_point(mapping = aes(x = stores_size, y = pred_3, color="red"))
ggsave("model_3_point.jpg")

ggplot(data = walmart) + geom_smooth(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_smooth(mapping = aes(x = stores_size, y = pred_3, color="red"))
ggsave("model_3_smooth.jpg")

ggplot(data = walmart) + geom_point(mapping = aes(x = weekly_sales, y = pred_3)) +
  geom_path(x = seq(from = 0, to = 25000, length = 6435), y = seq(from = 0, to =25000, length = 6435), color = "red")
ggsave("model_3_compare.jpg")





model_4 <- lm(weekly_sales ~ stores_size + is_holiday + month_of_year, data=walmart)
summary(model_4)
lrtest(model_3, model_4)
# p = 2.2e-16 model_4 is better

walmart <- mutate(walmart, pred_4 = predict(model_4, walmart))

ggplot(data = walmart) + geom_point(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_point(mapping = aes(x = stores_size, y = pred_4, color="red"))
ggsave("model_4_point.jpg")

ggplot(data = walmart) + geom_smooth(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_smooth(mapping = aes(x = stores_size, y = pred_4, color="red"))
ggsave("model_4_smooth.jpg")

ggplot(data = walmart) + geom_point(mapping = aes(x = weekly_sales, y = pred_4)) +
  geom_path(x = seq(from = 0, to = 25000, length = 6435), y = seq(from = 0, to =25000, length = 6435), color = "red")
ggsave("model_4_compare.jpg")







model_5 <- lm(weekly_sales ~ stores_size + is_holiday + month_of_year + unemployment, data=walmart)
summary(model_5)
lrtest(model_4, model_5)
# p = 0.008638 model_5 is better


walmart <- mutate(walmart, pred_5 = predict(model_5, walmart))

ggplot(data = walmart) + geom_point(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_point(mapping = aes(x = stores_size, y = pred_5, color="red"))
ggsave("model_5_point.jpg")

ggplot(data = walmart) + geom_smooth(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_smooth(mapping = aes(x = stores_size, y = pred_5, color="red"))
ggsave("model_5_smooth.jpg")

ggplot(data = walmart) + geom_point(mapping = aes(x = weekly_sales, y = pred_5)) +
  geom_path(x = seq(from = 0, to = 25000, length = 6435), y = seq(from = 0, to =25000, length = 6435), color = "red")
ggsave("model_5_compare.jpg")







model_6 <- lm(weekly_sales ~ stores_size + is_holiday + month_of_year + unemployment + temperature, data=walmart)
summary(model_6)
lrtest(model_5, model_6)
# p = 2.2e-16 model_6 is better
walmart <- mutate(walmart, pred_6 = predict(model_5, walmart))

ggplot(data = walmart) + geom_point(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_point(mapping = aes(x = stores_size, y = pred_6, color="red"))
ggsave("model_6_point.jpg")

ggplot(data = walmart) + geom_smooth(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_smooth(mapping = aes(x = stores_size, y = pred_6, color="red"))
ggsave("model_6_smooth.jpg")

ggplot(data = walmart) + geom_point(mapping = aes(x = weekly_sales, y = pred_6)) +
  geom_path(x = seq(from = 0, to = 25000, length = 6435), y = seq(from = 0, to =25000, length = 6435), color = "red")
ggsave("model_6_compare.jpg")




model_7 <- lm(weekly_sales ~ stores_size + is_holiday + month_of_year + unemployment + temperature + fuel_price, data=walmart)
summary(model_7)
lrtest(model_6, model_7)
# p = 0.2966 model_6 is better

model_8 <- lm(weekly_sales ~ stores_size + is_holiday + month_of_year + unemployment + temperature + type, data=walmart)
summary(model_8)
lrtest(model_6, model_8)
# p = 2.2e-16 model_8 is better

walmart <- mutate(walmart, pred_8 = predict(model_8, walmart))

ggplot(data = walmart) + geom_point(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_point(mapping = aes(x = stores_size, y = pred_8, color="red"))
ggsave("model_8_point.jpg")

ggplot(data = walmart) + geom_smooth(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_smooth(mapping = aes(x = stores_size, y = pred_8, color="red"))
ggsave("model_8_smooth.jpg")

ggplot(data = walmart) + geom_point(mapping = aes(x = weekly_sales, y = pred_8)) +
  geom_path(x = seq(from = 0, to = 25000, length = 6435), y = seq(from = 0, to =25000, length = 6435), color = "red")
ggsave("model_8_compare.jpg")



model_9 <- lm(weekly_sales ~ stores_size + is_holiday + month_of_year + unemployment + temperature + type + week_of_year, data=walmart)
summary(model_9)
lrtest(model_8, model_9)
# p = 2.2e-16 model_9 is better
walmart <- mutate(walmart, pred_9 = predict(model_9, walmart))

ggplot(data = walmart) + geom_point(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_point(mapping = aes(x = stores_size, y = pred_9, color="red"))
ggsave("model_9_point.jpg")

ggplot(data = walmart) + geom_smooth(mapping = aes(x = stores_size, y = weekly_sales)) +
  geom_smooth(mapping = aes(x = stores_size, y = pred_9, color="red"))
ggsave("model_9_smooth.jpg")

ggplot(data = walmart) + geom_point(mapping = aes(x = weekly_sales, y = pred_9)) +
  geom_path(x = seq(from = 0, to = 25000, length = 6435), y = seq(from = 0, to =25000, length = 6435), color = "red")
ggsave("model_9_compare.jpg")


ggplot(data = walmart) + geom_point(mapping = aes(x = week_of_year, y = weekly_sales)) +
  geom_point(mapping = aes(x = week_of_year, y = pred_9, color="red"))
ggsave("model_9_week.jpg")


ggplot(data = walmart) + geom_point(mapping = aes(x = month_of_year, y = weekly_sales)) +
  geom_point(mapping = aes(x = month_of_year, y = pred_9, color="red"))
ggsave("model_9_month.jpg")







## boxplot
ggplot(data = walmart, mapping = aes(x = month_of_year, y = weekly_sales)) + geom_boxplot()
ggsave("holiday.jpg")


save.image("walmart")
















