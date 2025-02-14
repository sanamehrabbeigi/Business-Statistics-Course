
##  data
bike <- read.csv("D:/Ivey/Business Statistics/Assignment1/BikeshareData.csv", 
                 header = TRUE)
View(bike)
str(bike)
summary(bike)

##   Analysis
qqnorm(bike$rental)
qqline(bike$rental, col = "red")

hist(bike$rental)
#not normally distributed

####by seasons###

#divide to 4 seasons
spring = subset(bike, season == "spring")
summer = subset(bike, season == "summer")
fall = subset(bike, season == "fall")
winter = subset(bike, season == "winter")

View(spring)
summary(spring)
summary(summer)
summary(fall)
summary(winter)

shapiro.test(spring$rental)
shapiro.test(summer$rental)
shapiro.test(fall$rental)
shapiro.test(winter$rental)
#not normally distributed
par(mfrow = c(2, 2))
hist(spring$rental,main = "Spring", xlab = "rental", col = "lightgreen")
hist(summer$rental,main = "Summer", xlab = "rental", col = "yellow")
hist(fall$rental,main = "Fall", xlab = "rental", col = "orange")
hist(winter$rental,main = "Winter", xlab = "rental", col = "lightblue")
par(mfrow = c(1, 1))



install.packages("ggplot2")
library(ggplot2)

# in same plot
ggplot(bike, aes(x = rental, fill = season)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("spring" = "green", "summer" = "yellow", "fall" = "orange", "winter" = "blue")) +
  labs(title = "Histogram of Bike Data Across Four Seasons",
       x = "rental",  
       y = "Frequency") +
  theme_minimal()

#in different plots
ggplot(bike, aes(x = rental)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~season) +
  labs(title = "Histogram of Bike Data by Season",
       x = "rental",  
       y = "Frequency") +
  theme_minimal()


#######################
##   Hypothesis Test
########################
# null hypothesis --> 4 seasons have the same means
kruskal.test(rental ~ season, data =bike)
# we rejected the null hypothesis because p-value < 0.05 => they dont have the same mean

#box plot
boxplot(data = bike, rental ~ season, main = "Box Plot of Total Numbers of Rental by season", col = c("orange", "green","yellow","blue"))
mean(spring$rental)
median(spring$rental)

ggplot(bike, aes(x = season, y =rental, fill = season)) +
  geom_boxplot() +
  labs(title = "Boxplot of Bike Data Across Seasons",
       x = "Seasons",
       y = "Rental Counts") + 
  theme_minimal()
# we can see that the most popular season for rent a bike is summer

#######################
##   How these factors affected users' riding decisions?
########################

###weekend

#hist(bike$weekend)

weekends = subset(bike, weekend == 1)
weekdays = subset(bike, weekend == 0)

wilcox.test(rental ~ weekend,data = bike)
boxplot(data = bike, rental ~ weekend, main = "Box Plot of Total Numbers of Rental by weekend", col = c("purple", "coral"))


####Hour###

ggplot(bike, aes(x = hour, y = rental)) +
  geom_point() +
  labs(title = "Scatter Plot of hour vs Bike Count",
       x = "Hour",
       y = "rental Count") +
  theme_minimal()


plot(weekdays$hour,weekdays$rental, col = 'blue', pch = 19, xlab = "Rental Counts", ylab = "Hour", main = "Rental Counts based on hours for weekdays vs weekends")
points( weekends$hour, weekends$rental, col = 'red', pch = 17)
legend("topright", legend = c("Weekdays", "Weekends"), col = c("blue", "red"), pch = c(19, 17))

plot(bike$hour, bike$rental, main = "Scatter Plot of rental and hour", xlab = "hour", ylab = "rental counts", col = "blue", pch = 10)
model1 = lm(data = bike, rental ~ hour)
abline(model1, col = "red", lwd = 3)

####Temperature ###

plot(bike$temperature, bike$rental, main = "Scatter Plot of rental and temperature", xlab = "temperature", ylab = "rental counts", col = "darkred", pch = 10)
model1 = lm(data = bike, rental ~ temperature)
abline(model1, col = "blue", lwd = 3)
summary(model1)


ggplot(bike, aes(x = temperature, y = rental, color = season)) +
  geom_point() +
  labs(title = "Scatter Plot of Temperature vs Bike Count by Season",
       x = "Temperature (°C)",
       y = "rental Count") + theme_minimal()

ggplot(bike, aes(x = temperature, y = rental, color = humidity)) +
  geom_point() +
  scale_color_gradient(low = "light blue", high = "blue") +
  labs(title = "Scatter Plot of Temperature vs Bike Count by Humidity",
       x = "Temperature (°C)",
       y = "Bike Count",
       color = "Humidity (%)") + theme_minimal()
