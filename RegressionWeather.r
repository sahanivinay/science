library(ggplot2)

set.seed(123)
weather_data <- data.frame(
  Temperature = rnorm(100, mean = 25, sd = 5),
  Humidity = runif(100, min = 40, max = 80),
  WindSpeed = rnorm(100, mean = 10, sd = 2),
  Rain = sample(c(0, 1), 100, replace = TRUE)
)

head(weather_data)

linear_model <- lm(Temperature ~ Humidity + WindSpeed, data = weather_data)

summary(linear_model)

ggplot(weather_data, aes(x = Humidity, y = Temperature)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Linear Regression: Temperature vs. Humidity")

logistic_model <- glm(Rain ~ Temperature + Humidity + WindSpeed, data = weather_data, family = "binomial")

summary(logistic_model)

ggplot(weather_data, aes(x = Humidity, y = factor(Rain))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Logistic Regression: Rain vs. Humidity")

