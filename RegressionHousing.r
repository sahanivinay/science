# Load necessary libraries
library(ggplot2)

# Set a seed for reproducibility
set.seed(123)

# Generate synthetic housing dataset
n_samples <- 100
housing_data <- data.frame(
  Size = rnorm(n_samples, mean = 1500, sd = 300),
  Bedrooms = sample(1:4, n_samples, replace = TRUE)
)
housing_data$Price <- 100000 + 200 * housing_data$Size + 5000 * housing_data$Bedrooms + rnorm(n_samples, mean = 0, sd = 10000)

# Explore the dataset
head(housing_data)

# Linear Regression
linear_model <- lm(Price ~ Size + Bedrooms, data = housing_data)

# Print the summary of the linear model
summary(linear_model)

# Visualize the linear regression line
ggplot(housing_data, aes(x = Size, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: Price vs. Size", x = "Size", y = "Price")

# Logistic Regression
# Create a binary response variable indicating whether the price is above a threshold
threshold <- median(housing_data$Price)
housing_data$Above_Threshold <- as.factor(housing_data$Price > threshold)

# Logistic regression model
logistic_model <- glm(Above_Threshold ~ Size + Bedrooms, data = housing_data, family = "binomial")

# Print the summary of the logistic model
summary(logistic_model)

# Visualize the logistic regression curve
ggplot(housing_data, aes(x = Size, y = Above_Threshold)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +
  labs(title = "Logistic Regression: Above Threshold vs. Size", x = "Size", y = "Above Threshold")
