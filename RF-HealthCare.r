install.packages(c("dplyr", "randomForest"))
library(dplyr)
library(randomForest)

set.seed(123)


n <- 1000  


age <- rnorm(n, mean = 40, sd = 10)
sex <- sample(c("Male", "Female"), n, replace = TRUE)

diabetes <- sample(c("Yes", "No"), n, replace = TRUE)
hypertension <- sample(c("Yes", "No"), n, replace = TRUE)

blood_pressure <- rnorm(n, mean = 120, sd = 10)
heart_rate <- rnorm(n, mean = 75, sd = 5)

cholesterol <- rnorm(n, mean = 200, sd = 20)
health_condition <- sample(c("Healthy", "Chronic Disease", "Acute Condition"), n, replace = TRUE)

healthcare_data <- data.frame(
  Age = age,
  Sex = sex,
  Diabetes = diabetes,
  Hypertension = hypertension,
  BloodPressure = blood_pressure,
  HeartRate = heart_rate,
  Cholesterol = cholesterol,
  HealthCondition = health_condition
)

head(healthcare_data)

healthcare_data$Sex <- as.factor(healthcare_data$Sex)
healthcare_data$Diabetes <- as.factor(healthcare_data$Diabetes)
healthcare_data$Hypertension <- as.factor(healthcare_data$Hypertension)
healthcare_data$HealthCondition <- as.factor(healthcare_data$HealthCondition)

set.seed(456)
train_index <- sample(1:n, 0.8 * n)
train_data <- healthcare_data[train_index, ]
test_data <- healthcare_data[-train_index, ]

rf_model <- randomForest(HealthCondition ~ ., data = train_data)

predictions <- predict(rf_model, newdata = test_data)

confusion_matrix <- table(predictions, test_data$HealthCondition)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix["Healthy", "Healthy"] / sum(confusion_matrix[, "Healthy"])
recall <- confusion_matrix["Healthy", "Healthy"] / sum(confusion_matrix["Healthy", ])
f1_score <- 2 * precision * recall / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

varImpPlot(rf_model)
