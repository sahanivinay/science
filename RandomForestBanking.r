# Load libraries
library(randomForest)
library(caret)

# Create banking dataset
n <- 1000
set.seed(101)
data <- data.frame(
  loan_amt = runif(n,1000, 100000),
  term = sample(c(36, 60), n, replace = TRUE),  
  interest = runif(n, 3, 15),
  income =  runif(n, 20000, 200000),
  credit_score = sample(500:850, n, replace = TRUE),
  risk = sample(c("low","medium","high"),n, replace = TRUE)
)
head(data)

# Pre-process data
data$term <- as.factor(data$term)
data$risk <- as.factor(data$risk)

# Define training and testing sets
trainIndex <- createDataPartition(data$risk, p=0.7, list = FALSE)
train <- data[trainIndex,]
test <- data[-trainIndex,]

# Build random forest model 
rf_model <- randomForest(risk~., data = train, ntree=500)

# Prediction on test data  
predictions <- predict(rf_model, test)

# Evaluation metrics
#confusionMatrix(predictions, test$risk)
#accuracy <- sum(predictions == test$risk)/length(test$risk) 
#print(paste("Accuracy:", accuracy))
cm <- table(predictions, test$risk)
acc <- sum(diag(cm))/sum(cm)
print(cm)
cat("Acc",acc,"\n")
varImpPlot(rf_model) # Variable importance plot