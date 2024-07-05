
# Loading the Dataset

library(readxl)

healthcare_data <- read_excel('Health_Dataset.xlsx')

View(healthcare_data)

# Performing data preprocessing steps 

library(dplyr)

new_healthcare_data <- healthcare_data %>%
  filter(final_grade_year > 0, rank > 0, global_rank > 0)

View(new_healthcare_data)

# Checking the number of null values present in the dataset

sum(is.na(healthcare_data))

healthcare_data$final_grade_year[is.na(healthcare_data$final_grade_year)] <- mean(healthcare_data$final_grade_year, na.rm = TRUE)


# Performing Data Visualization

plot(healthcare_data$max_tech,healthcare_data$min_tech...5)


plot(healthcare_data$max_tech,healthcare_data$median_tech, col='red')


plot(healthcare_data$max_tech,healthcare_data$min_tech...7, col='blue')


plot(healthcare_data$median_tech,healthcare_data$min_tech...7, col = 'green')


plot(healthcare_data$max_tech,healthcare_data$rank, col='black')


plot(healthcare_data$median_tech,healthcare_data$final_grade_year, col='red')


# Dividing the data set into training set and testing set


library(caret)

set.seed(123) 
trainIndex <- createDataPartition(healthcare_data$median_tech, p = 0.8, list = FALSE) # 80% for training
trainData <- healthcare_data[trainIndex,] # Extract the training set
testData <- healthcare_data[-trainIndex,] # Extract the test set


median_value <- median(healthcare_data$median_tech)

# Creating a new variable X_1 that stores 1 and 0 based on the median value of median_tech column

healthcare_data$X_1 <- ifelse(healthcare_data$median_tech > median_value, 1, 0)

# Creating a Logistic Regression Model

logistic_regression_model <- glm(X_1~final_grade_year + rank, data = healthcare_data, family = 'binomial')

summary(logistic_regression_model)



predictions <- predict(logistic_regression_model, newdata = healthcare_data, type = "response")


confusion_matrix <- table(healthcare_data$X_1, round(predictions))

# Print the confusion matrix
confusion_matrix


accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

accuracy

library(pROC)

roc_curve <- roc(healthcare_data$X_1, predictions)
auc <- auc(roc_curve)

plot(roc_curve, main = "ROC curve")
abline(a = 0, b = 1, lty = 2)
legend("bottomright", paste0("AUC = ", round(auc, 2)))

# Creating a Decision Tree Model


library(rpart)

decision_tree_model <- rpart(X_1~final_grade_year + rank, data = healthcare_data, method = "class")

summary(decision_tree_model)

predictions_tree <- predict(decision_tree_model, newdata = healthcare_data, type = "vector")


confusion_matrix_tree <- table(healthcare_data$X_1, round(predictions_tree))

# Print the confusion matrix
confusion_matrix_tree


accuracy <- sum(diag(confusion_matrix_tree)) / sum(confusion_matrix_tree)

accuracy

# Plotting X_1 and final_grade_year to determine relation between them 

plot(healthcare_data$X_1,healthcare_data$final_grade_year)


plot(healthcare_data$X_1,healthcare_data$rank)

# Creating a Random Forest Model


# Load the randomForest package
library(randomForest)

# Create the random forest model
random_forest_model <- randomForest(X_1 ~ max_tech, data = healthcare_data, importance = TRUE, proximity = TRUE)

# Predict using the model
random_forest_model_predictions <- predict(random_forest_model, newdata = healthcare_data)

# Check accuracy
confusion_matrix_random_forest <- table(healthcare_data$X_1, round(random_forest_model_predictions))

confusion_matrix_random_forest

accuracy <- sum(diag(confusion_matrix_random_forest)) / sum(confusion_matrix_random_forest)

accuracy









