#--------------------DATA LOADING--------------------
df <- read.csv("C:/Users/elif_/OneDrive/Belgeler/g/Salary.csv")

#--------------------DATA PREPROCESSING--------------------
#Check if there is missing values
missing_values <- colSums(is.na(df))
print(missing_values)

#--------------------DATA EXPLORATION--------------------
#General Observation
str(df)
summary(df$Age)

#Dublicated Control
dublicated_data <- sum(duplicated(df))
unique(dublicated_data)

#Remove dublicated data
df <- df[!duplicated(df), ]
dimension_df <- dim(df)
dimension_df

#-------------------- Histograms--------------------
#Continuous
#Age
hist(df$Age, probability = TRUE,breaks = 20,col = "cadetblue",main="Histogram of Age")
lines(density(df$Age), col = 'tomato1', lwd = 3)

#Education Level
hist(df$Education.Level, probability = TRUE,col = "cadetblue",main="Histogram of Education Level")
lines(density(df$Education.Level), col = 'tomato1', lwd = 3)

#Years of Experience
hist(df$Years.of.Experience, probability = TRUE,breaks = 20,col = "cadetblue",main="Histogram of Years Of Experience")
lines(density(df$Years.of.Experience), col = 'tomato1', lwd = 3)

#Salary
hist(df$Salary, probability = TRUE,breaks=30,col = "cadetblue",main="Histogram of Salary")
lines(density(df$Salary), col = 'tomato1', lwd = 3)

#Senior
hist(df$Senior, probability = TRUE,breaks=30,col = "cadetblue",main="Histogram of Senior")
lines(density(df$Senior), col = 'tomato1', lwd = 3)


#--------------------DATA CLEANING--------------------
#Encoding
#install.packages("fastDummies")
library(fastDummies)
df <- dummy_cols(df, select_columns = c("Gender", "Education.Level", "Job.Title", "Country", "Race"))

df$Job.Title <- as.factor(df$Job.Title)
df <- dummy_cols(df)

df$Education.Level <- as.factor(df$Education.Level)
df <- dummy_cols(df)

head(df)
df = subset(df, select = -c(Gender,Job.Title,Country,Race,Education.Level) )
dim(df)
head(df)
summary(df)

#SCALING
# Specify the columns to be scaled
standarized_cols <- c("Age", "Years.of.Experience")

# Standardize the selected columns
df[standarized_cols] <- scale(df[standarized_cols])

# Display the first few rows of the updated data frame
head(df)


#--------------------FEATURE SELECTION --------------------
#Boruta
#install.packages("Boruta")
library(Boruta)
set.seed(111)
boruta.bank_train <- Boruta(Salary~., data = df, doTrace = 2)
print(boruta.bank_train)

boruta.bank <- TentativeRoughFix(boruta.bank_train)
print(boruta.bank)

plot(boruta.bank, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.bank$ImpHistory),function(i)
  boruta.bank$ImpHistory[is.finite(boruta.bank$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.bank$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.bank$ImpHistory), cex.axis = 0.7)

getSelectedAttributes(boruta.bank, withTentative = F)

bank_df <- attStats(boruta.bank)
print(bank_df)

df <- df[, which(bank_df$decision == "Confirmed")]

#--------------------CORRELATION MATRIX--------------------
install.packages("corrplot")
library(corrplot)


#Select columns
selected_columns <- df[, 1:15]

#Calculate cor matrix
cor_matrix <- cor(selected_columns)

#visualize matrix
corrplot(cor_matrix, 
         method = "number",
         tl.pos = "lt",
         tl.cex = 0.7,
         title = "Correlation Matrix"
)

df <- subset(df, select = -Age)
colnames(df) <- gsub(" ", "_", colnames(df))
summary(df)



#--------------------VISUALIZATION--------------------
#Categoric Variables
install.packages("tm")
install.packages("wordcloud")

library(tm)
library(wordcloud)
#Bar Plots
#Job Title
barplot(table(df$Job.Title), col = c('blue', 'cadetblue', 'white', 'skyblue'), main = 'Job Title Distribution', xlab = 'Job Title', ylab = 'Count')

#Gender
barplot(table(df$Gender),col = c('cadetblue', 'gray'), main = 'Gender Distribution', xlab = 'Gender', ylab = 'Count')

#Country
barplot(table(df$Country),col = c('cadetblue', 'gray'), main = 'Country Distribution', xlab = 'Country', ylab = 'Count')

#Race
barplot(table(df$Race),col = c('cadetblue', 'gray'), main = 'Race Distribution', xlab = 'Race', ylab = 'Count')

#Salary Distribution in Each Country
library(ggplot2)
#Graph1
ggplot(df, aes(x = Country, y = Salary, fill = Country)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Country", x = "Country", y = "Salary")
#Graph2
ggplot(df, aes(x = Country, y = Salary)) +
  geom_bar(stat = "identity", position = "dodge", fill = "cadetblue") +
  labs(title = "Salary by Country", x = "Country", y = "Salary")

#Salary Distribution by Education Level
ggplot(df, aes(x = Education.Level, y = Salary)) +
  geom_point(position = position_jitter(width = 0.2), color = "tomato", alpha = 0.7) +
  labs(title = "Salary vs. Education Level", x = "Education Level", y = "Salary")

#Salary Distribution by Race
ggplot(df, aes(x = Race, y = Salary, fill = Race)) +
  geom_bar(stat = "summary", fun = "median") +
  labs(title = "Salary by Race", x = "Race", y = "Salary")

#Age Distribution by Gender
#Graph1
ggplot(df, aes(x = Gender, y = Age)) +
  geom_boxplot(fill = "cadetblue") +
  labs(title = "Age Distribution by Gender", x = "Gender", y = "Age")
#Graph2
ggplot(df, aes(x = Gender, y = Age)) +
  geom_point(position = position_jitter(width = 0.2), color = "cadetblue", alpha = 0.7) +
  labs(title = "Age Distribution by Gender", x = "Gender", y = "Age")

#Box plot of Age
ggplot(df, aes(y = Age)) +
  geom_boxplot(fill = "cadetblue") +
  labs(title = "Box Plot of Age", y = "Age")
#Box plot of Years.Of.Experience
ggplot(df, aes(y = Years.of.Experience)) +
  geom_boxplot(fill = "cadetblue") +
  labs(title = "Box Plot of Years.Of.Experience", y = "Years.of.Experience")

#Scatter plot of Age and Salary
ggplot(df, aes(x = Age, y = Salary)) +
  geom_point(color = "orange") +
  labs(title = "Scatter Plot of Age and Salary", x = "Age", y = "Salary")



#--------------------SPLIT TRAIN TEST VALIDATION DATA--------------------
library(caret)

target <- "Salary"
descriptive <- colnames(df[, -which(names(df) == "Salary")])

# Set the seed for reproducibility
set.seed(42)
# Create training, validation, and testing sets
splitSample <- sample(1:3, size=nrow(df), prob=c(0.8,0.1,0.1), replace = TRUE)
train_data <- df[splitSample==1,]
validation_data <- df[splitSample==2,]
test_data <- df[splitSample==3,]

#--------------------MODELING--------------------
#--------------------SINGLE LINEAR REGRESSION--------------------
#LEARNING PROCESS
library(caret)
library(MASS)

##----Linear Regression Model Creating---#
#Fit the linear regression model
start_time <- Sys.time()
mlr_model <- lm(Salary ~ Years.of.Experience, data = train_data)
end_time <- Sys.time()
runtime <- end_time - start_time
print(paste("Runtime:", as.numeric(runtime), "seconds"))

##------------For Train Set-----------#
#Make predictions on the train set
train_predictions <- predict(mlr_model, newdata = train_data)

##------------For Validation Set-----------#
#Make predictions on the validation set
validation_predictions <- predict(mlr_model, newdata = validation_data)


library(ggplot2)

# Plotting actual vs predicted values for training set
ggplot(data = train_data, aes(x = Years.of.Experience, y = Salary)) +
  geom_point(color = "blue") +
  geom_line(aes(y = train_predictions), color = "red") +
  labs(title = "Linear Regression - Training Set",
       x = "Years of Experience",
       y = "Salary")

# Plotting actual vs predicted values for validation set
ggplot(data = validation_data, aes(x = Years.of.Experience, y = Salary)) +
  geom_point(color = "blue") +
  geom_line(aes(y = validation_predictions), color = "red") +
  labs(title = "Linear Regression - Validation Set",
       x = "Years of Experience",
       y = "Salary")


##------------For Test set Set-----------#
#Make predictions on the test set
test_predictions <- predict(mlr_model, newdata = test_data)

# Plotting actual vs predicted values for test set
ggplot(data = test_data, aes(x = Years.of.Experience, y = Salary)) +
  geom_point(color = "blue") +
  geom_line(aes(y = test_predictions), color = "red") +
  labs(title = "Linear Regression - Test Set",
       x = "Years of Experience",
       y = "Salary")

print(mlr_model)

#CALCULATION TEST,TRAIN VE VALIDATION OF R_SQAURED

#Calculate R-squared for validation set
validation_r_squared <- 1 - sum((validation_data$Salary - validation_predictions)^2) / sum((validation_data$Salary - mean(validation_data$Salary))^2)
#Calculate R-squared for test and train sets
test_r_squared <- 1 - sum((test_data$Salary - test_predictions)^2) / sum((test_data$Salary - mean(test_data$Salary))^2)
train_r_squared <- 1 - sum((train_data$Salary - train_predictions)^2) / sum((train_data$Salary - mean(train_data$Salary))^2)


#Display results
cat("Test Error:", test_r_squared, "\n")
cat("Train Error:", train_r_squared, "\n")
cat("Validation Error:", validation_r_squared, "\n")

# Residuals
residuals <- residuals(mlr_model)

# Sum of Squared Residuals (SSE)
sse <- sum(residuals^2)

# Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Coefficient of Determination (R^2)
r_squared <- summary(mlr_model)$r.squared

# Mean Absolute Error (MAE)
mae <- mean(abs(residuals))

# Print the results
cat("Sum of Squared Residuals (SSE):", sse, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Coefficient of Determination (R^2):", r_squared, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")


#--------------------SINGLE LINEAR CROSS VALIDATION--------------------
#install.packages(c("fastDummies", "caret", "MASS", "DAAG"))
library(fastDummies)
library(caret)
library(MASS)
library(DAAG)


# K-Fold Cross Validation
set.seed(125)

# Create the control object for k-fold cross-validation
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# Perform k-fold cross-validation for linear regression
cv_model <- train(Salary ~ Years.of.Experience , data = df, method = "lm", trControl = ctrl)

# Display cross-validation results
print(cv_model)

##------------For Train Set-----------#
#Make predictions on the train set
train_predictions <- predict(cv_model, newdata = train_data)

##------------For Test set Set-----------#
#Make predictions on the test set
test_predictions <- predict(cv_model, newdata = test_data)

#Plotting Model
# Create a scatter plot with regression lines
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))

# Plotting actual vs predicted values for test set
ggplot(data = test_data, aes(x = Years.of.Experience, y = Salary)) +
  geom_point(color = "blue") +
  geom_line(aes(y = test_predictions), color = "red") +
  labs(title = "Linear Regression - Test Set",
       x = "Years of Experience",
       y = "Salary")

# Residuals
residuals <- residuals(cv_model)

# Sum of Squared Residuals (SSE)
sse <- sum(residuals^2)

# Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Coefficient of Determination (R^2)
r_squared <- summary(cv_model)$r.squared

# Mean Absolute Error (MAE)
mae <- mean(abs(residuals))

# Print the results
cat("Sum of Squared Residuals (SSE):", sse, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Coefficient of Determination (R^2):", r_squared, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

#--------------------MULTI-LINEAR REGRESSION--------------------
# install.packages("caret")
library(caret)

set.seed(125)
start_time <- Sys.time()

# Fit the multiple linear regression model
mlr_model <- lm(Salary ~ ., data = train_data)

end_time <- Sys.time()

# Calculate the runtime
runtime <- end_time - start_time
print(paste("Runtime:", as.numeric(runtime), "seconds"))

# Make predictions on the training, validation, and test sets
train_predictions <- predict(mlr_model, newdata = train_data)
validation_predictions <- predict(mlr_model, newdata = validation_data)
test_predictions <- predict(mlr_model, newdata = test_data)

#Calculate R-squared for validation set
validation_r_squared <- 1 - sum((validation_data$Salary - validation_predictions)^2) / sum((validation_data$Salary - mean(validation_data$Salary))^2)
#Calculate R-squared for test and train sets
test_r_squared <- 1 - sum((test_data$Salary - test_predictions)^2) / sum((test_data$Salary - mean(test_data$Salary))^2)
train_r_squared <- 1 - sum((train_data$Salary - train_predictions)^2) / sum((train_data$Salary - mean(train_data$Salary))^2)

#Plotting Model
# Create a scatter plot with regression lines
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))

# Plotting actual vs predicted values for test set
ggplot(data = test_data, aes(x = Years.of.Experience, y = Salary)) +
  geom_point(color = "blue") +
  geom_line(aes(y = test_predictions), color = "red") +
  labs(title = "Multi-Linear Regression - Test Set",
       x = "Years of Experience",
       y = "Salary")

# Print the results
cat("Test Error:", test_r_squared, "Train Error:", train_r_squared, "Validation Error:", validation_r_squared, "\n")

# Residuals
residuals <- residuals(mlr_model)

# Sum of Squared Residuals (SSE)
sse <- sum(residuals^2)

# Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Coefficient of Determination (R^2)
r_squared <- summary(mlr_model)$r.squared

# Mean Absolute Error (MAE)
mae <- mean(abs(residuals))

# Print the results
cat("Sum of Squared Residuals (SSE):", sse, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Coefficient of Determination (R^2):", r_squared, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

#--------------------MULTI-LINEAR REGRESSION CROSS-VALIDATION--------------------
install.packages(c("fastDummies", "caret", "MASS", "DAAG"))
library(fastDummies)
library(caret)
library(MASS)
library(DAAG)


# K-Fold Cross Validation
set.seed(125)

# Create the control object for k-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform k-fold cross-validation for linear regression
cv_model <- train(Salary ~ ., data = df, method = "lm", trControl = ctrl)

# Display cross-validation results
print(cv_model)

test_predictions <- predict(cv_model, newdata = test_data)
train_predictions <- predict(cv_model, newdata = train_data)
test_r_squared <- 1 - sum((test_data$Salary - test_predictions)^2) / sum((test_data$Salary - mean(test_data$Salary))^2)
cat("Test Error:", test_r_squared)
# Residuals
residuals <- residuals(cv_model)

# Sum of Squared Residuals (SSE)
sse <- sum(residuals^2)

# Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Coefficient of Determination (R^2)
r_squared <- summary(cv_model)$r.squared

# Mean Absolute Error (MAE)
mae <- mean(abs(residuals))

# Print the results
cat("Sum of Squared Residuals (SSE):", sse, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Coefficient of Determination (R^2):", r_squared, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

#--------------------MULTI-LINEAR REGRESSION PARAMETER TUNING--------------------
library(caret)
library(glmnet)     
library(Metrics)    

# Create the model
model <- "glmnet"

# Specify the hyperparameter grid
hyperparameters <- expand.grid(
  alpha = seq(0, 1, by = 0.1),  # Regularization parameter (0 for Ridge, 1 for Lasso)
  lambda = seq(0.001, 0.1, by = 0.001)  # Regularization strength
)

fitControl <- trainControl(
  method = "cv",       # Cross-validation
  number = 5,          # Number of folds
  verboseIter = TRUE   # Print progress
)

# Train the model
lm_model <- train(
  Salary ~ ., 
  data = train_data, 
  method = model, 
  trControl = fitControl,
  tuneGrid = hyperparameters
)
predictions <- predict(lm_model, newdata = test_data)

# Evaluate performance
rmse <- sqrt(mean((test_data$Salary - predictions)^2))
rsquared <- cor(predictions, test_data$Salary)^2

print(paste("RMSE:", rmse))
print(paste("R-squared:", rsquared))

# Plotting actual vs predicted values for test set
ggplot(data = test_data, aes(x = Years.of.Experience, y = Salary)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predictions), color = "red") +
  labs(title = "Multi-Linear Regression - Test Set",
       x = "Years of Experience",
       y = "Salary")
# Residuals
residuals <- residuals(lm_model)

# Sum of Squared Residuals (SSE)
sse <- sum(residuals^2)

# Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Error (MAE)
mae <- mean(abs(residuals))

# Print the results
cat("Sum of Squared Residuals (SSE):", sse, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")



#--------------------RIDGE REGRESSION FOR OVERFITTING--------------------
set.seed(123)
control = trainControl(method ="cv", number = 5)
Grid_reg = expand.grid(alpha = 0, lambda = seq(0.001, 0.1,
                                                  by = 0.0002))
start_time <- Sys.time()

# Training Ridge Regression model
Ridge_model = train(x = train_data,
                    y = train_data$Salary,
                    method = "glmnet",
                    trControl = control,
                    tuneGrid = Grid_reg
)

end_time <- Sys.time()

# Calculate the runtime
runtime <- end_time - start_time
print(paste("Runtime:", as.numeric(runtime), "seconds"))

Ridge_model

# mean validation score
mean(Ridge_model$resample$RMSE)

# Plot
plot(Ridge_model, main="Ridge Regression")


#--------------------DECISION TREE--------------------
#install.packages("tree")
library(tree)
library(rpart)
library(rpart.plot)

set.seed(42)

start_time <- Sys.time()
# Training the Decision Tree Regressor
tree_model <- rpart(Salary ~ ., data = train_data)
#tree_model <- tree(Salary ~ ., data = train_data)
end_time <- Sys.time()

# Calculate the runtime
runtime <- end_time - start_time
print(paste("Runtime:", as.numeric(runtime), "seconds"))

# Printing the test and train errors
test_pred <- predict(tree_model, newdata = test_data)
train_pred <- predict(tree_model, newdata = train_data)
validation_pred <- predict(tree_model, newdata = validation_data)

test_acc <- cor(test_pred, test_data$Salary)^2
train_acc <- cor(train_pred, train_data$Salary)^2
validation_acc <- cor(validation_pred, validation_data$Salary)^2

cat("Test Accuracy: ", test_acc, " Train Accuracy: ", train_acc, "Validation Accuracy: ", validation_acc)

# Residuals
residuals <- residuals(tree_model)

# Sum of Squared Residuals (SSE)
sse <- sum(residuals^2)

# Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Error (MAE)
mae <- mean(abs(residuals))

# Print the results
cat("Sum of Squared Residuals (SSE):", sse, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

# Plot the decision tree
rpart.plot(tree_model, main = "Decision Tree")


#--------------------DECISION TREE CROSS VALIDATION AND PARAMETER TUNING--------------------
install.packages("caret")
library(caret)

# Create a training control object for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.01))  # Complexity parameter values


set.seed(123)
dt_model <- train(Salary ~ .,  
                  data = train_data,  
                  method = "rpart",  
                  trControl = ctrl,  
                  tuneGrid = grid)  

# Print the best parameters
print(dt_model)

plot(dt_model$results$cp, 1 - dt_model$results$Rsquared, type = "l", ylab = "Error", xlab = "cp", main = "Error Rate for Dataset With Different Subsets of Data")

# Access the best model
best_dt_model <- dt_model$finalModel

# Printing the test and train errors
test_pred <- predict(best_dt_model, newdata = test_data)
train_pred <- predict(best_dt_model, newdata = train_data)
test_acc <-cor(test_pred, test_data$Salary)^2
train_acc <- cor(train_pred, train_data$Salary)^2
cat("Test Accuracy: ", test_acc, " Train Accuracy: ", train_acc)

#--------------------RANDOM FOREST--------------------
# install.packages("randomForest")
library(randomForest)
# Train the random forest model
start_time <- Sys.time()
forest_model <- randomForest(Salary ~ ., data = train_data)
end_time <- Sys.time()
runtime <- end_time - start_time
print(paste("Runtime:", as.numeric(runtime), "seconds"))

# Print the test and train errors
test_pred <- predict(forest_model, newdata = test_data)
train_pred <- predict(forest_model, newdata = train_data)
validation_pred <- predict(forest_model, newdata = validation_data)

#R square calculations
test_acc <- cor(test_pred, test_data["Salary"])^2
train_acc <- cor(train_pred, train_data["Salary"])^2
validation_acc <- cor(validation_pred, validation_data["Salary"])^2

cat("Test Accuracy: ", test_acc, " Train Accuracy: ", train_acc, " Validation Accuracy: ", validation_acc)


train_mse <- mean((train_data$Salary - train_pred)^2)
test_mse <- mean((test_data$Salary - test_pred)^2)

cat("Training Mean Squared Error:", train_mse, "\n")
cat("Test Mean Squared Error:", test_mse, "\n")


plot(forest_model)
varImpPlot(forest_model)

#find number of trees that produce lowest test MSE
which.min(forest_model$mse)

#find RMSE of best model
sqrt(forest_model$mse[which.min(forest_model$mse)])

#--------------------RANDOM FOREST CROSS VALIDATION--------------------
# Repeated CV.
ctrl <- trainControl(method = "cv", number = 5)

set.seed(42)
rf_model <- train(Salary ~ ., data = df, trControl = ctrl, verbose = TRUE)

# Print the test and train errors
test_pred <- predict(rf_model, newdata = test_data)
train_pred <- predict(rf_model, newdata = train_data)

test_acc <- cor(test_pred, test_data["Salary"])^2
train_acc <- cor(train_pred, train_data["Salary"])^2

cat("Test Accuracy: ", test_acc, " Train Accuracy: ", train_acc)
plot(rf_model)

#RANDOM FOREST PARAMETER TUNING
tunegrid <- expand.grid(.mtry = (1:15)) 

rf_gridsearch <- train(Salary ~ ., 
                       data = df,
                       method = 'rf',
                       metric = 'RMSE',
                       tuneGrid = tunegrid)
print(rf_gridsearch)


#--------------------GRADIENT BOOSTING--------------------
#install.packages("gbm")
library(gbm)
#install.packages("caret")
library(caret)

# Train the gradient boosting model
start_time <- Sys.time()
boosting_model <- gbm(Salary ~ ., data = train_data, distribution = "gaussian", n.trees = 100, interaction.depth = 3, shrinkage = 0.1)
end_time <- Sys.time()
runtime <- end_time - start_time
print(paste("Runtime:", as.numeric(runtime), "seconds"))

# Print the test and train errors
train_pred <- predict(boosting_model, newdata = train_data, n.trees = 100)
test_pred <- predict(boosting_model, newdata = test_data, n.trees = 100)
validation_pred <- predict(boosting_model, newdata = validation_data, n.trees = 100)

#R-Squared Calculations
train_acc <- cor(train_pred, train_data$Salary)^2
test_acc <- cor(test_pred, test_data$Salary)^2
validation_acc <- cor(validation_pred, validation_data$Salary)^2

cat("Test Accuracy: ", test_acc, " Train Accuracy: ", train_acc, "Validation Accuracy: ", validation_acc)

summary(boosting_model)
plot(boosting_model$train.error, ylab = "Error")


#Plotting the test error vs number of trees
trees <- seq(1, 100, by = 1)
test_errors <- numeric(length(trees))

for (i in seq_along(trees)) {
  pred <- predict(boosting_model, newdata = test_data, n.trees = trees[i])
  test_errors[i] <- cor(pred, test_data$Salary)^2
}

#Plot the results
plot(trees, test_errors, pch = 19, col = "blue", xlab = "Number of Trees", ylab = "Test Accuracy", main = "Performance of Boosting on Test Set")
abline(h = min(test_errors), col = "red")
abline(h = mean(test_errors), col = "orange")
abline(h = max(test_errors), col = "green")


#--------------------GRADIENT BOOSTING CROSS-VALIDATION--------------------
# install.packages("gbm")
# install.packages("caret")
library(gbm)
library(caret)

# Create a train control with 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Train the gradient boosting model with cross-validation
boosting_model <- train(
  Salary ~ .,
  data = train_data,
  trControl = ctrl,
  method = "gbm",
)

# Access the cross-validated results
cv_results <- boosting_model$results

# Print the cross-validated results
print(cv_results)

# Print the best model
best_model <- boosting_model$finalModel
print(best_model)

var_importance <- varImp(best_model)
print(var_importance)

#Plotting the test error vs number of trees
trees <- seq(1, 100, by = 1)
test_errors <- numeric(length(trees))

for (i in seq_along(trees)) {
  pred <- predict(best_model, newdata = test_data, n.trees = trees[i])
  test_errors[i] <- cor(pred, test_data$Salary)^2
}
head(test_errors)
#Plot the results
plot(trees, test_errors, pch = 19, col = "blue", xlab = "Number of Trees", ylab = "Test Accuracy", main = "Performance of Boosting on Test Set")
abline(h = min(test_errors), col = "red")
abline(h = mean(test_errors), col = "orange")
abline(h = max(test_errors), col = "green")

plot(cv_results$Rsquared)

#--------------------GRADIENT BOOSTING PARAMETER TUNING--------------------

 set.seed(42)
 # Train model.
 gbm_model <- train(Salary ~ ., 
                    data = train_data, 
                    method = "gbm", 
                    trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                    verbose = FALSE,
                    tuneLength = 4)

 # Print the test and train errors
 test_pred <- predict(gbm_model, newdata = test_data, n.trees = 100)
 train_pred <- predict(gbm_model, newdata = train_data, n.trees = 100)
 
 test_acc <- cor(test_pred, test_data$Salary)^2
 train_acc <- cor(train_pred, train_data$Salary)^2
 
 cat("Test Accuracy: ", test_acc, " Train Accuracy: ", train_acc)
 # Residuals
 residuals <- residuals(gbm_model)
 
 # Sum of Squared Residuals (SSE)
 sse <- sum(residuals^2)
 
 # Mean Squared Error (MSE)
 mse <- mean(residuals^2)
 
 # Root Mean Squared Error (RMSE)
 rmse <- sqrt(mse)
 
 # Mean Absolute Error (MAE)
 mae <- mean(abs(residuals))
 
 # Print the results
 cat("Sum of Squared Residuals (SSE):", sse, "\n")
 cat("Mean Squared Error (MSE):", mse, "\n")
 cat("Root Mean Squared Error (RMSE):", rmse, "\n")
 cat("Mean Absolute Error (MAE):", mae, "\n")
 print(gbm_model)
 
 #-------------------------NEURAL NETWORK--------------------
 install.packages("neuralnet")
 library(neuralnet)
 library(MASS)
 
 hist(df$Salary)
 
 MaxValue <- apply(df, 2, max)
 MinValue <- apply(df, 2, min)
 
 normData <- as.data.frame(scale(df, center = MinValue, scale = MaxValue-MinValue ))
 
 
 samp<-floor(0.7 * nrow(df))
 set.seed(123956619)
 
 index.train <- sample(seq_len(nrow(normData)), size = samp)
 df.train <- normData[index.train, ]
 df.test <- normData[-index.train, ]
 str(normData)
 
 allVars <- colnames(normData)
 
 PredictVars <- allVars[!allVars%in%"Salary"]
 PredictVars
 
 PredictVars <- paste(PredictVars, collapse = "+")
 
 ModelFormula <- as.formula(paste("Salary~", PredictVars, collapse = "+"))
 ModelFormula
 
 #------------------------- first NN --------------------------
 start_time <- Sys.time()
 
 nn1 <- neuralnet(ModelFormula, data=df.train, hidden=c(4,2), linear.output=T)
 
 end_time <- Sys.time()
 
 # Calculate the runtime
 runtime <- end_time - start_time
 print(paste("Runtime:", as.numeric(runtime), "seconds"))
 
 
 print(nn1$result.matrix)
 
 plot(nn1)
 
 
 pr.nn_train <- compute(nn1, df.train)
 SSE.train <- sum((df.train$Salary - pr.nn_train$net.result)^2)/2
 SSE.train 
 
 
 pr.nn_test <- compute(nn1, df.test)
 SSE.test <- sum((df.test$Salary - pr.nn_test$net.result)^2)/2
 SSE.test
 
 error.df <- data.frame(df.test$Salary, pr.nn_test$net.result)
 head(error.df)
 
 plot(x = df.test$Salary, y =pr.nn_test$net.result, xlab="True Salary", ylab="Predicted Salary")
 
 abline(lm(df.test$Salary ~ pr.nn_test$net.result, data=error.df ), col = "blue")
 
 mean_salary <- mean(df.test$Salary)
 SST <- sum((df.test$Salary - mean_salary)^2)
 
 # Calculate SSE for the test set
 SSE_test <- sum((df.test$Salary - pr.nn_test$net.result)^2)
 
 # Calculate R-squared
 R_squared <- 1 - (SSE_test / SST)
 
 MAE_test <- mean(abs(df.test$Salary - pr.nn_test$net.result))
 
 # Calculate RMSE for the first neural network on the test set
 RMSE_test <- sqrt(mean((df.test$Salary - pr.nn_test$net.result)^2))
 
 print(paste("R^2 for the first neural network:", R_squared))
 print(paste("MAE for the first neural network:", MAE_test))
 print(paste("RMSE for the first neural network:", RMSE_test))
 
 #----------------------------- second NN --------------------------
 start_time <- Sys.time()
 nn2 <- neuralnet(ModelFormula, data=df.train, hidden=c(8,4), linear.output=T)
 end_time <- Sys.time()
 runtime <- end_time - start_time
 print(paste("Runtime:", as.numeric(runtime), "minute"))
 
 print(nn2$result.matrix)
 
 plot(nn2)
 
 pr.nn2_train <- compute(nn2, df.train)
 SSE.train2 <- sum((df.train$Salary - pr.nn2_train$net.result)^2)/2
 SSE.train2 
 
 pr.nn2_test <- compute(nn2, df.test)
 SSE.test2 <- sum((df.test$Salary - pr.nn2_test$net.result)^2)/2
 SSE.test2
 
 error2.df <- data.frame(df.test$Salary, pr.nn2_test$net.result)
 head(error2.df)
 
 plot(x=df.test$Salary, y=pr.nn2_test$net.result, xlab="True Salary", ylab="Predicted Salary")
 
 abline(lm(df.test$Salary ~ pr.nn2_test$net.result, data=error2.df ), col = "blue")
 
 
 SSE_test2 <- sum((df.test$Salary - pr.nn2_test$net.result)^2)
 R_squared2 <- 1 - (SSE_test2 / SST)
 MAE_test2 <- mean(abs(df.test$Salary - pr.nn2_test$net.result))
 RMSE_test2 <- sqrt(mean((df.test$Salary - pr.nn2_test$net.result)^2))
 print(paste("R-squared for the second neural network:", R_squared2))
 print(paste("MAE_test2 for the second neural network:", MAE_test2))
 print(paste("RMSE_test2 for the second neural network:", RMSE_test2))
 
 
 