# Talha
# Introduction to Data Science
# Homework Assignment 3 Part 2 Logistic Regression Model and Naive Bayes Model Using Leading Club Dataset
# introduction_To_Data_Science_Homework_Assignment_3_LendingClub_Part2.R

############################################################################################################################################################
# Part 2 Problem 1: Using the LendingClub Dataset that is the Microsoft Excel Comma-Separated Value CSV File "Lending.csv", do the Descriptive Statistics. #
############################################################################################################################################################

# First, install the necessary Library Packages below by going to "Tools" -> "Install Packages" and then typing the name of the Library Package. Second, load the
# necessary libraries.
library(installr)
library(dplyr)
# Load the Library "caTools" in order to split the data.
library(caTools)
# Load the Library "e1071" for Naive Bayes Model.
library(e1071)
library(tidyverse)
# Load the Library "caret" for the Logistic Regression Model.
library(caret)
library(readr)
library(fBasics)
library(readxl)
# Load the Library "ROCR" for the ROC Curve.
library(ROCR)


# Use the setwd() Function to set the specified Path Name as the current working space directory to its Parent Directory.
setwd("C:/Users/Talha/Documents/dataScienceAssignment3AMultipleLinearRegressionModelLogisticRegressionModelNaiveBayesClassifierModel")

# Load the LendingClub Dataset from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv".
lending_club_dataset <- read.csv("Lending.csv", header = TRUE)

# Define the structure for the "lending_club_dataset".
str(lending_club_dataset)

glimpse(lending_club_dataset)

# View the "lending_club_dataset".
View(lending_club_dataset)

# Attach the "lending_club_dataset".
attach(lending_club_dataset)

# Convert the "residence_property" column and "loan_default" column into factor variables.
lending_club_dataset$residence_property = as.factor(lending_club_dataset$residence_property)
lending_club_dataset$loan_default = as.factor(lending_club_dataset$loan_default)

# Create a two-way table for the factor variable "residence_property" and the factor variable "loan_default".
xtabs(~residence_property + loan_default, data = lending_club_dataset)
str(lending_club_dataset)

# First, use the Function "xtabs()" to create a Contingency Table that computes and displays the number of rows where the "residence_property" is "Own" as shown in the "residence_property" Column 
# within the Microsoft Excel Comma-Separated Value CSV File "Lending.csv". Then, in addition, use the Function "xtabs()" for the creation of that very same Contingency Table to compute and 
# display the number of rows where the "residence_property" is "Rent" as shown in the "residence_property" Column within the Microsoft Excel Comma-Separated Value CSV File "Lending.csv".
print("Descriptive Statistics for Categorical Data Table 'residence_property':")
print("------------------------------------------------------------------------")
first_categorical_data_table_for_residence_property <- xtabs(~ residence_property, data = lending_club_dataset)

# Apply the "addmargins()" Function to add the margin total, in other words the sum, of the number of rows where the categorical value of "residence_property" is "Own" and the number of rows 
# where the categorical value of "residence_property" is "Rent" as shown in the "residence_property" Column within the Microsoft Excel Comma-Separated Value CSV File "Lending.csv".
addmargins(first_categorical_data_table_for_residence_property)

# First, use the Function "xtabs()" to create a Contingency Table that computes and displays the number of rows where the "loan_default" is "0" indicating that it is "FALSE" that
# there is a "loan_default", in other words "0" signifies that there is no "loan_default" as shown in the "loan_default" Column within the Microsoft Excel Comma-Separated Value CSV 
# File "Lending.csv". Then, in addition, use the Function "xtabs()" for the creation of the very same Contingency Table that computes and displays the number of rows where the
# Boolean value for "loan_default" is "1" indicating that is "TRUE" there is a "loan_default" as shown in the "loan_default" Column within the Microsoft Excel Comma-Separated Value CSV
# File "Lending.csv".
print("Descriptive Statistics for Categorical Data Table 'loan_default':")
print("------------------------------------------------------------------")
second_categorical_data_table_for_loan_default <- xtabs(~ loan_default, data = lending_club_dataset)

# Apply the "addmargins()" Function to add the margin total, in other words the sum, of the number of rows where the categorical value of the "loan_default" is "0" or "FALSE" and the number of
# rows where the categorical value of "loan_default is "1" or "TRUE" as shown in the "loan_default" Column within the Microsoft Excel Comma-Separated Value CSV File "Lending.csv".
addmargins(second_categorical_data_table_for_loan_default)

# Load the "fBasics" Library.
library(fBasics)
# Use the "basicStats()" from the "fBasics" Library in order to create a Data Frame to get the Descriptive Statistics of all Quantitative Data Table Columns within in the Microsoft 
# Excel Comma-Separated Value CSV
# File "Lending.csv".
print("Descriptive Statistics for All Quantitative Data Tables in Microsoft Excel Comma-Separated Value CSV File 'Lending.csv':")
print("-------------------------------------------------------------------------------------------------------------------------")
options(scipen = 999)
basicStats(data.frame(loan_amnt, adjusted_annual_inc, pct_loan_income, dti, months_since_first_credit, inq_last_6mths, open_acc, bc_util, num_accts_ever_120_pd, pub_rec_bankruptcies))

#################################################################################################################################################################################################
# Part 2 Problem 2: First, build the Logistic Regression Model using the LendingClub Dataset from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv". Then, build the Naive Bayes #
# Model using the LendingClub Dataset from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv".                                                                                    #
#################################################################################################################################################################################################
## Logistic Regression -> Left-Click the Computer Mouse and Highlight Lines 94 to 136 to see the "ROC Curve of Logistic Regression Model for Lendng Club Dataset".

# Use the set.seed() Function to set the seeds for any random numerical number value and maintain consistency.
set.seed(123)

# Split the LendingClub Dataset from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv" into a Training Set and a Testing Set. Use 60% of the LendingClub Dataset from the Microsoft
# Excel Comma-Separated Value CSV as a Training Set. In addition, use the sample() Function to randomly sample data from the LendingClub Dataset in the Microsoft Excel Comma-Separated CSV
# File "Lending.csv".
#lending_club_dataset_one <- sample(2, nrow(lending_club_dataset), replace = T, prob = c(0.6, 0.4))

split <- sample.split(lending_club_dataset$loan_default, SplitRatio = 0.7)
lendingClubTrainingDataset <- subset(lending_club_dataset, split == TRUE)
lendingClubTestingDataset <- subset(lending_club_dataset, split == FALSE)

# Create the Logistic Regression Model for the LendingClub Dataset that is the dataset from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv" by first using the Function glm() in 
# order to build that Logistic Regression Model. The First Parameter, or in other words the First Argument "residence_property ~ ." passed inside the Function "glm()" is a Formula that 
# specifies the prediction of the "residence_property" on the basis of every other variable in the LendingClub Dataset that is the dataset from the Microsoft Excel Comma-Separated Value CSV 
# File "Lending.csv". The Third Parameter, in other words, the Third Argument "family=binomial" within the Function "family = binomial" signifies that a Binary Classification Model is created
# and indicates the Dependent Variable is Binary.
logistic_regression_model_for_lending_club_dataset <- glm(loan_default ~ ., data = lendingClubTrainingDataset, family = "binomial")

# Get the summary of the Logistic Regression Model for the LendingClub Dataset that is the dataset from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv".
summary(logistic_regression_model_for_lending_club_dataset)

# Analyze the "logistic_regression_model_for_lending_club_dataset" from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv" by making predictions 
# on the Test Data that is the "lendingClubTrainingDataset".
logistic_predictions_probabilities <- predict(logistic_regression_model_for_lending_club_dataset, newdata = lendingClubTestingDataset, type = "response")
logistic_prediction_obj <- prediction(logistic_predictions_probabilities, lendingClubTestingDataset$loan_default)
logistic_performance <- performance(logistic_prediction_obj, "tpr", "fpr")
logistic_auc_performance <- performance(logistic_prediction_obj, "auc")
logistic_auc <- as.numeric(logistic_auc_performance@y.values)
plot(logistic_performance, main = "ROC Curve of Logistic Regression Model for LendingClub Dataset", col = "blue")
abline(a = 0, b = 1, lty = 2, col = "green4")	
print(paste("AUC for Logistic Regression:", logistic_auc))

# Confusion Matrix for Logistic Regression Model.
logistic_predictions_binary <- ifelse(logistic_predictions_probabilities > 0.5, 1, 0)
confusion_matrix_logistic <- table(logistic_predictions_binary, lendingClubTestingDataset$loan_default)
print("Confusion Matrix for Logistic Regression Model:")
print(confusion_matrix_logistic)

# Compute the Accuracy of the Logistic Regression Model for the LendingClub Dataset that is the dataset from the Microsoft Excel Comma-Separated Value CSV
# File "Lending.csv".
accuracy_of_logistic_regression_model_for_lending_club_dataset <- sum(diag(confusion_matrix_logistic))/sum(confusion_matrix_logistic)
print(paste("Accuracy of Logistic Regression Model for LendingClub Dataset:", accuracy_of_logistic_regression_model_for_lending_club_dataset))

## Naive Bayes Model

# Create a Naive Bayes Model for the LendingClub Dataset that is the dataset from the Microsoft Excel Commas-Separated Value CSV File "Lending.csv".
naive_bayes_model <- naiveBayes(loan_default ~., data = lendingClubTrainingDataset)

# Get the summary of the Naive Bayes Model for the LendingClub Dataset that is the dataset from the Microsoft Excel Comma-Separated Value CSV File "Lending.csv".
summary(naive_bayes_model)

# Use the Naive Bayes Model to make predictions on the Test Data that is the lendingClubTrainingDataset for Naive Bayes.
naive_bayes_predictions_probabilities <- predict(naive_bayes_model, newdata = lendingClubTestingDataset, type = "raw")
naive_bayes_predictions_obj <- prediction(naive_bayes_predictions_probabilities[,2], lendingClubTestingDataset$loan_default)
naive_bayes_performance <- performance(naive_bayes_predictions_obj, "tpr", "fpr")
naive_bayes_auc_performance <- performance(naive_bayes_predictions_obj, "auc")
naive_bayes_auc <- as.numeric(naive_bayes_auc_performance@y.values)
plot(naive_bayes_performance, main = "ROC Curve of Naive Bayes Model for LendingClub Dataset", col = "red")
abline(a = 0, b = 1, lty = 2, col = "green4")
print(paste("AUC for Naïve Bayes:", naive_bayes_auc))

# Confusion Matrix for Naive Bayes Model.
naive_bayes_predictions <- ifelse(naive_bayes_predictions_probabilities[,2] > 0.5, 1, 0)
confusion_matrix_naive_bayes <- table(naive_bayes_predictions, test_data$loan_default)
print("Confusion Matrix for Naive Bayes Model:")
print(confusion_matrix_naive_bayes)

# Compute the Accuracy of the Naive Bayes Model for the LendingClub Dataset that is the dataset from the Microsoft Excel Commas-Separated Value CSV File "Lending.csv".
accuracy_of_naive_bayes_model_for_lending_club_dataset <- sum(diag(confusion_matrix_naive_bayes))/sum(confusion_matrix_naive_bayes)
print(paste("Accuracy of Naive Bayes Model:", accuracy_of_naive_bayes_model_for_lending_club_dataset))