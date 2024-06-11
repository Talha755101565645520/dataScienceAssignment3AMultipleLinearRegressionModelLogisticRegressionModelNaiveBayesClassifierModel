# Talha
# Introduction to Data Science
# Homework Assignment 3 Part 1 Multiple Linear Regression Model Using Covid Mortality Dataset
# introduction_To_Data_Science_Homework_Assignment_3_CovidMortality_Part1.R

# First, install the necessary Library Packages below by going to "Tools" -> "Install Packages..." and then typing the name of the Library Package. Second, load the 
# necessary libraries.
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)


# Use the setwd() Function to set the specified Path Name as the current working space directory to its Parent Directory.
setwd("C:/Users/Talha/Documents/dataScienceAssignment3AMultipleLinearRegressionModelLogisticRegressionModelNaiveBayesClassifierModel")

# Load the Dataset for Covid Morality by reading in the Data from the Microsoft Excel Comma-Separated Value CSV File "CovidMortality.csv".
covid_mortality_dataset <- read.csv("CovidMortality.csv", header = TRUE)

# Define the structure of the "covid_mortality_dataset".
str(covid_mortality_dataset)

# Use the "glimpse()" Function to print out the columns running down or vertically and print out the rows running towards the right or horizontally from the Microsoft Excel Comma-Separated
# Value CSV File "CovidMortality.csv".
glimpse(covid_mortality_dataset)

# Declare that the variable is measured by levels or categories.
state_median_household_income_status = factor(covid_mortality_dataset$State.Median.Household.Income.Status)

summary(state_median_household_income_status)

# View the "covid_mortality_dataset".
View(covid_mortality_dataset)

# Attach the "covid_mortality_dataset".
attach(covid_mortality_dataset)


# Convert the Area to a numeric data type.
covid_mortality_dataset$Area <- as.numeric(gsub("[^0-9]", "", covid_mortality_dataset$Area))

# Create dummy data. If the "State Median Household Income Status" based on 52 different states from the Microsoft Excel Comma-Separated Value CSV File "CovidMortality.csv" is categorized as "High", 
# then it will have a value of 1. If the "State Median Household Income Status" based on 52 different states from the Microsoft Excel Comma-Separted Value CSV File "CovidMortality.csv" is categorized
# as "Low", then it will have a value of 0.
covid_mortality_dataset <- covid_mortality_dataset %>%
  mutate(income_dummy_data = ifelse(State.Median.Household.Income.Status == "High", 1, 0))

# Build the Multiple Linear Regression Model with Categorical Variable "State Median Household Income Status" from the Microsoft Excel Comma-Separated Value CSV File "CovidMortality.csv".
first_multiple_linear_regression_model <- lm(Deaths ~ Confirmed + Population + Area + income_dummy_data, data = covid_mortality_dataset)

# Build the Multiple Linear Regression Model without the Categorical Variable "State Median Household Income Status".
second_multiple_linear_regression_model <- lm(Deaths ~ Confirmed + Population + Area, data = covid_mortality_dataset)

# Use the summary() Function to summarize the "first_multiple_linear_regression_model".
summary(first_multiple_linear_regression_model)

# Use the summary() Function to summarize the "second_multiple_linear_regression_model".
summary(second_multiple_linear_regression_model)

# Prediction of Deaths based on the both Multiple Linear Regression Models that includes the "first_multiple_linear_regression_model" and the "second_multiple_linear_regression_model".
predictionOfDeathsForFirstMultipleLinearRegressionModel <- predict(first_multiple_linear_regression_model, covid_mortality_dataset)
predictionOfDeathsForSecondMultipleLinearRegressionModel <- predict(second_multiple_linear_regression_model, covid_mortality_dataset)

# Residual Standard Error RSE: The Residual Standard Error RSE is the average deviation of the observed values from the fitted values. A lower Residual Standard Error RSE indicates a better fit
# of the model to the data.

# R-Squared: The R-Squared measures the proportion of the variance in the Dependent Variable explained by the Independent Variables that is in this case the variability in COVID-19 Deaths.

# Adjusted R-Squared: The Adjusted R-Squared adjusts the R-squared value for the number of predictors in the two different multiple linear regression models.

# F-Statistics: The F-statistics for the two different multiple linear regression models defines the overall significance of those multiple linear regression models. A Higher F-statistic and a
# lower P-value indicates a more significant model. 

# Degrees of Freedom DOF: The Degrees of Freedom DOF is calculated based on the total number of observations minus the total number of parameters.

# Create the base plot.
pIsForPlot <- ggplot(covid_mortality_dataset, aes(x = Population, y = Deaths, color = Confirmed)) +
  geom_point() + # Scatterplot for the Number of Deaths versus Population.
  geom_line(aes(y = predictionOfDeathsForFirstMultipleLinearRegressionModel), color = "red", size = 0.2) + # Predictions of Deaths based on the first model.
  geom_line(aes(y = predictionOfDeathsForSecondMultipleLinearRegressionModel), color = "black", size = 0.2) + # Predictions of Deaths based on the second model.
  geom_smooth(aes(x = Confirmed, y = Deaths), method = "lm", se = TRUE, color = "blue") + # Linear Regression Line for Deaths versus Confirmed.
  geom_smooth(aes(x = Population, y = Deaths), method = "lm", se = TRUE, color = "green") + # Linear Regression Line for Deaths versus Population.
  geom_smooth(aes(x = Area, y = Deaths), method = "lm", se = TRUE, color = "purple") + # Linear Regression Line for Deaths versus Area.
  labs(title = "Multiple Linear Regression Model with Deaths as Dependent Variable", x = "Population", y = "Deaths", caption = "RED: Predictions based on the First Model with all Independent Variables including income_dummy \nBLACK : Predictions based on Model with all independent variables excluding income_dummy \nGREEN: Linear Regression Line for Deaths versus Population \nBLUE : Linear Regression Line for Deaths versus Confirmed \nPURPLE : Linear Regression Line for Deaths versus Area") +
  scale_color_gradient(low = "grey", high = "blue") # Color Gradient for Confirmed Number of Cases.

# Print Out the Plot by just typing, highlighting, and running the following line of R Code "print(pIsForPlot)" within the R Studio Console.
print(pIsForPlot)

# Create a Correlation Plot for the "first_multiple_linear_regression_model" in order to check for Multicollinearity. 
temp <- covid_mortality_dataset
reduced_data <- subset(temp,select = -Deaths)
reduced_data <- subset(reduced_data,select = -State)
reduced_data <- subset(reduced_data,select = -State.Median.Household.Income.Status)

# Calculate the correlation to 2 decimal places.
correlation_matrix_one = round(cor(reduced_data), 2)

# Compute and Display the Result.
ggcorrplot(correlation_matrix_one, hc.order = TRUE, type = "lower", lab = TRUE)

# For the Second Model that is the model without income, create a Second Correlation Plot and make sure only the Quantitative Columns are selected.
reduced_data_one <- subset(reduced_data, select = -income_dummy_data)

# Compute correlation at 2 decimal places.
correlation_matrix_two <- round(cor(reduced_data), 2)

# Compute and Display the Result.
ggcorrplot(correlation_matrix_two, hc.order = TRUE, type = "lower", lab = TRUE)
