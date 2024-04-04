library(tidyverse)
library(furrr)
library(glmnet)
library(pROC)
library(GGally)
rm(list=ls())

setwd("/Users/Tanner/Library/CloudStorage/Box-Box/I2DB Datathon 2024/data/processed")

#Top variables to try for logistic regression
##Left Plot
#Min.Weight.Result
#Number.of.Outpatient.Visits.Last.Year
#First.Creatinine.Result
#Last.Weight.Result

##Right Plot
#Number.of.Outpatient.Visits.Last.Year
#Age.At.Admission
#Last.Hemoglobin.Result
#Max.Sodium.Days.From.Admit

#Lasso Regression
#Loading in training set and selecting appropriate variables
#Modifying X1Yr_Death to a logical variable
data <- readRDS("train_set_imputed.rds") %>% 
  dplyr::select(Patient_ID,Min.Weight.Result,Number.of.Outpatient.Visits.Last.Year,
                First.Creatinine.Result, Last.Weight.Result,
                Age.At.Admission, Last.Hemoglobin.Result,
                Max.Sodium.Days.From.Admit,X1Yr_Death) %>% 
  mutate(X1Yr_Death = as.logical(X1Yr_Death))

### EDA Plots
data %>% dplyr::select(-Patient_ID) %>% 
  ggpairs(aes(color = X1Yr_Death),
          lower = list(continuous = wrap("points", alpha = 0.5)))

#Possibly log these variables
#Min.Weight.Result, Number.of.Outpatient.Visits.Last.Year,
#First.Creatinine.Result, Last.Weight.Result, Max.Sodium.Days.From. Admit
log_vars <- c("Min.Weight.Result", "Number.of.Outpatient.Visits.Last.Year",
  "First.Creatinine.Result", "Last.Weight.Result", "Max.Sodium.Days.From.Admit")

data_log <- data %>% mutate_at(log_vars, ~log(.x))

data_log %>% dplyr::select(-Patient_ID) %>% 
  ggpairs(aes(color = X1Yr_Death),
          lower = list(continuous = wrap("points", alpha = 0.5)))


valid <- data %>% slice_sample(prop = 0.3) %>% dplyr::select(-Patient_ID)

train <- data %>% filter(!(Patient_ID %in% valid$Patient_ID))%>% dplyr::select(-Patient_ID)
                
x <- model.matrix(X1Yr_Death~., train)

y <- as.logical(train$X1Yr_Death)

cv.out <- cv.glmnet(x, y, alpha = 1,family = "binomial", type.measure = "mse")

plot(cv.out)

lambda_min <- cv.out$lambda.min

lambda_1se <- cv.out$lambda.1se

coef(cv.out, s = lambda_1se)

x_test <- model.matrix(X1Yr_Death~., valid)

lasso_prob <- predict(cv.out, newx = x_test, s = lambda_1se, type = "response")

lasso_predict <- rep("neg", nrow(valid))
lasso_predict[lasso_prob > 0.5] <- "pos"

#confusion matrix
table(pred = lasso_predict, true = valid$X1Yr_Death)

#Accuracy
mean(lasso_predict == valid$X1Yr_Death)

#######


#Setting options for parallel processing
cores <- availableCores()
plan(multisession,workers =  cores - 1)
options <- furrr_options(seed=123)

# Create empty vectors to store predictions and actual values
#
# predictions <- matrix(NA, nrow = nrow(data), ncol = 2)
# actual <- rep(NA, nrow(data))
# accurate <- matrix(NA, nrow = nrow(data), ncol = 2)
  
# Perform LOOCV
  loo_accuracy <- 
    future_map(1:nrow(data), function(x){
      
      predictions <- matrix(NA, nrow = 1, ncol = 2)
      #actual <- rep(NA, 1)
      accurate <- matrix(NA, nrow = 1, ncol = 2)
      
      # Create training data (all observations except the ith one)
      train_data <- data[-x, ]
      
      # Train logistic regression models
      model_first_order <- glm(formula = X1Yr_Death ~ ., family = binomial("logit"),
                   data = train_data, na.action = "na.omit")
      
      model_second_order <- glm(X1Yr_Death ~ .^2, family = binomial("logit"), 
                                data = train_data, na.action = "na.omit")
      
      
      # Predict on the left-out observation
      #For first order model
      predictions[1, 1] <- ifelse(predict(model_first_order, newdata = data[x, ], type = "response") > 0.5, TRUE, FALSE)
      
      #For second order model
      predictions[1, 2] <- ifelse(predict(model_second_order, newdata = data[x, ], type = "response") > 0.5, TRUE, FALSE)
      
      # Record the actual value
      actual <- data$X1Yr_Death[x]
      
      #matrix that tabulates if the model prediction was accurate for that observation
      #For first order model
      accurate[1, 1] <- actual == predictions[1, 1]
      
      #For second order model
      accurate[1, 2] <- actual == predictions[1, 2]
      
      return(as.data.frame(accurate))
    }) %>% 
    bind_rows() %>% 
    set_names(c("First Order", "Second Order"))

colMeans(loo_accuracy)

#Assessing sensitivity and specificity
#First Order Model
model_first_order <- glm(formula = X1Yr_Death ~ ., family = binomial("logit"),
                                                    data = train)

#Predictions for First Order Model
first_order_predictions <- predict(model_first_order, newdata = valid, type = "response")

#Contingency Table for First Order Model
contingency_1 <- data.frame(Predicted = ifelse(first_order_predictions > 0.5, TRUE, FALSE),
           Actual = valid$X1Yr_Death) %>% table()

sensitivity_1 <- contingency_1[2,2]/(contingency_1[1,2] + contingency_1[2,2])

specificity_1 <- contingency_1[1,1]/(contingency_1[1,1] + contingency_1[2,1])

auc_1 <- auc(valid$X1Yr_Death, first_order_predictions)
auc_1

roc_1 <- roc(valid$X1Yr_Death, first_order_predictions)
plot(roc_1)

#Second Order Interaction Model
model_second_order <- glm(formula = X1Yr_Death ~ .^2, family = binomial("logit"),
                         data = train)

#Predictions for Second Order Model
second_order_predictions <- predict(model_second_order, newdata = valid, type = "response")
                  

#Contingency Table for Second Order Model
contingency_2 <- data.frame(Predicted = ifelse(second_order_predictions > 0.5, TRUE, FALSE),
           Actual = valid$X1Yr_Death) %>% table()

sensitivity_2 <- contingency_2[2,2]/(contingency_2[1,2] + contingency_2[2,2])

specificity_2 <- contingency_2[1,1]/(contingency_2[1,1] + contingency_2[2,1])

auc_2 <- auc(valid$X1Yr_Death, second_order_predictions)
auc_2

roc_2 <- roc(valid$X1Yr_Death, second_order_predictions)
plot(roc_2)


