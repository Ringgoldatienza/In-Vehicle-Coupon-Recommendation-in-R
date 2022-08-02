#In-Vehicle Coupon Recommendation
#by Ringgold P. Atienza

################################################################################
#Install packages and load dataset

#For easy data manipulation and visualization
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#For Fast Aggregation of Data
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#A fast, consistent tool for working with data frame like objects
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#For tyding dataset
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
#For easy machine learning workflow
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#Recursive partitioning for classification, regression and survival trees.
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
#Implements Breiman's random forest algorithm for classification and regression.
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
#Implements eXtreme Gradient Boosting package.
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
#Tools for moving window statistics, GIF, Base64, ROC AUC, etc.
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
#Tools for Sparse and Dense Matrix Classes and Methods
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
#A collection of evaluation metrics
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")
#Make diagrams in R using viz.js or mermaid.js with infrastructure provided by htmlwidgets.
if(!require(DiagrammeR)) install.packages("DiagrammeR", repos = "http://cran.us.r-project.org")

#Load dataset
#Source: Wang, Tong, Cynthia Rudin, Finale Doshi-Velez, Yimin Liu, Erica Klampfl, 
#and Perry MacNeille. 'A bayesian framework for learning rule sets for interpretable classification.' 
#The Journal of Machine Learning Research 18, no. 1 (2017): 2357-2393.
dataset <- read.csv("C://Users//ADMIN//Documents//GitHub//In-Vehicle Coupon Recommendation//Data.csv")


#Partition data into training, validation and test sets.
#I use the 70-15-15 partition as it is one of the most common practice for partition.
#https://www.v7labs.com/blog/train-validation-test-set

set.seed(2022)
test_index <- createDataPartition(y = dataset$Y, times = 1, p = 0.3, list = FALSE)
trainingset <- dataset[-test_index,]
validation <- dataset[test_index,]
rm(test_index)

test_index <- createDataPartition(y = validation$Y, times = 1, p = 0.5, list = FALSE)
testset <- validation[-test_index,]
validation <- validation[test_index,]
rm(test_index)

#In this point, we will only work on the training trainingset
#Explore the trainingset
head(trainingset)
str(trainingset)

#Check for missing values in the trainingset
na_count <- sapply(trainingset, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Inspect data through visualization
#Coupon and Destination
trainingset%>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = destination)) +
  labs(x = "Coupon Acceptance", y = "Destination")

#Coupon and Passenger
trainingset%>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = passanger)) +
  labs(x = "Coupon Acceptance", y = "Passenger")

#Coupon and Weather
trainingset%>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = weather)) +
  labs(x = "Coupon Acceptance", y = "Weather")

#Coupon and Temperature
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(temperature))) +
  labs(x = "Coupon Acceptance", fill = "Temperature")

#Coupon and Time
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(time))) +
  labs(x = "Coupon Acceptance", fill = "Time")

#Coupon and Coupon
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(coupon))) +
  labs(x = "Coupon Acceptance", fill = "Coupon")

#Coupon and Expiration
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(expiration))) +
  labs(x = "Coupon Acceptance", fill = "Expiration")

#Coupon and Gender
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(gender))) +
  labs(x = "Coupon Acceptance", fill = "Gender")

#Coupon and Age
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(age))) +
  labs(x = "Coupon Acceptance", fill = "Age")

#Coupon and Marital Status
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(maritalStatus))) +
  labs(x = "Coupon Acceptance", fill = "Marital Status")

#Coupon and has_children
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(has_children))) +
  labs(x = "Coupon Acceptance", fill = "Has Children")

#Coupon and Education
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(education))) +
  labs(x = "Coupon Acceptance", fill = "Education")

#Coupon and Occupation
trainingset %>% ggplot(aes(y = as.factor(occupation))) + 
  geom_bar(aes(fill = as.factor(Y))) +
  labs(y = "Occupation", fill = "Coupon Acceptance")

#Coupon and Income
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(income))) +
  labs(x = "Coupon Acceptance", fill = "Income")

#Coupon and Car
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(car))) +
  labs(x = "Coupon Acceptance", fill = "Car")

#Coupon and Bar
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(Bar))) +
  labs(x = "Coupon Acceptance", fill = "Bar")

#Coupon and CoffeeHouse
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(CoffeeHouse))) +
  labs(x = "Coupon Acceptance", fill = "CoffeeHouse")

#Coupon and CarryAway
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(CarryAway))) +
  labs(x = "Coupon Acceptance", fill = "Carry Away")

#Coupon and Restaurant Less Than 20
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(RestaurantLessThan20))) +
  labs(x = "Coupon Acceptance", fill = "Restaurant Less Than 20")

#Coupon and Restaurant 20 To 50
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(Restaurant20To50))) +
  labs(x = "Coupon Acceptance", fill = "Restaurant 20 To 50")

#Coupon and To Coupon GEQ 5min
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(toCoupon_GEQ5min))) +
  labs(x = "Coupon Acceptance", fill = "To Coupon GEQ 5min")

#Coupon and To Coupon GEQ 15min
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(toCoupon_GEQ5min))) +
  labs(x = "Coupon Acceptance", fill = "To Coupon GEQ 15min")

#Coupon and To Coupon GEQ 25min
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(toCoupon_GEQ25min))) +
  labs(x = "Coupon Acceptance", fill = "To Coupon GEQ 25min")

#Coupon and To Direction Same
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(direction_same))) +
  labs(x = "Coupon Acceptance", fill = "Direction Same")

#Coupon and To Direction Opp
trainingset %>% ggplot(aes(as.factor(Y))) + 
  geom_bar(aes(fill = as.factor(direction_opp))) +
  labs(x = "Coupon Acceptance", fill = "Direction Opp")


#Drop variables with no variation
#Ex: toCoupon_GEQ5min, toCoupon_GEQ15min,car
trainingset <- subset(trainingset, select = -c(toCoupon_GEQ5min, toCoupon_GEQ15min, car))
testset <- subset(testset, select = -c(toCoupon_GEQ5min, toCoupon_GEQ15min, car))
validation <- subset(validation, select = -c(toCoupon_GEQ5min, toCoupon_GEQ15min, car))

#The direction_opp variable is just the opposite of direction_same.
#This will result to a very high correlation between both variables (one should be excluded)
#We retain the direction_same variable.
trainingset <- subset(trainingset, select = -c(direction_opp))
testset <- subset(testset, select = -c(direction_opp))
validation <- subset(validation, select = -c(direction_opp))

#Some variables have NAs when inspected using barplots. 
#ex. RestaurantLessThan20, Restaurant20To50, CarryAway, CoffeeHouse, Bar
table(trainingset$RestaurantLessThan20)
table(trainingset$Restaurant20To50)
table(trainingset$CarryAway)
table(trainingset$CoffeeHouse)
table(trainingset$Bar)

################################################################################  
#I use the CART Model (since it is more robust in handling missing values)
#Code references:
#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/

#Set interger values into factors
trainingset <- trainingset %>%
  mutate(temperature = as.factor(temperature)) %>%
  mutate(toCoupon_GEQ25min =  as.factor(toCoupon_GEQ25min)) %>%
  mutate(direction_same = as.factor(direction_same)) %>%
  mutate(Y = as.factor(Y))

CARTModel <- rpart(Y ~ destination + passanger + weather + temperature +
                     time + coupon + expiration + gender + age + maritalStatus +
                     has_children + education + income + direction_same +
                     Bar + CoffeeHouse + CarryAway + RestaurantLessThan20 +
                     Restaurant20To50 + toCoupon_GEQ25min, data = trainingset)

#Show result of the CART Model
par(xpd = NA) # Avoid clipping the text in some device
plot(CARTModel)
text(CARTModel, digits = 3)
summary(CARTModel)
print(CARTModel)

#Make sure testset has the same data structure from testset
testset <- testset %>%
  mutate(temperature = as.factor(temperature)) %>%
  mutate(toCoupon_GEQ25min =  as.factor(toCoupon_GEQ25min)) %>%
  mutate(direction_same = as.factor(direction_same)) %>%
  mutate(Y = as.factor(Y))

#Create predictions based on the CART model
predicted.cases <- CARTModel %>% predict(testset, "class")

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(predicted.cases, testset$Y, mode = "everything", positive = "1")

#Set tuning parameter by pruning the tree and check if smaller sub-tree can provide comparable results.
CARTModel2 <- train(
  Y ~ destination + passanger + weather + temperature +time + coupon + 
    expiration + gender + age + maritalStatus + has_children + education + 
    income + direction_same + Bar + CoffeeHouse + CarryAway + RestaurantLessThan20 + 
    Restaurant20To50 + toCoupon_GEQ25min, 
    data = trainingset, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# Plot model accuracy vs different values of cp (complexity parameter)
plot(CARTModel2)

# Print the best tuning parameter cp that maximizes the model accuracy
CARTModel2$bestTune

#Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(CARTModel2$finalModel)
text(CARTModel2$finalModel,  digits = 3)

#Decision rules in the model
CARTModel2$finalModel

#Make predictions on the test data
predicted.cases <- CARTModel2 %>% predict(testset)

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(predicted.cases, testset$Y, mode = "everything", positive = "1")

#Testing for the final model: validation
#Make sure validation has the same data structure from validation
#Set interger values into char
validation <- validation %>%
  mutate(temperature = as.factor(temperature)) %>%
  mutate(toCoupon_GEQ25min =  as.factor(toCoupon_GEQ25min)) %>%
  mutate(direction_same = as.factor(direction_same)) %>%
  mutate(Y = as.factor(Y))

predicted.cases <- CARTModel2 %>% predict(validation)

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(predicted.cases, validation$Y, mode = "everything", positive = "1")

#Create table showing results of F1, Precision and Recall
#Note: I found out that Recall and Precision Function in 'MLMetrics' are interchanged (a bug)
#Results can be checked through confusionMaxtrix
#In this case, I switch both functions
Results <- data.frame(Variable = "CART", 
                      F1 = F1_Score(predicted.cases, validation$Y, positive = "1"),
                      Precision = Recall(predicted.cases, validation$Y, positive = "1"),
                      Recall = Precision(predicted.cases, validation$Y, positive = "1"))

################################################################################
#Check other model for improvement of the prediction
#For the next model: I use the random forest algorithm because it the most widely used
#machine learning algorithm for classification.
#Code reference:
#https://www.r-bloggers.com/2021/04/random-forest-in-r/

RFModel <-randomForest(Y ~ destination + passanger + weather + temperature +
                    time + coupon + expiration + gender + age + maritalStatus +
                    has_children + education + income + direction_same +
                    Bar + CoffeeHouse + CarryAway + RestaurantLessThan20 +
                    Restaurant20To50 + toCoupon_GEQ25min, data = trainingset, proximity = TRUE)

print(RFModel)

#Create prediction values on validation-set
predicted.cases <- predict(RFModel, testset)

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(predicted.cases, testset$Y, mode = "everything", positive = "1")

#Show confusion matrix
table(predicted.cases, testset$Y)

#Set starting value of mtry (as tuning parameter)
floor(sqrt(ncol(trainingset) - 1))

#Tune the parameter by finding mtry value with minimum out of bag(OOB) error
mtry <- tuneRF(trainingset[-5], trainingset$Y, ntreeTry = 500,
               stepFactor = 1.1, improve = 0.01, trace = TRUE, plot = TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

#Run Random Forest using the new mtry value
RFModel <-randomForest(Y ~ destination + passanger + weather + temperature +
                         time + coupon + expiration + gender + age + maritalStatus +
                         has_children + education + income + direction_same +
                         Bar + CoffeeHouse + CarryAway + RestaurantLessThan20 +
                         Restaurant20To50 + toCoupon_GEQ25min, data = trainingset, 
                       proximity = TRUE)

print(RFModel)

#Evaluate variable importance
importance(RFModel)
varImpPlot(RFModel)

#Create prediction values on test-set
predicted.cases <- predict(RFModel, testset)

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(predicted.cases, testset$Y, mode = "everything", positive = "1")

#Create prediction values on validation set
predicted.cases <- predict(RFModel, validation)

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(predicted.cases, validation$Y, mode = "everything", positive = "1")

#Create table showing results of F1, Precision and Recall
Results2 <- data.frame(Variable = "Random Forest", 
                      F1 = F1_Score(predicted.cases, validation$Y, positive = "1"),
                      Precision = Recall(predicted.cases, validation$Y, positive = "1"),
                      Recall = Precision(predicted.cases, validation$Y, positive = "1"))
Results <- rbind(Results, Results2)
rm(Results2)

################################################################################
#Check other model for improvement of the prediction
#For the next model: I use the XGBoost algorithm as it highly regarded as of the 
#most effective techniques.
#Code references:
#https://cran.r-project.org/web/packages/xgboost
#https://www.r-bloggers.com/2021/02/machine-learning-with-r-a-complete-guide-to-gradient-boosting-and-xgboost/

#Create combined dataset of training and test set
combinedset <- rbind(trainingset, testset)

#Transform all categorical data into dummy variables of trainset
sparse_matrix_train <- sparse.model.matrix(Y ~ destination + passanger + weather + 
                                       temperature + time + coupon + expiration + 
                                       gender + age + maritalStatus + has_children + 
                                       education + income + direction_same + Bar + 
                                       CoffeeHouse + CarryAway + RestaurantLessThan20 +
                                       Restaurant20To50 + toCoupon_GEQ25min, 
                                     data = trainingset)[,-1]

#Transform all categorical data into dummy variables of testset
sparse_matrix_test <- sparse.model.matrix(Y ~ destination + passanger + weather + 
                                       temperature + time + coupon + expiration + 
                                       gender + age + maritalStatus + has_children + 
                                       education + income + direction_same + Bar + 
                                       CoffeeHouse + CarryAway + RestaurantLessThan20 +
                                       Restaurant20To50 + toCoupon_GEQ25min, 
                                     data = testset)[,-1]

#Create a numeric vector for the output (Y)
y_train <- as.integer(trainingset$Y) - 1
y_test <- as.integer(testset$Y) - 1

#Create DMatrix data structure for both training and test sets
xgb_train <- xgb.DMatrix(data = as.matrix(sparse_matrix_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(sparse_matrix_test), label = y_test)

#Build list of parameters
xgb_params <- list( booster = "gbtree", eta = 0.01, max_depth = 8,
                    gamma = 4, subsample = 0.75, colsample_bytree = 1,
                    objective = "multi:softprob", eval_metric = "mlogloss",
                    num_class = length(levels(combinedset$Y)))

#Build XGBoost Model
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nrounds = 5000,
                       verbose = 1)
xgb_model

#Make predictions on testset
xgb_preds <- predict(xgb_model, as.matrix(sparse_matrix_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(combinedset$Y)
xgb_preds

#The result in xgb_preds only show the probability of each terminal nodes (Y = 1 or 0)
#We need to convert these probability into actual predictions in a new column
xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(combinedset$Y)[y_test + 1]
xgb_preds

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass),
                mode = "everything", positive = "1")

#Make prediction for the final validation set
#Transform all categorical data into dummy variables of validation set
sparse_matrix_validation <- sparse.model.matrix(Y ~ destination + passanger + weather + 
                                             temperature + time + coupon + expiration + 
                                             gender + age + maritalStatus + has_children + 
                                             education + income + direction_same + Bar + 
                                             CoffeeHouse + CarryAway + RestaurantLessThan20 +
                                             Restaurant20To50 + toCoupon_GEQ25min, 
                                           data = validation)[,-1]

#Create a numeric vector for the output (Y)
y_validation <- as.integer(validation$Y) - 1

#Create DMatrix data structure for validation
xgb_validation <- xgb.DMatrix(data = as.matrix(sparse_matrix_validation), label = y_validation)

#Make predictions on validation set
xgb_preds <- predict(xgb_model, as.matrix(sparse_matrix_validation), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(combinedset$Y)
xgb_preds

#The result in xgb_preds only show the probability of each terminal nodes (Y = 1 or 0)
#We need to convert these probability into actual predictions in a new column
xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(combinedset$Y)[y_validation + 1]
xgb_preds

#Show confusion matrix statistics to check the sensitivity, specificity and balanced accuracy
confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass), 
               mode = "everything", positive = "1")

#Create table showing results of F1, Precision and Recall
Results2 <- data.frame(Variable = "XGBoost", 
                       F1 = F1_Score(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass), positive = "1"),
                       Precision = Recall(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass), positive = "1"),
                       Recall = Precision(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass), positive = "1"))
Results <- rbind(Results, Results2)
rm(Results2)

#Plot XGBoost Multi-Trees
xgb.plot.multi.trees(feature_names = names(xgb_validation), model = xgb_model)

#Plot XGBoost importance matrix
importance_matrix <- xgb.importance(names(xgb_validation), model = xgb_model)
xgb.plot.importance(importance_matrix)
