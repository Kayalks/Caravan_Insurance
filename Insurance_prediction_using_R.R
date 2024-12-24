
## import libraries
library(pacman)
library(ISLR,ISLR2)
library(lares)
library(caret)
library(C50)
library(randomForest)
library(smotefamily)
library(ROSE)
library(tidyverse)

#Correlation plot
D <- read.csv("C:/Users/Kayalvili/Documents/Surrey/principles of analytics/Coursework/Data/combined_data.csv", sep=",", header =1)
D_socio <- read.csv("C:/Users/Kayalvili/Documents/Surrey/principles of analytics/Coursework/Data/combined_data_socio.csv", sep=",", header =1)
D_policies <- read.csv("C:/Users/Kayalvili/Documents/Surrey/principles of analytics/Coursework/Data/combined_data_policies_sep.csv", sep=",", header =1)
D |> lares::corr_var(CARAVAN, top=25)
D_socio |> lares::corr_var(CARAVAN, top=25)
D_policies |> lares::corr_var(CARAVAN, top=25)

## read data
train<-read.csv("C:/Users/Kayalvili/Documents/Surrey/principles of analytics/Coursework/Data/training_data.csv", sep=",", header =1)

test<-read.csv("C:/Users/Kayalvili/Documents/Surrey/principles of analytics/Coursework/Data/test_data.csv", sep=",", header =1)

table(train$CARAVAN)

column_names <- colnames(train)
print(column_names)

vars <- c("avg_age","avg_income",
           "social_classB1","social_classA","purchasing_power_class", 
           "high_level_education",
           "house_owners","rented_house",
           "income_30_45K","income_45_75K",
           "married",
           "protestant",
           "X1car",
           "entrepreneur","middle_management",
           "num_boat","cn_boat_policies",
           "num_SS_insur","cn_SS_insurance_policies",
           "cn_car_policies","num_car_policies",
           "cn_fire_policies","num_fire_policies",
           "cn_surfboard_policies","num_surfboard",
           "num_of_agri_3rd_insur","cn_agri_3rd_insur",
           "cn_moped_policies","num_moped_policies",
           "CARAVAN")

## Data Balancing
# Db <- ROSE::ovun.sample(CARAVAN ~ .,
#                         data =train[,vars], method = "over", seed = 123, p=0.6)$data
# Db <- ROSE::ovun.sample(CARAVAN ~ .,
                        # data =train[,vars], method = "over", seed = 123, p=0.7)$data
## best balancing
Db <- ROSE::ovun.sample(CARAVAN ~ .,
                        data =train[,vars], method = "over", seed = 123, p=0.8)$data
table(Db$CARAVAN)
Db$CARAVAN <- as.factor(Db$CARAVAN)

## C5Model with hyperparameter tuning
#hyper parameter tuning
ctrl <- trainControl(method="cv", 
                     number =5)

hyperparameters_c5 <- expand.grid(trials = c(5,10), model = c("tree"), winnow = FALSE)

# perform tuning
tuned_model <- train(CARAVAN ~ .,data = Db[,vars],method="C5.0", trControl = ctrl, tuneGrid= hyperparameters_c5)
print(tuned_model)
#Predictions
y<-as.factor(test$caravan)
confusionMatrix(predict(tuned_model,newdata=test),y,positive = "1")

## Logistic Model with hyperparameter tuning
Db <- ROSE::ovun.sample(CARAVAN ~ .,
                        data =train[,vars], method = "over", seed = 123, p=0.5)$data
table(Db$CARAVAN)
Db$CARAVAN <- as.factor(Db$CARAVAN)
#hyper parameter tuning
lmodel <- train(CARAVAN ~ ., data = Db[,vars], method = "glm")
print(lmodel)
#Predictions
y<-as.factor(test$caravan)
confusionMatrix(predict(lmodel,newdata=test),y,positive = "1")

##random forest
Db <- ROSE::ovun.sample(CARAVAN ~ .,
                        data =train[,vars], method = "over", seed = 123, p=0.7)$data
table(Db$CARAVAN)
Db$CARAVAN <- as.factor(Db$CARAVAN)
#hyper parameter tuning
rfctrl <- trainControl(method = "cv", 
                     number = 5, 
                     verboseIter = TRUE)

rfmodel <- randomForest(CARAVAN ~ .,data=Db[,vars], method="rf",trControl=rfctrl)

#Predictions
y<-as.factor(test$caravan)
confusionMatrix(predict(rfmodel,newdata=test),y,positive = "1")


