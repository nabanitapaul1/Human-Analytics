#Importing library

library(xlsx)
library(ggplot2)
library(mice)
library(VIM)
library(e1071)
library(gmodels)
library(caret)
library(MLmetrics)
library(DMwR) # for unbalanced classification problem (Data Mining and smote package)


# Importing Dataset

ha_train <-  read.csv("D:\\HumanAnalytics\\train_LZdllcl.csv")
ha_train <- ha_train[,-1]
View(ha_train)

# converting to factor
attach(ha_train)
ha_train$department <- as.factor(department)

ha_train$region <- as.factor(region)
ha_train$education <- as.factor(education)
ha_train$gender <- as.factor(gender)
ha_train$recruitment_channel  <- as.factor(recruitment_channel)
ha_train$awards_won. <- as.factor(awards_won.)
ha_train$KPIs_met..80. <- as.factor(KPIs_met..80.)
ha_train$is_promoted<- as.factor(is_promoted)

str(ha_train)

# Renaming column names
colnames(ha_train)[colnames(ha_train)== "KPIs_met..80."] <-  "KPIs_met>80%"
colnames(ha_train)[colnames(ha_train)== "awards_won."] <- "awards_won?"
names(ha_train)
attach(ha_train)

summary(ha_train)

# EDA

#Graphical Representation

# For department

barplot(table(department), col = "Yellow", xlab = "Name Of Departments", ylab = "No. of Employees")

ggplot(ha_train, aes(x=department )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Different Departments", y="No. of employees")
  
# For Region
ggplot(ha_train, aes(x=region )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Different Regions", y="No. of employees")
levels(education)

table(region)
View(table(region))

# For  education

ggplot(ha_train, aes(x=education )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Education levels", y="No. of employees")

# For gender

ggplot(ha_train, aes(x=gender )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Gender", y="No. of employees")


# For recruitment_channel
ggplot(ha_train, aes(x=recruitment_channel )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Requriment Channels from where they where recruited", y="No. of employees")

# For no_of_trainings

hist(no_of_trainings)
max(no_of_trainings)

min(no_of_trainings)

ggplot(ha_train, aes(x=as.factor(no_of_trainings) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Number of training", y="No. of employees")


plot(length_of_service, no_of_trainings )
cor(length_of_service, no_of_trainings )

# For age
hist(age)
max(age)
min(age)

# for previous_year_rating

hist(previous_year_rating)
ggplot(ha_train, aes(x=as.factor(previous_year_rating) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Previous year ratings", y="No. of employees")

# For length_of_service

hist(length_of_service)
ggplot(ha_train, aes(x=as.factor(length_of_service) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Length of service", y="No. of employees")
min(length_of_service)
max(length_of_service)

# For KPIs_met>80%

ggplot(ha_train, aes(x=`KPIs_met>80%`) )+
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+
    labs(x="Persons meets KPIs_met>80%", y="No. of employees")

table(ha_train$`KPIs_met>80%`)

# for awards_won?
ggplot(ha_train, aes(x=`awards_won?`) )+
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+
  labs(x="Persons won award and not won award", y="No. of employees")

table(ha_train$`awards_won?`)

# For avg_training_score
hist(avg_training_score)
ggplot(ha_train, aes(x=as.factor(avg_training_score) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ labs(x="Average training score", y="No. of employees")

max (avg_training_score)
min(avg_training_score)

# is_promoted?

ggplot(ha_train, aes(x=as.factor(`is_promoted`) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ 
  labs(x="Promotions", y="No. of employees")
table(ha_train$`is_promoted`)  
prop.table(table(ha_train$`is_promoted`))  
           
# Imbalanced Datasets

set.seed(121)

balance_test_data <-SMOTE(is_promoted~., ha_train, perc.over = 600,perc.under = 200,k=5 )

ggplot(balance_test_data, aes(x=as.factor(`is_promoted`) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+ 
  labs(x="Promotions", y="No. of employees")
table(balance_test_data$is_promoted)

str(balance_test_data$is_promoted)
levels(balance_test_data$is_promoted)

# Missing value 

is.na(ha_train)
sum(is.na (ha_train))

md.pattern(ha_train)
mice_plot <- aggr(ha_train, col=c('grey','blue'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(ha_train), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern")) 

summary(ha_train$previous_year_rating)

View()

# Model Building

ha.model = naiveBayes(balance_test_data$is_promoted ~.,data=balance_test_data, laplace = 1)
summary(ha.model)
ha.model$levels
ha.model$apriori    
ha.model$isnumeric
ha.model$tables
ha.model$call
summary(ha.model)

# Model Evaluation

# Accuracy for train data

train_pred = predict(ha.model, balance_test_data[,-13])
View(train_pred)
compare_true_pred = data.frame(train_pred,balance_test_data[,13])
names(compare_true_pred)

colnames(compare_true_pred)[colnames(compare_true_pred)== "ha_train...13."] <- "true_train"
View(compare_true_pred)


Accuracy(y_pred=train_pred, y_true= balance_test_data$is_promoted) # 

#crosstable

ct <- CrossTable(x = balance_test_data$is_promoted, y = train_pred,prop.chisq=FALSE)

cm =  confusionMatrix(balance_test_data$is_promoted,train_pred)

# F1 score
f1_score = F1_Score( y_pred=train_pred, y_true= balance_test_data$is_promoted) 

#Prediction for test data

ha_test= read.csv("D:/HumanAnalytics/test_2umaH9m.csv")
ha_test =ha_test[,-1]
attach(ha_test)
str(ha_test)
ha_test$department <- as.factor(department)
ha_test$region <- as.factor(region)
ha_test$education <- as.factor(education)
ha_test$gender <- as.factor(gender)
ha_test$recruitment_channel  <- as.factor(recruitment_channel)
ha_test$awards_won. <- as.factor(awards_won.)
ha_test$KPIs_met..80. <- as.factor(KPIs_met..80.)
str(ha_test)
labels(ha_test)
names(ha_test)

# Renaming column names
colnames(ha_test)[colnames(ha_test)== "KPIs_met..80."] <-  "KPIs_met>80%"
colnames(ha_test)[colnames(ha_test)== "awards_won."] <- "awards_won?"
labels(ha_test)

# Predict for the test datasets

ha.predict = predict(ha.model,newdata= ha_test)
View(ha.predict)
table(ha.predict)
str(ha.predict)
levels(ha.predict)

# Importing label data

ha_test_labels =read.csv("D:/HumanAnalytics/sample_submission_M0L0uXE.csv")
ha_test_labels$is_promoted<- as.factor(ha_test_labels$is_promoted)
str(ha_test_labels$is_promoted)
View(ha_test_labels)
levels(ha_test_labels$is_promoted) <- c(levels(ha_test_labels$is_promoted),1)
names(ha_test_labels)


levels(ha_test_labels$is_promoted)
compare = cbind(ha_test_labels,ha.predict) 
names(compare)
colnames(compare)[colnames(compare)== "ha.predict"] <- "is_promoted_predicted"
View(compare)
str(compare)

# Writing to csv
getwd()
write.csv(compare,file = "sample_submission.csv", row.names = FALSE)

