


#rumor model classifiers with stratified cross validation

library(caTools)
library(e1071)
library(superml)
library(caret)
library(data.table)
library(dplyr)
library(readxl)
library(randomForest)
library(tidyverse)
library(pROC)
library(tree)
library(plyr)
library(MLmetrics)
###########################
#dataset
dataset = read_excel('')
summary(dataset) #summary
lapply(dataset, summary)#summary
str(dataset) #structure

#the target varibale 
result_count <- dataset %>% group_by(Result) %>% summarise(Total_of_results= n())
barplot(result_count$Total_of_results)

#columns renaming 
names(dataset)[names(dataset) == "Followers/Friends"] <- "Followers_Friends"
names(dataset)[names(dataset) == "Favorite/Statu"] <- "Favorite_Statu"

################
#dummying the variables 
Cols <-  which(sapply(dataset, is.logical))
setDT(dataset)

for(j in Cols){
  set(dataset, i=NULL, j=j, value= as.numeric(dataset[[j]]))
}


label <- LabelEncoder$new()
#dataset$Result <- label$fit_transform(dataset$Result)
dataset$Verified <- label$fit_transform(dataset$Verified)
dataset$Favorited <- label$fit_transform(dataset$Favorited)
dataset$Retweeted <- label$fit_transform(dataset$Retweeted)
dataset$Protected <- label$fit_transform(dataset$Protected)
dataset %>% mutate_if(is.numeric, round,digits=1)
dataset$Followers_Friends <- round(as.numeric(dataset$Followers_Friends)) # Round off the column to integer
dataset$Favorite_Statu <- round(as.numeric(dataset$Favorite_Statu)) # Round off the column to integer

sapply(dataset, class) #the columns data types

#########################
#the selected features
new_dataset= dataset %>% select(Favorited, Friends,Followers,Status_count,Fovorites_count,
                                E_score, Followers_Friends,Time_span,Retweet_Count,Lists,
                                Result)

dataset %>% 
  select(Favorited, Friends,Followers,Status_count,Fovorites_count,
         E_score, Followers_Friends,Time_span,Retweet_Count,Lists,
         Result) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")


#splitting the data
# split = sample.split(new_dataset$Result, SplitRatio = 0.7)
# training_set = subset(new_dataset, split == TRUE)
# prop.table(table(training_set$Result))
# 
# test_set = subset(new_dataset, split == FALSE)
# prop.table(table(test_set$Result))
####################
#building the model
# SVM

#Labeling result

cvIndex <- createFolds(factor(new_dataset$Result), folds, returnTrain = T)

tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = folds
                   # classProbs = T,
                   # savePredictions = T,
                   # summaryFunction = twoClassSummary
                   )

svmFit <- train(Result ~ ., data = training_set, 
               method = "svmLinear", 
               trControl = tc)


#rfFit$bestTune

plot (varImp(svmFit,scale=FALSE), main= "Var Imp: SVM CV")

confusionMatrix(svmFit)

confusionMatrix(svmFit$pred$pred, svmFit$pred$obs)

#########################################################3

#without Labeling result

#new_dataset$Result=str_replace_all(new_dataset$Result,"not rumor","not_rumor") #removing hamzah

new_dataset$Result[new_dataset$Result == 'not rumor'] <- 'not_rumor'

# new_dataset %>% 
#   mutate(Result = factor(Result, 
#                          levels  = make.names(levels(Result))))
# 
# 
# new_dataset$Result<- factor(new_dataset$Result, 
#                   levels = c(1, 2), 
#                   labels = c("rumor", "not_rumor"))

tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = 5,
                  classProbs = T,
                   savePredictions = T,
                  summaryFunction = twoClassSummary
)

svmFit11 <- train(Result ~ ., data = new_dataset, 
                method = "svmLinear",
                preProc=c("center", "scale"), 
                trControl = tc,
                metric = "ROC",
                na.action=na.exclude)

confusionMatrix(table(svmFit11$pred$pred, svmFit11$pred$obs))


ddply(svmFit11$pred, "Resample", summarise,
      accuracy = Accuracy(pred, obs))

#####################################################

#Naive Bayes Model 


nv <- train(Result ~ ., data = new_dataset, 
                  method = "nb",
                  preProc=c("center", "scale"), 
                  trControl = tc,
                  metric = "ROC",
                  na.action=na.exclude)

confusionMatrix(table(nv$pred$pred, nv$pred$obs))


ddply(nv$pred, "Resample", summarise,accuracy = Accuracy(pred, obs))

# # Predicting on test data'
# y_pred <- predict(nv, newdata = test_set)
# 
# # Confusion Matrix
# 
# confusionMatrix(y_pred, as.factor(test_set$Result), mode = "prec_recall", positive="1")
# 
# roc(test_set$Result,factor(y_pred, ordered = TRUE), plot=TRUE,label=TRUE)



###################################
#tree randomforest  
rf <- train(Result ~ ., data = new_dataset, 
            method = "rf",
            preProc=c("center", "scale"), 
            trControl = tc,
            metric = "ROC",
            na.action=na.exclude)

confusionMatrix(table(rf$pred$pred, rf$pred$obs))


ddply(rf$pred, "Resample", summarise,accuracy = Accuracy(pred, obs))

# training_set$Result <- as.character(training_set$Result)
# training_set$Result <- as.factor(training_set$Result)
# 
# rf_classifier <- randomForest(Result ~ ., data = training_set, ntree= 200, importance = TRUE,
#                               proximity=T)
# rf_classifier
# #table(predict(rf_classifier), training_set$Result)
# plot(rf_classifier)
# importance(rf_classifier)
# 
# test_set$Result <- as.character(test_set$Result)
# test_set$Result <- as.factor(test_set$Result)
# 
# prediction = predict(rf_classifier, newdata=test_set)
# perf <- performance(prediction,"tpr","fpr")
# 
# 
# confusionMatrix(prediction, as.factor(test_set$Result), mode = "prec_recall", positive="1")
# confusionMatrix(as.factor(test_set$Result),prediction)
# 
# 
# roc(test_set$Result,factor(prediction,  ordered = TRUE), plot=TRUE,label=TRUE)

###############################



