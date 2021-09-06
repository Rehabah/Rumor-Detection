

#rumor model classifiers

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
library(ROCR)
library(naniar)
###########################
#dataset
dataset = read_excel(')


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
dataset$Result <- label$fit_transform(dataset$Result)
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


#missing values 
miss_var_summary(dataset)
#replace missing values
dataset$Lists[is.na(dataset$Lists)] <- 0

#splitting the data
split = sample.split(new_dataset$Result, SplitRatio = 0.7)
training_set = subset(new_dataset, split == TRUE)
prop.table(table(training_set$Result))

test_set = subset(new_dataset, split == FALSE)
prop.table(table(test_set$Result))
####################
#building the model
# SVM
classifier = svm(formula = Result ~ .,data = training_set,type = 'C-classification',kernel = 'linear')
#radial
summary(classifier)


#testing
y_pred = predict(classifier, newdata = test_set,type = "decision")
#confusion matrix
#cm = table(test_set$Result, y_pred)
confusionMatrix(y_pred, as.factor(test_set$Result), mode = "prec_recall", positive="1")

#to check
correct.test<-test_set[test_set$Result==y_pred, ]
mean(test_set$Result==y_pred)
wrong.test<-test_set[test_set$Result!=y_pred, ]
mean(test_set$Result!=y_pred)



#plotting te model 
plot(test_set, pch=16)
points(test_set$Result, y_pred, col = "blue", pch=16)

#plotting the model  (to add)



roc(test_set$Result,factor(y_pred, ordered = TRUE), plot=TRUE,label=TRUE)


#####################################################

#Naive Bayes Model 

classifier_nv <- naiveBayes(Result ~ ., data = training_set)
summary(classifier_nv)




# Predicting on test data'
y_pred <- predict(classifier_nv, type = 'class',newdata = test_set)

# Confusion Matrix

confusionMatrix(y_pred, as.factor(test_set$Result), mode = "prec_recall", positive="1")

roc(test_set$Result,factor(y_pred, ordered = TRUE), plot=TRUE,label=TRUE)


##################################
#tree 

model_tree<- partykit::ctree(Result ~ ., training_set)
y_pred_tree <- predict(model_tree, newdata = test_set,type = "density")
y_pred_tree <- predict(model_tree, newdata = test_set,type = "quantile")
y_pred_tree <- predict(model_tree, newdata = test_set,type = "response")

cm_tree <- table( y_pred_tree,test_set$Result)

plot(model_tree)
confusionMatrix(as.factor(test_set$Result),y_pred_tree)
#with(training_set,table(y_pred_tree,test_set$Result))

# predict new data
test_set$predClass = predict(model_tree, newdata=test_set, type="response")    # obtain the class (0/1)
table(test_set$predNode, test_set$Result)  # you have some cases classified as 1

#test_set$predProb = sapply(predict(model_tree, newdata=test_set,type="prob"),'[[',2)  # obtain probability of class 1 (second element from the lists)
test_set$predNode = predict(model_tree, newdata=test_set, type="node")   


#accuracy(predicted = y_pred_tree, actual = test_set$Result)



####################

romur_tree = tree(Result ~ ., data = training_set)
plot(romur_tree)
text(romur_tree, pretty = 0)
summary (romur_tree)

tree_pred = predict(romur_tree, test_set, type = "class")
confusionMatrix(tree_pred, as.factor(test_set$Result), mode = "prec_recall", positive="1")
confusionMatrix(as.factor(test_set$Result),tree_pred)
roc(test_set$Result,factor(tree_pred, ordered = TRUE), plot=TRUE,label=TRUE)

###################################
#tree randomforest  

training_set$Result <- as.character(training_set$Result)
training_set$Result <- as.factor(training_set$Result)

rf_classifier <- randomForest(Result ~ ., data = training_set, ntree= 200, importance = TRUE,
                              proximity=T)
rf_classifier

#without error
rf_classifier<-randomForest(x = training_set,
             y = training_set$Result,
             ntree = 200, na.action=na.omit)



#table(predict(rf_classifier), training_set$Result)
plot(rf_classifier)
importance(rf_classifier)

test_set$Result <- as.character(test_set$Result)
test_set$Result <- as.factor(test_set$Result)

prediction = predict(rf_classifier, newdata=test_set)
#perf <- performance(prediction,"tpr","fpr")


confusionMatrix(prediction, as.factor(test_set$Result), mode = "prec_recall", positive="1")
confusionMatrix(as.factor(test_set$Result),prediction)


roc(test_set$Result,factor(prediction,  ordered = TRUE), plot=TRUE,label=TRUE)

###############################
