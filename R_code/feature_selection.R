library(caTools)
library(e1071)
library(superml)
library(caret)
library(data.table)
library(dplyr)
library(corrplot)
library(readxl)
library(randomForest)
library(ggcorrplot)
library(corrr)
library(mlbench)
library(DataExplorer)
#library(contextual)
dataset=read_excel('')

#balanced data file 
#dataset = read_excel('')

#dataframe structure
str(dataset) 
#dataframe summary
summary(dataset)
lapply(dataset, FUN=summary)

#Data exploring
plot_intro(dataset)
plot_bar(dataset)
plot_correlation(dataset)

#######?######################

result_count <- dataset %>% group_by(Result) %>% summarise(Total_of_results= n())
barplot(result_count$Total_of_results)

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
dataset$Favorite_Statu <- round(as.numeric(dataset$Favorite_Statu)) # ?ound off the column to integer

sapply(dataset, class)

#########################
#feature importance 
#correlation 
#correlation 
rumor_cor <-cor(rumor)

corrplot(rumor_cor, type="upper", order="hclust")

ggcorrplot(rumor_cor, hc.order = FALSE, lab = TRUE , lab_size = 3, pch = 4,
           pch.col = "black",pch.cex = 5, tl.cex = 10, tl.col = "black", 
           tl.srt = 45,digits = 2)


# feature that are highly correlated with each othe
for(i in 1:ncol(rumor))
{
  for(j in 1:ncol(rumor))
  { 
    corr_=cor(rumor[i], rumor[j], method = "spearman")
    if ((cor(rumor[i], rumor[j], method = "spearman"))> 0.7 &(cor(rumor[i], rumor[j], method = "spearman"))<0.9 )
    {
      print(corr_)
      print (colnames(rumor)[i])
      print( colnames(rumor)[j])
        }     
    }
}

#another approach
# calculate correlation matrix
correlationMatrix <- cor(dataset[,1:24])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrel?tion(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#############
#Rank Features By Importance
control <- trainControl(method="repeatedcv", number=10, repeats=3)
dataset$Result <- as.factor(dataset$Result)
# train the model
model <- train(Result~., data=dataset, method="lvq", preProcess="scale", trControl=control,na.action=na.exclude)
#variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


##############
#Recursive Feature Elimination 
#defining the control using a random forest selection function
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# 
# # run the RFE algorithm
# results <- rfe(dataset[,1:24], as.matrix(dataset?,25]), sizes=c(1:24), rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))


