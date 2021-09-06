
#Features Importance and selection 

# load the library
library(mlbench)
library(caret)
#library(xlsx)
library(readxl)
library(ggcorrplot)
library(corrplot)
library(corrr)

# load the rumor data
rumor <- read_excel('C:/Users/admin/Desktop/Rehab1-Final Data Set.xlsx')

#columns renaming
names(rumor)[names(rumor) == "Followers/Friends"] <- "Followers_Friends"
names(rumor)[names(rumor) == "Favorite/Statu"] <- "Favorite_Statu"

################
#dummying the variables 

# library(magrittr)
# rumor %<>% mutate_if(is.logical,as.numeric) 

Cols <-  which(sapply(rumor, is.logical))
setDT(rumor)

for(j in Cols){
  set(rumor, i=NULL, j=j, value= as.numeric(rumor[[j]]))
}


label <- LabelEncoder$new()
rumor$Result <- label$fit_transform(rumor$Result)
rumor$Verified <- label$fit_transform(rumor$Verified)
rumor$Favorited <- label$fit_transform(rumor$Favorited)
rumor$Retweeted <- label$fit_transform(rumor$Retweeted)
rumor$Protected <- label$fit_transform(rumor$Protected)
rumor %>% mutate_if(is.numeric, round,digits=1)
#rumor$Followers_Friends <-str(rumor$Followers_Friends) 
#rumor$Followers_Friends  <- lapply(rumor$Followers_Friends , as.numeric)
#rumor$Favorite_Statu  <- lapply(rumor$Favorite_Statu , as.numeric)
rumor$Followers_Friends <- round(as.numeric(rumor$Followers_Friends)) # Round off the column to integer
rumor$Favorite_Statu <- round(as.numeric(rumor$Favorite_Statu))

########################################
#first approach
rumor$Result <- as.factor(rumor$Result)
# Split as data
data <- rumor[,1:24]
target <- rumor$Result
# Train the model (random forest)
model <- train(data, target, method="rf", preProcess="scale", trControl=trainControl(method = "cv"))
# Compute variable importance
importance <- varImp(model)
# summarize importance
print(importance)
# plot importance
plot(importance)

summary(model)

####################
#second approach
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model1 <- train(target~., data=data, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance1 <- varImp(model1, scale=FALSE)
# summarize importance
print(importance1)
# plot importance
plot(importance1)
summary (model1)

#########################
#correlation 
rumor_cor <-cor(rumor)
corrplot(rumor_cor, type="upper", order="hclust")

ggcorrplot(rumor_cor, hc.order = FALSE, lab = TRUE, lab_size = 3, pch = 4,
           pch.col = "black",pch.cex = 5, tl.cex = 10, tl.col = "black", 
           tl.srt = 45,digits = 2)


# x=rumor %>% 
#   correlate() %>%
#   rearrange() %>%  # rearrange by correlations
#   shave() 


# for(i in 1:ncol(rumor))
# {
#  # print(i)
#   for(j in 1:ncol(rumor))
#   { 
#     corr_=cor(rumor[i], rumor[j], method = "spearman")
#     #print(corr_)
#     if ((cor(rumor[i], rumor[j], method = "spearman"))> 0.7 &(cor(rumor[i], rumor[j], method = "spearman"))<0.9 )
#    #  if ((cor(rumor[i], rumor[j], method = "spearman"))>= 0.7 )
#    # # (((cor(rumor[i], rumor[j], method = "spearman"))> 0.7 &(cor(rumor[i], rumor[j], method = "spearman"))<=0.9 ))
#    #      
#    #    {
#         print(corr_)
#         print (colnames(rumor)[i])
#         print( colnames(rumor)[j])
#    #   }
#      
#   }
# }

for(i in 1:ncol(rumor))
{
  # print(i)
  for(j in 1:ncol(rumor))
  { 
    corr_=cor(rumor[i], rumor[j], method = "spearman")
    #print(corr_)
    if ((cor(rumor[i], rumor[j], method = "spearman"))> 0.7 &(cor(rumor[i], rumor[j], method = "spearman"))<0.9 )
          {
            print(corr_)
            print (colnames(rumor)[i])
            print( colnames(rumor)[j])
    #   }
    
  }
}}



