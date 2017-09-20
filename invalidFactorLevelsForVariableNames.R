# StackOverflow questoin on the following Error:

#Question: How do I predict a logical variable (TRUE,FALSE) using the R caret?

# Error: At least one of the class levels is not a valid R variable name; This will cause errors when 
# class probabilities are generated because the variables names will be converted to  FALSE., TRUE. . 
# Please use factor levels that can be used as valid R variable names  (see ?make.names for help).

##Platform: RStudio, caret package

## Quick and dirty solution
# I just applied the function make.names, which worked by transforming my logical variable into
# a String (e.g., "False." and "TRUE."). I would like to know if there are any other 
#more elegant alternatives that would allow me to preserve my logical variable.

#Source code
#Explanatory variable = ranking value from 1 to 10
#Prediction variable = TRUE or FALSE
#I want to predict the minimal ranking level for Selected==TRUE
library(caret)
myData <- data.frame(c(4,3,5,3,9,2,1,7,10,2,8,4,1,3,5), 
                     make.names(c(FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)));
colnames(myData)<-c("Ranking","Selected");

myFolds <- createFolds(myData$Ranking, k=5);
set.seed(1234);
myControl<-trainControl(index=myFolds, classProbs=TRUE, summaryFunction=twoClassSummary);

knnModel<-train(Selected~Ranking, myData,method="knn", trControl=myControl);
rfModel<-train(Selected~Ranking, myData,method="rf", trControl=myControl);
glmModel<-train(Selected~Ranking, myData,method="glm", trControl=myControl);
svmModel<-train(Selected~Ranking, myData,method="svmRadial", trControl=myControl);

glmModel
rfModel
knnModel
svmModel

#Visualize models
resampleList<-resamples(list(rf=rfModel,glm=glmModel,knn=knnModel, svm=svmModel))

bwplot(resampleList,metric="ROC")
densityplot(resampleList,metric="ROC")
dotplot(resampleList,xlim=range(0,1),metric="ROC")
xyplot(resampleList,xlim=range(0,1), metric="ROC")

compareTable <- data.frame(myData$Ranking,
                           myData$Selected,
                           predict(glmModel,myData),
                           predict(knnModel,myData),
                           predict(rfModel,myData)
);
colnames(compareTable) <- c("ranking","actual","glm","knn","rf");
compareTable

