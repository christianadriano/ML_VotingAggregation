#K-nearest neighbor KNN
#Predict bug covering questions based on various values of 
#the parameters in the aggregation methods


#Obtain the data

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//photinus-analytics//sumAnswerOptionsPerQuestion.R");
summaryTable <- runMain();

install.packages("class")
library(class);

#Training set (2/3)

totalData = length(summaryTable$Question.ID);
trainingSize = trunc(totalData * 2/3);
startTestIndex = totalData - trainingSize;
endTestIndex = totalData;

###################################################################
#Scramble the dataset before extracting the training set.
set.seed(9850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];

#Extract training and test data
trainingData = as.data.frame(summaryTable[1:trainingSize,]);
testData = as.data.frame(summaryTable[startTestIndex:endTestIndex,])

##################################################################

#Select only the ranking as feature to predict bugCovering
summaryTable <- summaryTable[,c("bugCovering","ranking")];

#Alternative option
#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets
set.seed(4321);
ind<- sample(2, nrow(summaryTable),replace = TRUE, prob=c(0.67,0.33));

trainingData <- as.data.frame(summaryTable[ind==1,]);
testData <- as.data.frame(summaryTable[ind==2,]);

##Obtain the ground truth

trainingLabels <- as.data.frame(summaryTable[ind==1,"bugCovering"]);
testLabels <- as.data.frame(summaryTable[ind==2,"bugCovering"]);

##################################################################

#Build the KNN model

fitModel <- knn(train =trainingData, test=testData, cl=trainingLabels[,1] , k=2);
attributes(.Last.value)
summary(fitModel);

trainingData <-summaryTable;
trainingLabels<-data.frame(summaryTable$bugCovering);
fitModel.cv <- knn.cv (train =trainingData, cl=trainingLabels[,1], k=1, l=0, prob = FALSE, use.all=TRUE);

#Evaluate model

testLabels<-data.frame(testLabels[,1]);
merge <- data.frame(fitModel,testLabels);
names(merge)<- c("Predicted bug","Actual bug");
merge

install.packages("gmodels");
library(gmodels)

CrossTable(x = trainingData[,"bugCovering"], y=fitModel.cv, prop.chisq = FALSE)
plot(fitModel.cv)
fitModel
fitFrame <- data.frame(fitModel)

predictionFrame<-data.frame(fitModel.cv)
mean(trainingData[predictionFrame[,1]==TRUE,"ranking"])
trainingData[predictionFrame[,1]==TRUE,]
predictionFrame[predictionFrame[,1]==TRUE,]



####PLOTTING bug-covering by ranking

########## Clustering

# # Function computes N clusters and plot a chart with them
# plotKMeansClusters<- function(datafrm, numberOfClusters){
#   ## Clustering in three 
#   (cl <- kmeans(datafrm,numberOfClusters))
#   
#   datafrm$cluster=factor(cl$cluster)
#   centers=as.data.frame(cl$centers)
#   
#   #Looking at the clusters with size diameter 15.
#   ggplot(datafrm, aes(x=years_of_programming_experience, y=age, color=cluster)) +
#     geom_point(shape=1) +    # Use hollow circles
#     geom_smooth() + # Add a loess smoothed fit curve with confidence region
#     geom_point(data=centers, aes(x=years_of_programming_experience,y=age, color='Center'), size=40, alpha=.3)+
#     ggtitle("Clusters of years of experience and age")+
#     guides(color=FALSE);
#   
# }
# 
# #What happens if we divide workers for 5 clusters, which is the number of professions?
# plotKMeansClusters(5)
