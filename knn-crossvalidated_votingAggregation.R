#K-Nearest Neighbor KNN
#Predict bug covering questions based on various values of 
#the parameters in the aggregation methods

install.packages("class");
library(class);
install.packages("gmodels");
library(gmodels)
install.packages("caret")
library(caret)
install.packages('e1071', dependencies=TRUE)
library(e1071)

#Obtain the data

# Import data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_VotingAggregation//aggregateAnswerOptionsPerQuestion.R");
summaryTable <- runMain();

#I need to guarantee that some examples (i.e., failing methods)
#do not dominate the training or testing sets. To do that, I need to get a 
#close to equal proportion of examples in both sets

#Scramble the dataset before extracting the training set.
set.seed(9850);
g<- runif((nrow(summaryTable))); #generates a random distribution
summaryTable <- summaryTable[order(g),];
#head(summaryTable)

##################################################################
#Build the KNN model

#Select only the ranking as a feature to predict bugCovering
#trainingData <- summaryTable[,c("bugCovering","Yes.Count")];
trainingData <- summaryTable[,c("bugCovering","rankingVote")];


#Prepare explanatory variable (rankingVote) and target (bugCovering)
#trainingData <-data.frame(summaryTable);
trainingData$rankingVote <- as.numeric(trainingData$rankingVote);


######################################################################################
#Using KNN from CLASS package

fitModel.cv <- knn.cv (train =trainingData, cl=trainingData$bugCovering, k=3, l=0, prob = FALSE, use.all=TRUE);

#Evaluate model


fitModel.cv.df<-data.frame(fitModel.cv)
CrossTable(x = trainingData$bugCovering, y=fitModel.cv.df[,1], prop.chisq = FALSE)

plot(fitModel.cv)

trainingData$bugCovering <- as.factor(trainingData$bugCovering);
predictedBugCoveringList<-trainingData[fitModel.cv.df[,1]==TRUE,];
predictedList <- as.numeric(unlist(predictedBugCoveringList[,2]));
mean(predictedList)
min(predictedList)
max(predictedList)

#Plot metric distribution
predictedList.df <- data.frame(predictedList);
colnames(predictedList.df)<- c("votes");

ggplot(data=predictedList.df, aes(x=predictedList.df$votes)) +
  geom_histogram(binwidth = .5,alpha=.5, position="identity")+
  geom_vline(aes(xintercept=mean(predictedList.df$votes, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("Distribution of votes for the questions categorized as bug covering")+
  labs(x="Threshold vote values of questions categorized as bug-covering. Mininal vote=6, mean=9.41", 
       y="Frequency");


###################################################################################### 
### Using KNN from CARET package
## https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
## https://dataaspirant.com/2017/01/09/knn-implementation-r-using-caret-package/


# I will do 5 repeats of 10-Fold CV. I will fit
# a KNN model that evaluates 10 values of k

set.seed(1234)

trctrl <- trainControl(method = "repeatedcv", number=10, p=0.9, repeats = 5)

trainingData$rankingVote <- as.numeric(trainingData$rankingVote);
trainingData$bugCovering <- as.factor(trainingData$bugCovering);
mean(trainingData$rankingVote);

knn_fit <- train(bugCovering ~ rankingVote, data = trainingData, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

bugCoveringPredicted <- predict(knn_fit,newdata = trainingData);

confusionMatrix(data=bugCoveringPredicted,trainingData$bugCovering)
#False Positives = 10
#False Negatives = 6
#True Positives = 15
#True Negatives = 98

df<-data.frame(bugCoveringPredicted);
predictedBugCoveringList<-trainingData[df[,1]==TRUE,];
rankingList <- as.numeric(unlist(predictedBugCoveringList[,2]));
predictedBugCoveringList[,1]
mean(rankingList)
max(rankingList)
min(rankingList)
hist(rankingList,main="Bug-covering ranking dist., knn caret repeatedcv, mean=1.52, max=2",xlab="ranking");

#Caret produced more false positives than knn.cv from class package. I tried to fine tune it more,
#but was not enough. The k value for Caret seems very high 15 t 23.

###########################################################################################