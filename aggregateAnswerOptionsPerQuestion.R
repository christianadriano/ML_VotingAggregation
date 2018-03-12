

library(dplyr);

#Auxiliary functions

##by_questions<- group_by(subset_df,Question.ID) ;
countAnswerOptions<- function(dataf){
  subset_df <-subset(dataf,select= c(Answer.option));
  subset_df <- data.frame(subset_df); 
  
  #mark all rows that match the selection
  YesCount<- rowSums(subset_df=="YES"); 
  subset_df["YesCount"] <- YesCount;
  NoCount<- rowSums(subset_df=="NO"); 
  subset_df["NoCount"] <- NoCount;
  IDKCount<- rowSums(subset_df=="IDK"); 
  subset_df["IDKCount"] <- IDKCount;
  
  subset_df["QuestionID"] <- dataf$Question.ID;
  
  subset_df <-subset(subset_df,select= c(QuestionID,YesCount,NoCount,IDKCount));
  
  ##Group by question ID, which means adding all the values from the parallel columns in order to have
  #a sum for each questionID
  
  question_by <- group_by(subset_df,QuestionID);
  summaryTable<- summarize(question_by,TotalYes = sum(YesCount),TotalNo = sum(NoCount), TotalIDK = sum(IDKCount));
  
  colnames(summaryTable)<-c("Question.ID","Yes.Count","No.Count","IDK.Count");
  
  return(summaryTable);
}


#Add a column with the information of whether the question.ID 
#covers a bug of not.
appendGroundTruth<- function(summaryTable, questionList){
  list <- summaryTable[,"Question.ID"];
  summaryTable[,"bugCovering"] <- list$Question.ID %in% questionList;
  return(summaryTable);  
}


setJavaMethodID<- function(summaryTable){
  
  summaryTable<- setRangeID(summaryTable,1,c(0:9));
  summaryTable<- setRangeID(summaryTable,2,c(10:15));
  summaryTable<- setRangeID(summaryTable,3,c(16:32));
  summaryTable<- setRangeID(summaryTable,4,c(33:69));
  summaryTable<- setRangeID(summaryTable,5,c(70:78));
  summaryTable<- setRangeID(summaryTable,6,c(79:96));
  summaryTable<- setRangeID(summaryTable,7,c(97:104));
  summaryTable<- setRangeID(summaryTable,8,c(105:128));
  
  return (summaryTable);
}

setRangeID<-function(summaryTable,id, range){
  
  for(i in 1:length(range)){
    matchedRows <- which(summaryTable$Question.ID == range[i]);  
    summaryTable[matchedRows,"JavaMethod"]<-id;
  }
  return(summaryTable);
}



#Select only the rows that match the questionIDlist
selectRows <-function(summaryTable,questionIDList){
  selectedRows <- summaryTable[summaryTable$Question.ID %in% questionIDList,];
  return(selectedRows);
}

######## Main code

runMain<-function(){
  
  # Load vote aggregation methods
  source("C://Users//Chris//Documents//GitHub//ML_VotingAggregation//aggregateVotes.R");
  
  # Import data
  source("C://Users//Chris//Documents//GitHub//ML_VotingAggregation//loadAllAnswers.R");
  dataf <- loadAnswers("answerList_data.csv");
  
  # Initialize Java method questions and bug covering data
  questionList <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
  
  
  summaryTable <- countAnswerOptions(dataf);
  summaryTable <- appendGroundTruth(summaryTable,questionList);
  summaryTable<- setJavaMethodID(summaryTable);
  summaryTable <- computeMajorityVote(summaryTable);
  summaryTable <- computeThresholdVote(summaryTable,6);
  summaryTable<- computeRanking(summaryTable);
  
  return (summaryTable);
}


runFromSample<-function(sampledDF){
  # Load vote aggregation methods
  source("C://Users//Chris//Documents//GitHub//ML_VotingAggregation//aggregateVotes.R");
  
  # Initialize Java method questions and bug covering data
  questionList <- c(1,4,10,14,20,23,30,32,55,56,57,58,59,72,73,77,84,92,95,97,102,104,115,119,123);
  
  summaryTable <- countAnswerOptions(sampledDF);
  summaryTable <- appendGroundTruth(summaryTable,questionList);
  summaryTable<- setJavaMethodID(summaryTable);
  summaryTable <- computeMajorityVote(summaryTable);
  summaryTable <- computeThresholdVote(summaryTable,6);
  summaryTable<- computeRanking(summaryTable);
  #summaryTable<- computePredicedbyRanking(summaryTable);
  
  return (summaryTable);
}




