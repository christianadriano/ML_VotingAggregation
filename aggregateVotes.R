#compute all voting aggregations
#RankingVote: ranks questions based on number of YES
#MajorityVote: balance between number of YES and NO
#ThresholdVote: above a certain number of YES


computeMajorityVote<- function(summaryTable){
  summaryTable["majorityVote"] <- summaryTable$Yes.Count - summaryTable$No.Count;
  return(summaryTable);
}

computeThresholdVote<- function(summaryTable, threshold){
  summaryTable["thresholdVote"] <- summaryTable[,"Yes.Count"]>threshold;
  return(summaryTable);
}

#Provides the list of questions that have to be considered 
#and the list of questions that covers bugs
computeRanking <- function(summaryTable){
  
  for(i in 1:8){
    selection <- summaryTable[summaryTable$JavaMethod==i,];
    
    #Sort 
    selection <- selection[with(selection,order(-Yes.Count)),];
    #remove duplicates
    uniqueLevels <- unique(selection$Yes.Count)
    labels<-matrix(NA,length(uniqueLevels),2);
    
    labels[,1]<- uniqueLevels;
    labels[,2]<- c(1:length(uniqueLevels));
    
    selection <- rankQuestions(selection,labels);
    
    summaryTable[selection$Question.ID+1,"rankingVote"]<-selection$rankingVote; 
  }
  
  return(summaryTable);
}

#rank questions based on nmber of YES's
rankQuestions <- function(selection, labels){
  
  for(i in 1:length(labels[,2])){
    matchedRows <- which(selection$Yes.Count==labels[i,1]);  
    selection[matchedRows,"rankingVote"]<-labels[i,2];
  }
  return(selection);
}


