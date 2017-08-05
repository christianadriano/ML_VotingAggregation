#compute all voting aggregations


computeMajorityVote<- function(summaryTable){
  summaryTable["MajorityVote"] <- summaryTable$Yes.Count - summaryTable$No.Count;
  return(summaryTable);
}

computeThresholdVote<- function(summaryTable, threshold){
  summaryTable["ThresholdVote"] <- summaryTable[,"Yes.Count"]>threshold;
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
    
    summaryTable[selection$Question.ID+1,"ranking"]<-selection$ranking; 
  }
  
  return(summaryTable);
}


rankQuestions <- function(selection, labels){
  
  for(i in 1:length(labels[,2])){
    matchedRows <- which(selection$Yes.Count==labels[i,1]);  
    selection[matchedRows,"ranking"]<-labels[i,2];
  }
  
  return(selection);
}