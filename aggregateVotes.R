#score all voting aggregations
#RankingVote: ranks questions based on number of YES
#MajorityVote: balance between number of YES and NO
#ThresholdVote: above a certain number of YES


scoreMajorityVote<- function(summaryTable){
  summaryTable["majorityVote"] <- summaryTable$Yes.Count - summaryTable$No.Count;
  return(summaryTable);
}

scorePositiveByNegativeVote<- function(summaryTable){
  summaryTable["positiveByNegativeVote"] <- summaryTable$Yes.Count/summaryTable$No.Count;
  return(summaryTable);
}

scoreThresholdVote<- function(summaryTable, threshold){
  summaryTable["thresholdVote"] <- summaryTable[,"Yes.Count"]>threshold;
  return(summaryTable);
}

#Provides the list of questions that have to be considered 
#and the list of questions that covers bugs
scoreRanking <- function(summaryTable){
  
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

#######################################
#Select top questions within each JavaMethod
selectTopQuestionsByJavaMethod <- function (utility_Table,
                                            sampled_dataf,
                                            questionsToSelect){
  topQuestions <- list();
  javaMethodList<- unique(sampled_dataf$FailingMethod);
  
  for(javaMethod in javaMethodList){
    javaMethod_df <- sampled_dataf[sampled_dataf$FailingMethod==javaMethod,];
    questionList <- unique(javaMethod_df$Question.ID);
    
    utilitySelection <- utility_Table[questionList,];
    
    utilityValues <- unique(utilitySelection$utility);
    sizeL<-length(utilityValues);
    if(sizeL>=questionsToSelect){
      #take only the top values
      index <- questionsToSelect;    
    }
    else{
      index <- length(utilityValues); #fewer questions than the questionsToSelect step
    }
    
    utilityValues<-utilityValues[order(utilityValues,decreasing = TRUE)];
    utilityValues <- utilityValues[1:index]; 
    
    #take the top questions by utility and sample another set answers from them
    topQuestions <- rbind(topQuestions, 
                          data.frame(utilitySelection[utilitySelection$utility>=min(utilityValues),]));
  }
  return(topQuestions);
}

