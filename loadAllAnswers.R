#Load ALL Answers into a dataframe
#NO FILTERING of WORKERS

loadAnswers<- function(fileName){
  
  setwd("C://Users//Chris//Documents//GitHub//ML_VotingAggregation//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf = data.frame(data_all);
  #summary(dataf)
  #First I need to look at the outliers or invalid values
  #Invalid age and invalid years of experience
  
  #Remove rows with empty data
  #dataf <- dataf [!(dataf$Worker.age==0),];
  
  #Remove confidence level zero (because it is associated with IDK answers)
  #dataf <- dataf [!(dataf$Answer.confidence==0) ,];
  #dataf <- dataf [!(dataf$Answer.confidence==3) ,];
  
  #Remove NO AND IDK ANSWERS
 #dataf <- dataf [!(dataf$Answer.option=="IDK") ,];
#  dataf <- dataf [!(dataf$Answer.option=="YES") ,];
  
  
  #Remove invalid values
  #dataf <- dataf [!dataf$Worker.age <1,];
  #dataf <- dataf [!dataf$Worker.yearsOfExperience <1,];
  
  #Outliers
  #Assuming that the youngest age to start programming is 10 years old
  #Remove inputs for which age-YoE<5 
  #dataf <- removeLinesColDiffSmallerThanValue(dataf,7,5,10)
  #summary (dataf);
  return(dataf);
}
