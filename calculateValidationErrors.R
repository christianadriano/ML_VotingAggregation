#Calculate validation metrics (AUC, Precision, Recall, Sensitivity, Specificity)
#Write results into a CSV file


# Calculate errors on a provided dataset (ideally a holdout one) ----------

calculateValidationErrors <- function(fitModel,modelName,validationData){
  
  outcome <- matrix(ncol = 11, nrow = 1);
  colnames(outcome)<- c("ModelName","AUC","accuracy","trueNegatives","truePositives",
                                   "falseNegatives","falsePositives","precision","recall","specificity","sensitivity");
  
  bugCoveringPredicted <- predict(fitModel,newdata = validationData);
  
  matrixResult<- confusionMatrix(data=bugCoveringPredicted,validationData$bugCoveringLabels, positive="T");
  trueNegatives<- matrixResult$table[1,1];
  truePositives<- matrixResult$table[2,2];
  falseNegatives<- matrixResult$table[1,2];
  falsePositives<- matrixResult$table[2,1];
  
  #calculate AUC
  aucValue<- roc(response = as.numeric(validationData$bugCoveringLabels),
                 predictor = as.numeric(bugCoveringPredicted)) %>% auc();
  
  aucValue<-as.numeric(aucValue);
  
  accuracy <- (truePositives + trueNegatives) / (truePositives + trueNegatives + falsePositives + falseNegatives);
  trainingError <- 1-accuracy;
  precision <- truePositives / (truePositives + falsePositives);
  recall <- truePositives / (truePositives + falseNegatives);
  specificity <- trueNegatives / (trueNegatives + falsePositives);
  sensitivity <- truePositives / (truePositives + falseNegatives);
  
  #create output table
  outcome[1,"ModelName"] <- modelName;
  outcome[1,"AUC"] <- aucValue;
  outcome[1,"accuracy"]<-accuracy;
  outcome[1,"trueNegatives"]<-trueNegatives;
  outcome[1,"truePositives"]<-truePositives;
  outcome[1,"falseNegatives"]<-falseNegatives;
  outcome[1,"falsePositives"]<-falsePositives;
  outcome[1,"precision"]<-precision;
  outcome[1,"recall"]<-recall;
  outcome[1,"sensitivity"]<-sensitivity;
  outcome[1,"specificity"]<-specificity;
  
  return(outcome);
  
}