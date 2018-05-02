#Calculate validation metrics (AUC, Precision, Recall, Sensitivity, Specificity)
#Write results into a CSV file


# Calculate errors on a provided dataset (ideally a holdout one) ----------

calculateValidationErrors <- function(fitModel,validationData){
 # browser()
#  fitModel <- fitModelList[1];
  ## Table to store the outcomes from the training and model selection and prediction
  outcome <- matrix(ncol = 12, nrow = 40);
  colnames(outcome)<- c("AUC","accuracy","trueNegatives","truePositives",
                        "falseNegatives","falsePositives","precision","recall","specificity","sensitivity");
  
  row = 0;
 # for(fitModel in fitModelList){  
    row <- row+1;
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
    outcome[row,"AUC"] <- aucValue;
    outcome[row,"accuracy"]<-accuracy;
    outcome[row,"trueNegatives"]<-trueNegatives;
    outcome[row,"truePositives"]<-truePositives;
    outcome[row,"falseNegatives"]<-falseNegatives;
    outcome[row,"falsePositives"]<-falsePositives;
    outcome[row,"precision"]<-precision;
    outcome[row,"recall"]<-recall;
    outcome[row,"sensitivity"]<-sensitivity;
    outcome[row,"specificity"]<-specificity;
    
  #}
  return(outcome);
  
}