####################################################################################################
# k-fold cross validation (adaboost)
####################################################################################################
kfold.ada <- function(iteration){
  # number of cross validation
  k       <- 5
  
  # fix the seed so that result could be reproducible
  set.seed(s) 
  
  # each data has its own id(1 to 5) to process k fold cross validation 
  data$id <- sample(1:k, nrow(data), replace = TRUE)
  list    <- 1:k
  
  # data frame reset
  prediction_ada <- testset_copy_ada <- data.frame()

  #function for k fold
  for(i in 1:k){
    # divide the whole dataset into train and testset
    trainset     <- subset(data, id %in% list[-i])
    testset      <- subset(data, id %in% c(i))
    
    #run a adaboost model
    model_ada                <- basic.adaboost(trainset[, independent], trainset[, dependent], n_rounds = iteration)

    # predict
    temp_ada                 <- as.data.frame(predict(model_ada, testset))

    # append this iteration's prediction to the end of the prediction data frame
    prediction_ada           <- rbind(prediction_ada, temp_ada)

    # append this iteration's test set to the testset copy data frame
    testset_copy_ada         <- rbind(testset_copy_ada, as.data.frame(testset[, dependent]))

    # result
    result_ada               <- cbind(prediction_ada, testset_copy_ada[, 1])

    # confustion matrix and accuracy
    names(result_ada)        <- c("Actual", "Predicted")
    confusion_matrix_ada     <- table(result_ada$Actual, result_ada$Predicted)
    accuracy_ada             <- sum(diag(confusion_matrix_ada)) / sum(confusion_matrix_ada)
    result_ada               <- list("confusion_matrix_ada " = confusion_matrix_ada, 
                                     "accuracy_ada"          = accuracy_ada)
  }  
  
  # store the accuracy of each model
  acc_ada    <<- result_ada$accuracy_ada         
  
  return(acc_ada)
}

