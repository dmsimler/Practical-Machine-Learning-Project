# ------------------------------------------------------------------------------------------------------------
# This is the set of functions used to create predictions on the test dataset and related datasets used for 
# analysis and visualization for the candidate classifiers of a given phase (they are used by all phases 
# in this report).
# ------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------
MapClassToProbabilities <- function(predictedClassLabel, classLevels)
{
  classProbabilities <- data.frame(A = 0.0, B = 0.0, C = 0.0, D = 0.0, E = 0.0)
  classProbabilities[1, which(classLevels == predictedClassLabel)] = 1.0
  return(classProbabilities)
} # end MapClassToProbabilities


# ------------------------------------------------------------------------------------------------------------
CreateTestPredictionResults <- function(model, testDataset, labels)
{
  # Create the class levels that will be used below to determine the predicted class from the max. predicted
  # class probability for a given observation
  classeLevels <- gl(5, 1)
  levels(classeLevels) <- c("A", "B", "C", "D", "E")
  
  # First get the predictions on the test dataset
  classProbabilities <- predict(model, newdata = testDataset, type = "prob")
  
  # Some models don't return probabilities, so for those, predict the class for each example and set the 
  # predicted class's probability to 100% and everything else to 0.
  # Note: The assumption is that if NA is returned for one probability in one example, it will be NA for all
  # probabilities across all examples.
  if (sum(is.na(classProbabilities)) > 0)
  {
    classPredictions <- predict(model, newdata = testDataset, type = "raw")
    classProbabilities <- do.call(rbind, lapply(classPredictions,
                                                function(predictedClass)
                                                  MapClassToProbabilities(predictedClass,
                                                                          levels(classeLevels))))
    
  }  # end if
  
  # Build the dataset of prediction results
  obsIDs <- seq(1, nrow(testDataset))
  obsLabel <- paste(labels, "(sample", obsIDs, ")", sep = "")
  testPredictions <- data.frame(ObservationID = obsIDs,
                                Model = model$method,
                                A = classProbabilities$A,
                                B = classProbabilities$B,
                                C = classProbabilities$C,
                                D = classProbabilities$D,
                                E = classProbabilities$E,
                                ObservationLabel = obsLabel,
                                PredictedClasse = levels(classeLevels)[apply(classProbabilities, 1,
                                                                             which.is.max)],
                                ActualClasse = labels)
  
  return (testPredictions)
} # end CreateTestPredictionResults


# ------------------------------------------------------------------------------------------------------------
CreateModelTestPerfSummary <- function(modelTestPredictions)
{
  # Create a dataframe with the error rate for the given model
  error <- 1 - (sum(modelTestPredictions$PredictedClasse == modelTestPredictions$ActualClasse) /
                  nrow(modelTestPredictions))
  modelTestPerfSummary <- data.frame(Model = modelTestPredictions[1, "Model"], TestError = error)
  
  return (modelTestPerfSummary)
} # end CreateModelTestPerfSummary


# ------------------------------------------------------------------------------------------------------------
CreateTestPredictionDatasets <- function(modelList, 
                                         testDataset,
                                         currentPhaseNumber,
                                         previousPhaseTestPerfSummary = NULL, 
                                         previousPhaseNumber = -1)
{
  # Create a base dataframe with the prediction results for each test observation
  allModelsTestPredictions <- do.call(rbind, lapply(modelList, function(model) 
    CreateTestPredictionResults(model, 
                                testDataset[, -ncol(testDataset)],
                                testDataset[, ncol(testDataset)])))
  
  # Now summarize the prediction data into error estimates per model
  allModelsTestPerfsummary <- do.call(rbind, lapply(split(allModelsTestPredictions,
                                                          allModelsTestPredictions$Model),
                                                    function(modelTestPredicitons)
                                                      CreateModelTestPerfSummary(modelTestPredicitons)))
  
  # Now reorder and relevel the result dataframe in increasing order for the error
  allModelsTestPerfsummary <- allModelsTestPerfsummary[order(allModelsTestPerfsummary$TestError), ]
  allModelsTestPerfsummary$Model <- factor(allModelsTestPerfsummary$Model, 
                                           levels = allModelsTestPerfsummary$Model)
  
  # If there's a previous phase's test results to include, then add that as a new column for comparison and update the
  # friendly column names accordingly
  if (!is.null(previousPhaseTestPerfSummary))
  {
    allModelsTestPerfsummary$PreviousPhaseTestSetError <- lapply(allModelsTestPerfsummary$Model,
                                                                 function(model) 
                                                                 previousPhaseTestPerfSummary[which(previousPhaseTestPerfSummary$Model == model), 2])
    colnames(allModelsTestPerfsummary) <- c("Model", 
                                            paste("Phase ", currentPhaseNumber, "Test Set Error", sep = " "),
                                            paste("Phase ", previousPhaseNumber, "Test Set Error", sep = " "))
  } # end if
  else
  {
    # Still include more friendly names to the columns that only include the current phase's results
    colnames(allModelsTestPerfsummary) <- c("Model", paste(currentPhaseNumber, "Test Set Error", sep = " "))
  } # end else
  
  # Kill the row names
  rownames(allModelsTestPerfsummary) <- NULL
  
  # Finally, create a melted dataset that will be used to create heatmaps
  meltedTestResults <- melt(allModelsTestPredictions[, 1:8], 
                            id.vars = c("ObservationID", "Model", "ObservationLabel"),
                            variable.name = "Classe", value.name = "Probability")
  meltedTestResults <- meltedTestResults[order(meltedTestResults$ObservationID, decreasing = TRUE), ]
  meltedTestResults$ObservationLabel <- factor(meltedTestResults$ObservationLabel, 
                                               levels = meltedTestResults$ObservationLabel)
  # Kill the row names
  rownames(meltedTestResults) <- NULL
  
  
  # Now return the results in a list
  testResultsList <- list(ModelTestPredictions = allModelsTestPredictions, 
                          ModelTestPerfSummary = allModelsTestPerfsummary,
                          MeltedTestResults = meltedTestResults)
  return(testResultsList)
} # end CreateTestPredictionDatasets
