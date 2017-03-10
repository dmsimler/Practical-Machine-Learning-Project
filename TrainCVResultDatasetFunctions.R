# ---------------------------------------------------------------------------------------------------
# This section provides functions that create datasets from the results of the model 
# training/tuning/cross-validation performed above.  These datasets will be used for analysis and 
# visualizations performed later in this document (for all phases) 
# ---------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------
# First, we need a function to build a dataframe holding all the CV errors for all 
# the models created above:
# ---------------------------------------------------------------------------------------------------
CreateModelsCVErrorDataset <- function(modelList)
{
  allModelsOOSErrors <- NULL
  for(model in modelList)
  {
    currentModelOOSErrors <- data.frame(Model = model$method, FoldName = model$resample$Resample,
                                        CVError = (1 - model$resample$Accuracy), 
                                        stringsAsFactors = FALSE)
    
    if (is.null(allModelsOOSErrors)) 
    {
      allModelsOOSErrors <- currentModelOOSErrors
    } # end if
    else
    {
      allModelsOOSErrors <- rbind(allModelsOOSErrors, currentModelOOSErrors, stringsAsFactors = FALSE)
    } # end else
  } # end for
  
  # Relevel the model factor to be in ascending order according to the mean of its OOSErrors
  OOSMeans <- aggregate(CVError ~ Model, data = allModelsOOSErrors, FUN = "mean")
  OOSMeans <- OOSMeans[order(OOSMeans$CVError, decreasing = TRUE), ]
  allModelsOOSErrors$Model <- factor(allModelsOOSErrors$Model, levels = OOSMeans$Model)
  
  return (allModelsOOSErrors)
} # end CreateModelsCVErrorDataset


# ---------------------------------------------------------------------------------------------------
# Now create a function that builds a dataframe summarizing the error estimation and hyper-parameter
# values for the final model chosen (by caret) for each model type trained.
# ---------------------------------------------------------------------------------------------------
CreateModelTuningSummaries <- function(modelList)
{
  allModelSummaries <- NULL
  
  for (model in modelList)
  {
    # First create a string with all of the final model tuning parameter values for the current
    # model type
    currentModelTuneParamList <- ""
    
    for (paramIndex in 1:ncol(model$bestTune))
    {
      paramVal <- model$bestTune[1, paramIndex]
      paramName <- colnames(model$bestTune)[paramIndex]
      
      if(is.numeric(paramVal))
      {
        # Only format as a decimal if there is a decimal portion
        tuneParam <- ifelse((round(paramVal) != paramVal), 
                            sprintf("%s = %f ", paramName, paramVal),
                            paste(paramName," = ", paramVal, " ", sep = ""))
      } # end if
      else
      {
        tuneParam <- paste(paramName," = ", paramVal, " ", sep = "")
      } # end else
      
      currentModelTuneParamList <- paste(currentModelTuneParamList, tuneParam, sep = "")
    } # end for
    
    # Create a row summarizing the performance and tuning of the current model
    currentModelSummary <- data.frame(Model = model$method, 
                                      CVError = 1 - max(model$results$Accuracy),
                                      TotalTrainingTime = model$times$everything[3],
                                      TuningParameters = currentModelTuneParamList,
                                      stringsAsFactors = FALSE)
    
    # Now add the current model summary to the overall model summary dataset
    if (is.null(allModelSummaries))
    {
      allModelSummaries <- currentModelSummary
    } # end if
    else
    {
      allModelSummaries <- rbind(allModelSummaries, currentModelSummary, stringsAsFactors = FALSE)
    } #end else
  } # end for
  
  # Sort the summary dataset by CV error in increasing order
  allModelSummaries <- allModelSummaries[order(allModelSummaries$CVError), ]
  
  # Kill the row names
  rownames(allModelSummaries) <- NULL
  
  # Now give more friendly names to the columns for plotting
  colnames(allModelSummaries) <- c("Model", "CV Error", "Training Time (secs)",
                                   "Final Tuning Parameters")
  
  # Now returns the results
  return (allModelSummaries)
} # end CreateModelTuningSummaries

# --------------------------------------------------------------------------------------------------
# Now create a function that builds a dataframe with the difference in error rates between models,
# comparing two at a time.  This will be used to calculate statistics needed for model selection 
# later in this document.
# --------------------------------------------------------------------------------------------------
CreateModelDiffsDataset <- function(modelsOOSErrors, modelComparisonsToMake, comparisonLevelOrder)
{
  allComparisons <- NULL
  for(comparisonIndex in 1:nrow(modelComparisonsToMake))
  {
    # Grab the out-of-sample error data for each model
    model1Name <- modelComparisonsToMake[comparisonIndex, 1]
    model2Name <- modelComparisonsToMake[comparisonIndex, 2]
    diffName <- paste(model1Name, " - ", model2Name, sep = "")
    model1Errors <- subset(modelsOOSErrors, modelsOOSErrors$Model == model1Name)
    model2Errors <- subset(modelsOOSErrors, modelsOOSErrors$Model == model2Name)
    
    # Now sort each vector by the fold name, so we're subtracting measurements for the same folds
    model1Errors <- model1Errors[order(model1Errors$FoldName), ]
    model2Errors <- model2Errors[order(model2Errors$FoldName), ]
    errorDiff = model1Errors$CVError - model2Errors$CVError
    
    # Compile the results into a temporary dataframe
    currentComparison <- data.frame(Model1 = model1Name, 
                                    Model2 = model2Name,
                                    DiffName = diffName,
                                    OOSErrorDiff = errorDiff,
                                    FoldName = model1Errors$FoldName,
                                    stringsAsFactors = FALSE)
    
    # Now add the current comparison data to the larger dataset
    if (is.null(allComparisons))
    {
      allComparisons <- currentComparison
    } # end if
    else
    {
      allComparisons <- rbind(allComparisons, currentComparison, stringsAsFactors = FALSE)
    } # end else
  } # end for
  
  # Relevel the combined diff name according to the order specified by the caller
  allComparisons$DiffName <- factor(allComparisons$DiffName, levels = comparisonLevelOrder)
  allComparisons <- allComparisons[order(allComparisons$DiffName, decreasing = TRUE), ]
  
  return (allComparisons)
} # end CreateModelDiffsDataset


# --------------------------------------------------------------------------------------------------
# Function that compares the means of the cross-validation errors of two two different models using 
# the student t-test and returns the results in a dataframe.
# --------------------------------------------------------------------------------------------------
ComparePerfOfTwoModels <- function(model1Errors, model2Errors, model1Name, model2Name, significanceLevel)
{
  # Perform a t-test to compare the cross-validation errors of two models
  confidenceLevel <- 1 - significanceLevel
  tResult <- t.test(x = model1Errors$CVError, 
                    y = model2Errors$CVError, 
                    alternative = "less", 
                    conf.level = confidenceLevel)
  diffName = paste(model1Name, " - ", model2Name, sep="")
  oosErrorMeanDiff <- tResult$estimate[1] - tResult$estimate[2]
  
  # Capture the t-test results and return it in a dataframe
  modelComparison <- data.frame(Model1 = model1Name, 
                                Model2 = model2Name, 
                                DiffName = diffName,
                                OOSErrorMeanDiff = oosErrorMeanDiff, 
                                P.Value = tResult$p.value,
                                T.Stat = tResult$statistic, 
                                RejectH0 = (tResult$p.value < significanceLevel),
                                stringsAsFactors = FALSE)
  
  return (modelComparison)
} # end ComparePerfOfTwoModels


# -----------------------------------------------------------------------------------------------------------
# Now create a function that builds a dataframe with the difference in error rates between the same models
# between Phase 1 and Phase 2, comparing two at a time.  This will be used to calculate statistics needed for
# model selection later in this document.
# -----------------------------------------------------------------------------------------------------------
CreatePhaseDiffDataset <- function(modelsCVErrorsCurrentPhase, 
                                   modelsCVErrorsPreviousPhase, 
                                   modelsToCompare, 
                                   currentPhaseNum,
                                   previousPhaseNum,
                                   comparisonLevelOrder)
{
  allComparisons <- NULL
  for(comparisonIndex in 1:length(modelsToCompare))
  {
    # Grab the cross-validation error data for the current model from each phase
    modelName <- modelsToCompare[comparisonIndex]
    modelCurrentPhaseErrors <- subset(modelsCVErrorsCurrentPhase, Model == modelName)
    modelPreviousPhaseErrors <- subset(modelsCVErrorsPreviousPhase, Model == modelName)
    currentDiffPreviousName <- paste(modelName, currentPhaseNum, " - ", modelName, previousPhaseNum, sep = "")
    previousDiffCurrentName <- paste(modelName, previousPhaseNum, " - ", modelName, currentPhaseNum, sep = "")
    
    # Now sort each vector by the fold name, so we're subtracting measurements for the same folds
    modelCurrentPhaseErrors <- modelCurrentPhaseErrors[order(modelCurrentPhaseErrors$FoldName), ]
    modelPreviousPhaseErrors <- modelPreviousPhaseErrors[order(modelPreviousPhaseErrors$FoldName), ]
    
    # Now take two error differences: Current Phase - Previous Phase, Previous Phase - Current Phase
    currentDiffPreviousError = modelCurrentPhaseErrors$CVError - modelPreviousPhaseErrors$CVError
    previousDiffCurrentError = modelPreviousPhaseErrors$CVError - modelCurrentPhaseErrors$CVError
    
    # Compile the current to previous phase comparison results for this model type into a temporary dataframe
    currentToPreviousComparison <- data.frame(Model1 = paste(modelName, currentPhaseNum, sep=""), 
                                              Model2 = paste(modelName, previousPhaseNum, sep=""),
                                              DiffName = currentDiffPreviousName,
                                              CVErrorDiff = currentDiffPreviousError,
                                              FoldName = modelCurrentPhaseErrors$FoldName, # doesn't matter which model we pick
                                              ComparesCurrentToPrevious = TRUE,
                                              stringsAsFactors = FALSE)
    
    # Now do the same for previous to current phase comparison results for this model type into a temporary dataframe
    previousToCurrentComparison <- data.frame(Model1 = paste(modelName, previousPhaseNum, sep=""), 
                                              Model2 = paste(modelName, currentPhaseNum, sep=""),
                                              DiffName = previousDiffCurrentName,
                                              CVErrorDiff = previousDiffCurrentError,
                                              FoldName = modelPreviousPhaseErrors$FoldName, # doesn't matter which model we pick
                                              ComparesCurrentToPrevious = FALSE,
                                              stringsAsFactors = FALSE)
    
    
    # Add the current to previous comparison data to the larger dataset
    if (is.null(allComparisons))
    {
      allComparisons <- currentToPreviousComparison
    } # end if
    else
    {
      allComparisons <- rbind(allComparisons, currentToPreviousComparison, stringsAsFactors = FALSE)
    } # end else
    
    # Now do the same for the previous to current comparison data
    allComparisons <- rbind(allComparisons, previousToCurrentComparison, stringsAsFactors = FALSE)
    
  } # end for
  
  # Relevel the combined diff name according to the order specified by the caller
  allComparisons$DiffName <- factor(allComparisons$DiffName, levels = comparisonLevelOrder)
  allComparisons <- allComparisons[order(allComparisons$DiffName), ]
  
  return (allComparisons)
} # end CreatePhaseDiffDataset


# --------------------------------------------------------------------------------------------------
# Function that orchestrates the creation of a dataset with the results of model pair-wise t-test
# comparison of the means of the respective cross-validation errors. 
# --------------------------------------------------------------------------------------------------
CreatePhaseComparisonsDataset <- function(modelsCVErrorsCurrentPhase, 
                                          modelsCVErrorsPreviousPhase, 
                                          modelsToCompare, 
                                          currentPhaseNum,
                                          previousPhaseNum,
                                          comparisonLevelOrder)
{
  # Now traverse each current to previous phase comparison for a given model, perform a t-test on the data, and 
  # add the results to a temporary dataset
  significanceLevel <- 0.05 / (2 * length(modelsToCompare))
  currentToPreviousComps <- do.call(rbind, lapply(modelsToCompare,  
                                           function(model)  
                                           ComparePerfOfTwoModels(modelsCVErrorsCurrentPhase[modelsCVErrorsCurrentPhase$Model == model, ],
                                                                  modelsCVErrorsPreviousPhase[modelsCVErrorsPreviousPhase$Model == model, ],
                                                                  paste(model, currentPhaseNum, sep = ""), 
                                                                  paste(model, previousPhaseNum, sep = ""), 
                                                                  significanceLevel)))
  currentToPreviousComps$ComparesCurrentToPrevious <- TRUE
  
  # Now do the same but for previous to current phase model comparisons
  significanceLevel <- 0.05 / (2 * length(modelsToCompare))
  previousToCurrentComps <- do.call(rbind, lapply(modelsToCompare,  
                                                  function(model)  
                                                    ComparePerfOfTwoModels(modelsCVErrorsPreviousPhase[modelsCVErrorsPreviousPhase$Model == model, ],
                                                                           modelsCVErrorsCurrentPhase[modelsCVErrorsCurrentPhase$Model == model, ],
                                                                           paste(model, previousPhaseNum, sep = ""), 
                                                                           paste(model, currentPhaseNum, sep = ""), 
                                                                           significanceLevel)))
  previousToCurrentComps$ComparesCurrentToPrevious <- FALSE
  
  # Now combine the two comparison datasets
  allModelComps <- rbind(currentToPreviousComps, previousToCurrentComps, stringsAsFactors = FALSE)
    
  # Relevel the DiffName factor to be in ascending order according to the mean of its diff OOSErrors
  allModelComps$DiffName <- factor(allModelComps$DiffName, levels = comparisonLevelOrder)
  allModelComps <- allModelComps[order(allModelComps$DiffName), ]
  
  # Now give more friendly names to the columns for plotting
  colnames(allModelComps) <- c("Model 1", "Model 2", "DiffName", "Diff Error Mean", "p-value",
                               "t-statistic", "Reject $H_{0}$", "ComparesCurrentToPrevious")
  rownames(allModelComps) <- NULL
  
  return (allModelComps)
} # end CreatePhaseComparisonsDataset


# --------------------------------------------------------------------------------------------------
# Function that orchestrates the creation of a dataset with the results of model pair-wise t-test
# comparison of the means of the respective cross-validation errors. 
# --------------------------------------------------------------------------------------------------
CreateModelComparisonsDataset <- function(modelsOOSErrors, modelComparisonsToMake, comparisonLevelOrder)
{
  # Now traverse each comparison, perform a t-test on the data, and add the results to a
  # cumulative dataset
  significanceLevel <- 0.05 / nrow(modelComparisonsToMake)
  allModelComps <- do.call(rbind, apply(modelComparisonsToMake, 1, 
                                        function(modelPair)  
                                          ComparePerfOfTwoModels(modelsOOSErrors[modelsOOSErrors$Model == modelPair[1], ],
                                                                 modelsOOSErrors[modelsOOSErrors$Model == modelPair[2], ],
                                                                 modelPair[1], modelPair[2], significanceLevel)))
  
  # Relevel the DiffName factor to be in ascending order according to the mean of its diff OOSErrors
  allModelComps$DiffName <- factor(allModelComps$DiffName, levels = comparisonLevelOrder)
  allModelComps <- allModelComps[order(allModelComps$DiffName), ]
  
  # Now give more friendly names to the columns for plotting later
  colnames(allModelComps) <- c("Model 1", "Model 2", "DiffName", "Diff Error Mean", "p-value",
                               "t-statistic", "Reject $H_{0}$")
  rownames(allModelComps) <- NULL
  
  return (allModelComps)
} # end CreateModelComparisonsDataset


# -------------------------------------------------------------------------------------------------------
# Function that orchestrates the creation of all the datasets encapsulating the modeling results for
# Phase 2.
# -------------------------------------------------------------------------------------------------------
CreateModelPerfResultsDatasetsWBetweenPhaseComparison <- function(modelList, 
                                                                  modelsCVErrorsPreviousPhase, 
                                                                  currentPhaseNum,
                                                                  previousPhaseNum,
                                                                  modelComparisonOrderedLevels)
{
  # First create the dataset that are used to assess the scaled models and compare them against each other, 
  # just like in Phase 1.
  modelsCVErrorsCurrentPhase <- CreateModelsCVErrorDataset(modelList)
  finalModelSummaries <- CreateModelTuningSummaries(modelList)
  
  
  # Get a list of all the different models in the phase 2 dataset.  It is assumed that the same set of models 
  # exist in both phases.  
  modelNames <- levels(modelsCVErrorsCurrentPhase$Model)
  
  # Create a dataset that takes the difference between each model's cross-validation error
  # (a pairwise comparison) fold-by-fold
  perfComparisons <- CreatePhaseDiffDataset(modelsCVErrorsCurrentPhase, 
                                            modelsCVErrorsPreviousPhase, 
                                            modelNames, 
                                            currentPhaseNum,
                                            previousPhaseNum,
                                            modelComparisonOrderedLevels)
  
  # Create a dataset comparing the means of each model in pairwise comparisons using a t-test
  perfAssessments <- CreatePhaseComparisonsDataset(modelsCVErrorsCurrentPhase, 
                                                   modelsCVErrorsPreviousPhase,
                                                   modelNames, 
                                                   currentPhaseNum,
                                                   previousPhaseNum,
                                                   modelComparisonOrderedLevels)
  
  # Now return the results in a list
  resultsList <- list(ModelCVErrors = modelsCVErrorsCurrentPhase,
                      FinalModelSummaries = finalModelSummaries,
                      PhasePerfComparisons = perfComparisons,
                      PhasePerfAssessments = perfAssessments,
                      ModelNames = modelNames)
  return (resultsList)
} # end CreateModelPerfResultsDatasetsWBetweenPhaseComparison


# --------------------------------------------------------------------------------------------------
# Function that orchestrates the creation of the different datasets derived from the models created
# above that are needed for visualizaiton and further analysis below.
# --------------------------------------------------------------------------------------------------
CreateModelPerfResultsDatasetsWWithinPhaseComparison <- function(modelList, modelComparisonOrderedLevels)
{
  # Create a dataset of the cross-validation errors for each fold each model was trained against
  modelsCVErrors <- CreateModelsCVErrorDataset(modelList)
  
  # Create a dataset summarizing the results of the tuning and training process for each model
  finalModelSummaries <- CreateModelTuningSummaries(modelList)
  
  # Get a list of all the different models in the dataset and then create the set of model by model 
  # comparisons that need to be made
  modelNames <- levels(modelsCVErrors$Model)
  modelComparisonsToMake <- permutations(length(modelNames), 2, modelNames)
  
  # Create a dataset that takes the difference between each model's cross-validation error
  # (a pairwise comparison) fold-by-fold
  perfComparisons <- CreateModelDiffsDataset(modelsCVErrors, modelComparisonsToMake, 
                                             modelComparisonOrderedLevels)
  
  # Create a dataset comparing the means of each model in pairwise comparisons using a t-test
  perfAssessments <- CreateModelComparisonsDataset(modelsCVErrors, modelComparisonsToMake, 
                                                   modelComparisonOrderedLevels)
  
  # Now return the results in a list
  resultsList <- list(ModelCVErrors = modelsCVErrors,
                      FinalModelSummaries = finalModelSummaries,
                      ModelPerfComparisons = perfComparisons,
                      ModelPerfAssessments = perfAssessments)
} # end CreateModelPerfResultsDatasetsWWithinPhaseComparison

