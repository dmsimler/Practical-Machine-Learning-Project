# ---------------------------------------------------------------------------------------------------
# Functions used to identify the various predictors that won't provide much predictive value and 
# could be removed from the training dataset.
# ---------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------
# Function to identifiy predictors whose values show very little variance and thus have very little
# predictive value.
# See pages 43-45 in Applied Predictive Analytics for additional details.
# ---------------------------------------------------------------------------------------------------
FindNearZeroVarPredictors <- function(trainingData, targetColIndex)
{
  zeroVarVarsToRemove <- nearZeroVar(trainingData[, -targetColIndex])
  return(zeroVarVarsToRemove)
} # end FindNearZeroVarPredictors


# ---------------------------------------------------------------------------------------------------
# Function to find predictors that could be removed are highly coorelated with each other.  
# See pages 45-47 in Applied Predictive Analytics for additional details.
# ---------------------------------------------------------------------------------------------------
FindRemovableCorrelatedPredictors <- function(trainingData, targetColIndex, correlationThreshold)
{
  correlations <- cor(trainingData[, -targetColIndex], use = "na.or.complete")
  correlatedVarsToRemove <- findCorrelation(correlations, cutoff = correlationThreshold)
  
  return (correlatedVarsToRemove)
} # end FindRemovableCorrelatedPredictors


# ---------------------------------------------------------------------------------------------------
# Function that calculates the percentage of rows with missing data per column per class (A, B, C, etc.) 
# ---------------------------------------------------------------------------------------------------
FindColsWithMostlyNAPerClass <- function(trainingData, targetColIndex, naPctThreshold)
{
  # Initialize a dataframe to hold the calculated percentages of missing values by column and class
  targetClasses <- unique(trainingData[, targetColIndex])
  naPctPerColPerClass <- data.frame(classe = targetClasses)
  
  for(colName in colnames(trainingData[, -targetColIndex])) 
    naPctPerColPerClass <- cbind(naPctPerColPerClass, rep(0.0, length(targetClasses)))
  
  colnames(naPctPerColPerClass)[2:ncol(trainingData)] <- colnames(trainingData[, -targetColIndex])
  
  # Now calculate the actual percentage of missing values for each column for the rows belonging to a
  # given target class
  for (currentClass in targetClasses)
  {
    # Grab the rows that belong to the current class being processed
    classRows <- trainingData[, targetColIndex] == currentClass
    classDataSubset <- trainingData[classRows, ]
    numSamplesInClass <- nrow(classDataSubset)
    
    # Find the rows for the current class being processed
    naMappingRowIndex <- which(naPctPerColPerClass$classe == currentClass)
    
    for (currentColIndex in 2:ncol(naPctPerColPerClass))
    {
      # Calculate the percentage of samples in this class that have an NA for this column
      naPctPerColPerClass[naMappingRowIndex, currentColIndex] <- 
        sum(is.na(classDataSubset[, currentColIndex - 1])) / numSamplesInClass
    } # end for
  } # end for
  
  # Return the NA pct per col per class
  colsWithMostlyNA <- which(colMeans(naPctPerColPerClass[, -1]) > naPctThreshold)
  
  return (colsWithMostlyNA)
} # end FindColsWithMostlyNAPerClass
