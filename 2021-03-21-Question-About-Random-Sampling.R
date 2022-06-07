# Reddit Question - 2021-03-21
# URL: https://www.reddit.com/r/Rlanguage/comments/ma2wtb/random_sampling_until_success_with_character/
#
# Random Sampling until success with character variables? (self.Rlanguage)
# submitted 5 hours ago by Ambitious_Badger341
#
# I haven't found much to help me with understanding how its done. But for
# example the letters in the alphabet, how would i find the average time it
# takes to get to "B" with random sampling (without replacement) at a large
# scale. I cant wrap my head around it.

library(ggplot2)

numIterations <- 100000
sampleSet <- LETTERS
targetVal <- "B"
# numHistBreaks <- round(length(LETTERS) / 2, 0)
numHistBreaks <- length(sampleSet) + 1

# Compute the results
## Declare a data structure container to hold all results
resultSets <- vector("list", numIterations)

## Pre-Fill the data structure container, which makes a significant performance difference
resultSets <- lapply(
  1:numIterations,
  function(x) {

    lst <- list(
      iteration = as.numeric(x),
      resultVec = sample(sampleSet, length(sampleSet), replace = FALSE)
    )

    lst$timeUnitsToFindB <- which(lst[["resultVec"]] == targetVal, arr.ind = TRUE)

    return(lst)
  }
)

## Save all of the results to a single vector
resultsVector <- unlist(
  lapply(resultSets[1:numIterations], function(rslt) {rslt$timeUnitsToFindB})
)

## Prepare a list of functions to be used to calculate summary metrics
functionList <- list("length", "min", "max", "mean", "median", "sd")

## Calculate the summary of results
resultsSummary <- lapply(functionList, do.call, args = list(resultsVector))

## Rename the list elements for clarity in reporting
names(resultsSummary) <- unlist(functionList)

resultsSummary

# Set up plotting functions to visualize the results
## Create an array of fill colors for the histogram
colSet <- topo.colors(round(numHistBreaks / 2, 0) - 1)
# colSet <- rainbow(round(numHistBreaks / 2, 0) - 1)
histFillColors <-  c(colSet, adjustcolor(colSet[length(colSet)], alpha.f = 0.5), rev(colSet))

## Plot the histogram
hist(
  resultsVector,
  main = paste("Histogram - Frequency Distribution for Number of Tries to Find", targetVal),
  sub = paste("Number of trials =", numIterations),
  xlab = "Number of Tries\n",
  ylab = "frequency",
  freq = TRUE,
  xlim = c(0, numHistBreaks + 1),
  col = histFillColors,
  breaks = 0:(numHistBreaks + 1)
)
