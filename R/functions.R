#' Train-Test Splitting
#'
#' Divide a data frame into training and test sets.
#'
#' @param seed A seed for randomization (recommended).
#' @param prop.train Proportion of the data to include in the training set.
#' @param data The data frame to be split.
#'
#' @return A list with elements named \code{test} and \code{train}.
#' @export
divideTrainTest <- function(seed = NULL, prop.train = 0.7, data) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(data)
  m <- floor(prop.train*n)
  bools <- c(rep(TRUE, m), rep(FALSE, n-m))
  inTrain <- sample(bools, size = n, replace = FALSE)
  results <- split(data, f = inTrain)
  names(results) <- c("test","train")
  results
}



#' Predict with a Tree
#'
#' @param mod A tree model constructed by package \code{tree}.
#' @param testSet The test set (a data frame).
#' @param truth Correct values of the response variable.
#' @param printOut If TRUE, provide a printout to the console.
#'
#' @return  A list,  If \code{md} is a classification tree, then the list
#' contains elements \code{error.rate} and \code{confusion} (the confucsion matrix).
#' If \code{md} is a classification tree, then the list
#' contains elements \code{error.rate} is a regression tree, then the list contains
#' elements named \code{mse} and \code{rmse} (mean-square error and root mean square
#' error).
#' @examples
#' dfs <- divideTrainTest(seed = 3030, prop.train = 0.67, data = iris)
#' irisTrain <- dfs$train
#' irisTest <- dfs$test
#' tr.mod <- tree(Species ~ ., data = iris)
#' summary(tr.mod)
#' tryTree(mod = tr.mod, testSet = irisTest, truth = irisTest$Species)
#' @export
tryTree <- function(mod, testSet, truth, printOut = TRUE) {
  treeType <- class(mod$y)
  if (treeType == "factor") {
    prediction <- predict(mod, newdata = testSet, type = "class")
    wrong <- prediction!= truth
    error.rate <- mean(wrong)
    tab <- xtabs(~prediction + truth)
    if (printOut) {
      cat(paste0("Error Rate is ", round(error.rate,4),".\n"))
      cat("Confusion matrix:\n")
      print(tab)
    }
    results <- list(error.rate = error.rate, confusion = tab)
    return(invisible(results))
  } else {
    prediction <- predict(mod, newdata = testSet)
    mse <- mean((prediction - truth)^2)
    rmse <- sqrt(mse)
    if (printOut) {
      cat(paste0("Mean-square-Error = ", round(mse,3)," RMSE = ", round(rmse, 3),"\n"))
    }
    results <- list(mse = mse, rmse = rmse)
    return(invisible(results))
  }
}
