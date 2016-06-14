#' Train-Test Splitting
#'
#' Divide a data frame into training and test set, or training, quiz and test set.
#'
#' @param seed A seed for randomization (recommended).
#' @param prop.train Proportion of the data to include in the training set.
#' @param prop.quiz Proportion of the data to include in a quiz set (if any).
#' Set to \code{NULL} by default.
#' @param data The data frame to be split.
#'
#' @return A list with elements named \code{test} and \code{train}, or with
#' elements named \code{train} and \code{quiz} and \code{test}.
#' @note For more details on use, consult
#' \url{http://statistics.rainandrhino.org/tigerTree/divideTrainTest.html}
#' @export
divideTrainTest <- function(seed = NULL, prop.train = 0.6,
                            prop.quiz = NULL, data) {

  # process seed option
  if (!is.null(seed)) set.seed(seed)

  if (prop.train >= 1) stop("Proportion in training set must be a number less than 1.")

  if (is.null(prop.quiz)) {
    n <- nrow(data)
    m <- floor(prop.train*n)
    bools <- c(rep(TRUE, m), rep(FALSE, n-m))
    inTrain <- sample(bools, size = n, replace = FALSE)
    results <- split(data, f = inTrain)
    names(results) <- c("test","train")
    return(results)
  }

  if (!is.null(prop.quiz)) {
    checksum <- prop.train + prop.quiz
    if (checksum >= 1) stop("Training and quiz proportions must sum to less than 1.")
    n <- nrow(data)
    m1 <- floor(prop.train*n)
    m2 <- floor(prop.quiz*n)
    m3 <- n - m1 - m2
    types <- c(rep(0, m1), rep(1,m2), rep(2, m3))
    types <- sample(types, size = n, replace = FALSE)
    results <- split(data, f = types)
    names(results) <- c("train", "quiz", "test")
    return(results)
  }



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
#' @return  A list containing: \code{deviance} and \code{residMeanDev}
#' (residual mean deviance).  If \code{mod} is a classification tree, then the list
#' also contains \code{error.rate}, \code{misclass} (number of misclassifications
#' at terminal nodes), and \code{confusion} (the confusion matrix).
#' @examples
#' dfs <- divideTrainTest(seed = 3030, prop.train = 0.67, data = iris)
#' irisTrain <- dfs$train
#' irisTest <- dfs$test
#' tr.mod <- tree(Species ~ ., data = irisTrain)
#' summary(tr.mod)
#' tryTree(mod = tr.mod, testSet = irisTest, truth = irisTest$Species)
#' @note For more details on use, consult
#' \url{http://statistics.rainandrhino.org/tigerTree/divideTrainTest.html}
#' @export
tryTree <- function(mod, testSet, truth, printOut = TRUE) {
  treeType <- class(mod$y)
  # exclude observations where value of response variable is missing
  missingResponse <- is.na(truth)
  numberMissing <- sum(missingResponse)
  testSet <- testSet[!missingResponse,]
  truth <- truth[!missingResponse]
  if (treeType == "factor") {

      res <- devClass(mod = mod, newdata = testSet, truth = truth)
      tab <- res$tab
      numberWrong <- res$wrong
      tries <- res$tries
      error.rate = numberWrong/tries
      if (printOut) {
        if ( numberMissing > 0) {
          cat(paste0("(",numberMissing," observations were missing the response",
                     " variable.\nThey were removed from the quiz/test set.)\n"))
        }
      cat(paste0("Residual mean deviance:  ",
                 round(res$meanDev,1),
                 " = ",
                 round(res$totalDev,1),
                 " / ",
                 res$divisor, "\n"))
      cat(paste0("Misclassification error rate:  ", round(error.rate,5),
                 " = ", numberWrong, " / ", tries, "\n"))
      cat("Confusion matrix:\n")
      print(tab)
      }

    results <- list(error.rate = error.rate,
                    misclass = numberWrong,
                    confusion = tab,
                    deviance = res$totalDev, residMeanDev = res$meanDev)
    return(invisible(results))
  } else {

    res <- devRegression(mod = mod, newdata = testSet, truth = truth)

    if ( printOut ) {
      if ( numberMissing > 0) {
        cat(paste0("(",numberMissing," observations were missing the response",
                  " variable.\nThey were removed from the quiz/test set.)\n"))
      }
      cat(paste0("Residual mean deviance:  ",
               round(res$meanDev,4),
               " = ",
               round(res$totalDev,1),
               " / ",
               res$divisor))
    }

    results <- list(deviance = res$totalDev, residMeanDev = res$meanDev)
    return(invisible(results))
  }
}

devRegression <- function(mod, newdata, truth) {

  # make frame of prediction, true value and node-number, for all
  # observations that arrived at a terminal node
  prediction <- predict(mod, newdata = newdata)
  predWhere <-  predict(mod, newdata = newdata, type = "where")
      # use utility functions (see after distAtNodes):
  predWhere <- plyr::mapvalues(predWhere,
                                  from = nodeRows(mod),
                                  to   = nodeNumbers(mod))
  leafNumbers <- row.names(mod$frame)
  leaves <- unique(leafNumbers[mod$frame$var == "<leaf>"])
  atLeaf <- predWhere %in% leaves
  df <- data.frame(prediction, truth, node = predWhere, atLeaf)
  df <- df[atLeaf,]

  # compute deviance and mean deviance
  df$sqDev <- with(df, (prediction - truth)^2 )
  totalDev <- sum(df$sqDev)
  divisor <- nrow(df) - length(leaves)
  meanDev <- totalDev/divisor
  return(list(totalDev = totalDev, divisor = divisor, meanDev = meanDev))
}


devClass <- function(mod, newdata, truth) {

  # cut data down to observations that arrived at a terminal node
  preds <- predict(mod, newdata = newdata, type = "class")
  predWhere <-  predict(mod, newdata = newdata, type = "where")
  # use utility functions (see after distAtNodes):
  predWhere <- plyr::mapvalues(predWhere,
                               from = nodeRows(mod),
                               to   = nodeNumbers(mod))
  leafNumbers <- row.names(mod$frame)
  leaves <- unique(leafNumbers[mod$frame$var == "<leaf>"])
  atLeaf <- predWhere %in% leaves
  truth <- truth[atLeaf]
  nodes <- predWhere[atLeaf]
  prediction <- preds[atLeaf]

  # get confusion matrix:
  confusion <- xtabs( ~ prediction + truth)

  # now get the distributions at the terminal nodes:
  tab <- xtabs(~ nodes + truth)

  # function to compute deviance at node:
  nodeDev <- function(dist) {
    dist <- dist[dist > 0]
    n <- sum(dist)
    dev <- -2*(sum(dist*log(dist/n)))
    dev
  }

  # use function to compute deviance:
  totalDev <- 0
  for ( i in 1:nrow(tab) ) {
    totalDev <- totalDev + nodeDev(tab[i,])
    print(nodeDev(tab[i,]))
  }
  divisor <- length(truth) - length(unique(nodes))
  meanDev <- totalDev/divisor

  # get the rest and return:
  wrong <- sum(prediction != truth)
  return(list(totalDev = totalDev, divisor = divisor, meanDev = meanDev,
              tries = length(prediction), wrong = wrong,
              tab = confusion))
}


#' Tree-Detective
#'
#' Yes-No questions guide the user through a classification or regression
#' tree.
#'
#' @param mod A tree model constructed by the \code{tree} package.
#' @param data Data frame used to construct the model.
#' @param rowname Character indicating the initial row name in \code{mod$frame}.
#' Set to "1" by default.
#'
#' @return Side-effects to console.
#' @export
#' @examples
#' \dontrun{
#' tr.mod <- tree(Species ~ ., data = iris)
#' treeDetective(tr.mod, iris)
#' }
#' @note For more details on use, consult
#' \url{http://statistics.rainandrhino.org/tigerTree/treeDetective.html}
treeDetective <- function(mod, data, rowname = "1") {
  df <- mod$frame
  yprobs <- df$yprob
  if ( !is.null(yprobs) ) {
    rownames(yprobs) <- row.names(df)
  }
  currentRow <- df[rowname, ]
  status <- currentRow$var
  if (status == "<leaf>") {
    cat(paste0("I have reached a terminal node with ", df[rowname,"n"]," items.\n"))
    msg <- paste0("I predict the value is ", currentRow$yval,".\n")
    cat(msg)
    if (is.factor(currentRow$yval)) {
      msg2 <- paste0("The estimated probabilities for each class are:\n")
      cat(msg2)
      print(yprobs[rowname,], row.names = FALSE)
      return(invisible())
    }
  } else {
    varName <- as.character(currentRow$var)
    var <- get(varName, envir = as.environment(data))
    isFactor <- is.factor(var)
    charStr <- currentRow$splits[,1]
    rn <- as.numeric(rowname)
    answer <- NULL
    while ( is.null(answer) ) {
      answer <- ifelse(isFactor,
                       readline(prompt = factorQuestion(charStr, varName, data)),
                       readline(prompt = numericQuestion(charStr, varName)))
      answer <- tolower(substr(answer, 1,1))
      if (!(answer %in% c("y","n","d"))) {
        cat("Sorry, I did not understand that.  Try again.")
        answer <- NULL
      }
    }
    if ( answer == "d") {
      cat(paste0("OK, we are stuck at a node with ", df[rowname,"n"]," items.\n"))
      msg <- paste0("I predict the value is ", currentRow$yval,".\n")
      cat(msg)
      if (is.factor(currentRow$yval)) {
        msg2 <- paste0("The estimated probabilities for each class are:\n")
        cat(msg2)
        print(yprobs[rowname,], row.names = FALSE)
        return(invisible())
      }
    }
    newRowName <- ifelse(answer == "y", 2*rn, 2*rn + 1)
    treeDetective(mod, data, as.character(newRowName))
  }
}

factorQuestion <- function(charStr, varName, data) {
  # strip leading :
  charStr <- substr(charStr, 2, nchar(charStr))
  chars <- strsplit(charStr, split = "")
  levelNumbers <- sapply(chars[[1]], function(x) which(letters == x))
  var <- get(varName, envir = as.environment(data))
  varLevels <- levels(var)[levelNumbers]
  joinedLevels <- paste0(varLevels, collapse = ", ")
  return(paste0("Is ", varName, " one of: ", joinedLevels," (y/n/dunno)? "))
}

numericQuestion <- function(charStr, varName) {
  # strip leading <
  charStr <- substr(charStr, 2, nchar(charStr))
  return(paste0("Is ", varName, " < ", charStr," (y/n/dunno)? "))
}


#' Distribution at Nodes
#'
#' Easily find the distribtution of the response variable at each of the
#' nodes of a classification tree.
#'
#' @param mod A tree model constructed by the \code{tree} package.
#' @param df A data frame (usually the training set, quiz set or test set).
#' @param resp_varname The name of the response variable, as a character string.
#'
#' @return A table object.
#' @export
#' @examples
#' \dontrun{
#' dfs <- divideTrainTest(seed = 3030, prop.train = 0.67, data = iris)
#' irisTrain <- dfs$train
#' irisTest <- dfs$test
#' tr.mod <- tree(Species ~ ., data = irisTrain)
#' distAtNodes(tr.mod, df = irisTest, resp_varname = "Species")
#' }
#' @note For more details on use, consult
#' \url{http://statistics.rainandrhino.org/tigerTree/distAtNodes.html}
distAtNodes <- function(mod, df, resp_varname) {
  nodes_by_row <- predict(mod, newdata = df, type = "where")
  nodes_by_num <- plyr::mapvalues(nodes_by_row,
                                  from = nodeRows(mod),
                                  to   = nodeNumbers(mod))
  tempDF <- data.frame(node = nodes_by_num, response = df[, resp_varname])
  names(tempDF)[2] <- resp_varname
  tab <- eval(parse(
    text = paste0("xtabs(~ node + ",resp_varname, ", data = tempDF)")))
  tab
}


nodeNumbers <- function(mod) {
  left_splits <- mod$frame$splits[, 1]
  nodes_lines <- mod$frame[left_splits == "",]
  as.numeric(row.names(nodes_lines))
}

nodeRows <- function(mod) {
  sort(as.numeric(unique(mod$where)))
}
