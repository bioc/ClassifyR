# Ranking of Survival-associated Features with coxph Statistic
coxphRanking <- function(measurementsTrain, survivalTrain, verbose = 3) # Clinical data or one of the other inputs, transformed.
{
  pValues <- rep(NA, ncol(measurementsTrain))
  names(pValues) <- colnames(measurementsTrain)

  isCat <- sapply(measurementsTrain, class) %in% c("character", "factor")
  if(any(isCat))
  {
    pValues[isCat] <- apply(measurementsTrain[, isCat, drop = FALSE], 2, function(featureColumn){
      fit <- survival::coxph(survivalTrain ~ featureColumn)
      s <- summary(fit)
      s$waldtest["pvalue"]
    })
  }
  if(any(!isCat))
  {
    tests <- colCoxTests(as.matrix(measurementsTrain[, !isCat, drop = FALSE]), survivalTrain)
    pValues[!isCat] <- tests[, "p.value"]
  }
  order(pValues) # From smallest to largest.
}
attr(coxphRanking, "name") <- "coxphRanking"

# Copied verbatim from the archived survHD package Christoph Bernau and Levi Waldron
# Available on bitbucket https://bitbucket.org/lwaldron/survhd/src/master/R/filter.r
###############################################################################
# Filename: filter.r
# Title: Gene selection (filter) methods.
#
# Author: Christoph Bernau
# Email: <bernau@ibe.med.uni-muenchen.de>
# Date of creation: 28.8.2012
#
# Brief description:
#   Returns an object of class 'GeneSel'.
#
# Further comments and notes:
#   Are usually not called directly by the user, but via
#   'GeneSelection'.
#
###############################################################################

#' @useDynLib ClassifyR, .registration = TRUE
coxmatC<-function(X,time,status){
  ### method for handling ties (alternative 'breslow')
  method <- "efron"
  result<-	.C("coxmat", regmat = as.double(X), ncolmat = 
                as.integer(ncol(X)), nrowmat = as.integer(nrow(X)), 
              reg = as.double(X[, 1]), zscores = as.double(numeric(ncol(X))),
              coefs = as.double(numeric(ncol(X))), maxiter = as.integer(20), 
              nusedx = as.integer(nrow(X)), nvarx = as.integer(1), 
              time = as.double(time), status = as.integer(status), 
              offset = as.double(numeric(nrow(X))), 
              weights = as.double(numeric(nrow(X)) + 1), 
              strata = as.integer(numeric(nrow(X))), means = double(1), 
              beta = double(1), u = double(1), imat = double(1), 
              loglik = double(2), flag = integer(1), work = 
                double(2 * nrow(X) + 2 + 3), eps = as.double(1e-09), 
              tol_chol = as.double(.Machine$double.eps^0.75), 
              sctest = as.double(method == "efron"), sctest2 = as.double(1), 
              sctest3 = as.double(1), PACKAGE = "ClassifyR")	
  return(result)	
}


fastCox <- function(X, y, learnind, criterion, ...) {
  ### use learningset only and sort according to time
  X <- X[learnind, ]
  time <- y[learnind, 1]
  status <- y[learnind, 2]
  sorted <- order(time)
  time <- time[sorted]
  status <- status[sorted]
  X <- as.matrix(X[sorted, ])
  # compute columnwise coxmodels
  out <- coxmatC(X,time,status)
  # compute p-values
  if (criterion == "pvalue") 
    crit <- (1 - pnorm(abs(out$zscores))) * 2
  if (criterion == "coefficient") 
    crit <- abs(out$coefs)
  
  ### and return a VarSelOut-object
  new("VarSelOut", varsel = crit, criterion = criterion)
}

# Equivalent to genefilter::rowttests for the cox model. This is much faster
# than calling coxph for each row of a high-dimensional matrix.

######################################
######################################
#' A function to perform fast or standard Cox proportional hazard model tests.
#'
#' @param measurements matrix with variables as columns.
#' @param outcome matrix with first column as time and second column as event.
#' @param option Default: \code{"fast"}. Whether to use the fast or slow method.
#' @param ... Not currently used.
#'
#' @return CrossValParams object
#' @export
#'
#' @examples
#' data(asthma)
#' time <- rpois(nrow(measurements), 100)
#' status <- sample(c(0,1), nrow(measurements), replace = TRUE)
#' outcome <- cbind(time, status)
#' output <- colCoxTests(measurements, outcome, "fast")
#' @export
colCoxTests <- function(measurements, outcome, option = c("fast", "slow"), ...) {
  option <- match.arg(option) # Error if not either of the two above options.
  if (option == "fast") {
    measurements <- as.matrix(measurements)  #make variables columns
    time <- outcome[, 1]
    status <- outcome[, 2]
    sorted <- order(time)
    time <- time[sorted]
    status <- status[sorted]
    measurements <- measurements[sorted, ]
    ## method for handling ties (alternative 'breslow')
    method <- "efron"
    ## compute columnwise coxmodels
    out <- coxmatC(measurements, time, status)
    ## compute p-values and return them
    output <- data.frame(coef = out$coefs, se.coef = out$coefs/out$zscores, 
                         p.value = (1 - pnorm(abs(out$zscores))) * 2)
    rownames(output) <- colnames(measurements)
  } else {
    output <- (apply(measurements, 2, function(measurementsFeature) {
      fit <- try(coxph(outcome ~ measurementsFeature))
      if (class(fit) == "try-error") {
        c(NA, NA)
      } else {
        summary(fit)$coefficients[1, c(1, 3, 5)]
      }
    }))
    colnames(output) <- c("coef", "se.coef", "p.value")
    rownames(output) <- rownames(measurements)
    output <- data.frame(output)
  }
  return(output)
  ### dataframe with two columns: coef = Cox regression coefficients, p.value = Wald Test p-values.
  ### Rows correspond to the rows of measurements.
}