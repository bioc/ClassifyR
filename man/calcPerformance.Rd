% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcPerformance.R
\name{calcExternalPerformance}
\alias{calcExternalPerformance}
\alias{calcPerformance}
\alias{calcCVperformance}
\alias{calcExternalPerformance,factor,factor-method}
\alias{calcExternalPerformance,Surv,numeric-method}
\alias{calcCVperformance,ClassifyResult-method}
\alias{calcExternalPerformance,factor,tabular-method}
\alias{performanceTable}
\title{Add Performance Calculations to a ClassifyResult Object or Calculate for a
Pair of Factor Vectors}
\usage{
\S4method{calcExternalPerformance}{factor,factor}(
  actualOutcome,
  predictedOutcome,
  performanceTypes = "auto"
)

\S4method{calcExternalPerformance}{Surv,numeric}(
  actualOutcome,
  predictedOutcome,
  performanceTypes = "auto"
)

\S4method{calcExternalPerformance}{factor,tabular}(
  actualOutcome,
  predictedOutcome,
  performanceTypes = "auto"
)

\S4method{calcCVperformance}{ClassifyResult}(result, performanceTypes = "auto")

performanceTable(
  resultsList,
  performanceTypes = "auto",
  aggregate = c("median", "mean")
)
}
\arguments{
\item{actualOutcome}{A factor vector or survival information specifying each sample's known outcome.}

\item{predictedOutcome}{A factor vector or survival information of the same length as \code{actualOutcome} specifying each sample's predicted outcome.}

\item{performanceTypes}{Default: \code{"auto"} A character vector. If \code{"auto"}, Balanced Accuracy will be used
for a classification task and C-index for a time-to-event task.
Must be one of the following options:
\itemize{
\item{\code{"Error"}: Ordinary error rate.}
\item{\code{"Accuracy"}: Ordinary accuracy.}
\item{\code{"Balanced Error"}: Balanced error rate.}
\item{\code{"Balanced Accuracy"}: Balanced accuracy.}
\item{\code{"Sample Error"}: Error rate for each sample in the data set.}
\item{\code{"Sample Accuracy"}: Accuracy for each sample in the data set.}
\item{\code{"Micro Precision"}: Sum of the number of correct predictions in
        each class, divided by the sum of number of samples in each class.}
\item{\code{"Micro Recall"}: Sum of the number of correct predictions in each 
        class, divided by the sum of number of samples predicted as
        belonging to each class.}
\item{\code{"Micro F1"}: F1 score obtained by calculating the
harmonic mean of micro precision and micro recall.}
\item{\code{"Macro Precision"}: Sum of the ratios of the number of correct predictions
in each class to the number of samples in each class, divided by the number of classes.}
\item{\code{"Macro Recall"}: Sum of the ratios of the number of correct predictions in each
class to the number of samples predicted to be in each class, divided by the number of classes.}
\item{\code{"Macro F1"}: F1 score obtained by calculating the harmonic mean of macro precision
and macro recall.}
\item{\code{"Matthews Correlation Coefficient"}: Matthews Correlation Coefficient (MCC). A score
between -1 and 1 indicating how concordant the predicted classes are to the actual classes. Only defined if
there are two classes.}
\item{\code{"AUC"}: Area Under the Curve. An area ranging from 0 to 1, under the ROC.}
\item{\code{"C-index"}: For survival data, the concordance index, for models which produce risk scores. Ranges from 0 to 1.}
\item{\code{"Sample C-index"}: Per-individual C-index.}
}}

\item{result}{An object of class \code{\link{ClassifyResult}}.}

\item{resultsList}{A list of modelling results. Each element must be of type \code{\link{ClassifyResult}}.}

\item{aggregate}{Default: \code{"median"}. Can also be \code{"mean"}. If there are multiple values, such as for repeated
cross-validation, then they are summarised to a single number using either mean or median.}
}
\value{
If \code{calcCVperformance} was run, an updated
\code{\linkS4class{ClassifyResult}} object, with new metric values in the
\code{performance} slot. If \code{calcExternalPerformance} was run, the
performance metric value itself.
}
\description{
If \code{calcExternalPerformance} is used, such as when having a vector of
known classes and a vector of predicted classes determined outside of the
ClassifyR package, a single metric value is calculated. If
\code{calcCVperformance} is used, annotates the results of calling
\code{\link{crossValidate}}, \code{\link{runTests}} or \code{\link{runTest}} with one of the user-specified performance measures.
}
\details{
All metrics except Matthews Correlation Coefficient are suitable for
evaluating classification scenarios with more than two classes and are
reimplementations of those available from Intel DAAL.

\code{\link{crossValidate}}, \code{\link{runTests}} or \code{\link{runTest}} was run in resampling mode, one performance
measure is produced for every resampling. Otherwise, if the leave-k-out mode was used,
then the predictions are concatenated, and one performance measure is
calculated for all classifications.

\code{"Balanced Error"} calculates the balanced error rate and is better
suited to class-imbalanced data sets than the ordinary error rate specified
by \code{"Error"}. \code{"Sample Error"} calculates the error rate of each
sample individually. This may help to identify which samples are
contributing the most to the overall error rate and check them for
confounding factors. Precision, recall and F1 score have micro and macro
summary versions. The macro versions are preferable because the metric will
not have a good score if there is substantial class imbalance and the
classifier predicts all samples as belonging to the majority class.
}
\examples{

  predictTable <- DataFrame(sample = paste("A", 1:10, sep = ''),
                            class = factor(sample(LETTERS[1:2], 50, replace = TRUE)))
  actual <- factor(sample(LETTERS[1:2], 10, replace = TRUE))                             
  result <- ClassifyResult(DataFrame(characteristic = "Data Set", value = "Example"),
                           paste("A", 1:10, sep = ''), paste("Gene", 1:50), list(paste("Gene", 1:50), paste("Gene", 1:50)), list(paste("Gene", 1:5), paste("Gene", 1:10)),
                           list(function(oracle){}), NULL, predictTable, actual)
  result <- calcCVperformance(result) 
  performance(result)

}
\author{
Dario Strbenac
}
