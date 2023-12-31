% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{ClassifyResult}
\alias{ClassifyResult}
\alias{ClassifyResult-class}
\alias{ClassifyResult,DataFrame,character,characterOrDataFrame-method}
\alias{show,ClassifyResult-method}
\alias{sampleNames}
\alias{sampleNames,ClassifyResult-method}
\alias{predictions}
\alias{predictions,ClassifyResult-method}
\alias{actualOutcome}
\alias{actualOutcome,ClassifyResult-method}
\alias{features}
\alias{features,ClassifyResult-method}
\alias{models}
\alias{models,ClassifyResult-method}
\alias{finalModel}
\alias{finalModel,ClassifyResult-method}
\alias{performance}
\alias{performance,ClassifyResult-method}
\alias{tunedParameters}
\alias{tunedParameters,ClassifyResult-method}
\alias{totalPredictions}
\alias{totalPredictions,ClassifyResult-method}
\alias{ClassifyResult,DataFrame,character-method}
\alias{allFeatureNames}
\alias{allFeatureNames,ClassifyResult-method}
\alias{chosenFeatureNames}
\alias{chosenFeatureNames,ClassifyResult-method}
\title{Container for Storing Classification Results}
\description{
Contains a list of models, table of actual sample classes and predicted
classes, the identifiers of features selected for each fold of each
permutation or each hold-out classification, and performance metrics such as
error rates. This class is not intended to be created by the user. It is
created by \code{\link{crossValidate}}, \code{\link{runTests}} or \code{\link{runTest}}.
}
\section{Constructor}{

\code{ClassifyResult(characteristics, originalNames, originalFeatures,
              rankedFeatures, chosenFeatures, models, tunedParameters, predictions, actualOutcome, importance = NULL, modellingParams = NULL, finalModel = NULL)}
\describe{
\item{\code{characteristics}}{A \code{\link{DataFrame}} describing the
characteristics of classification done. First column must be named
\code{"charateristic"} and second column must be named \code{"value"}. If
using wrapper functions for feature selection and classifiers in this
package, the function names will automatically be generated and therefore it
is not necessary to specify them.}
\item{\code{originalNames}}{All sample names.}
\item{\code{originalFeatures}}{All feature names. Character vector
or \code{\link{DataFrame}} with one row for each feature if the data set has multiple kinds
of measurements on the same set of samples.}
\item{\code{chosenFeatures}}{Features selected at each fold. Character
vector or a data frame if data set has multiple kinds of measurements on the same set of samples.}
\item{\code{models}}{All of the models fitted to the training data.}
\item{\code{tunedParameters}}{Names of tuning parameters and the value chosen of each parameter.}
\item{\code{predictions}}{A data frame containing sample IDs, predicted class or risk and information about the 
cross-validation iteration in which the prediction was made.}
\item{\code{actualOutcome}}{The known class or survival data of each sample.}
\item{\code{importance}}{The changes in model performance for each selected variable when it is excluded.}
\item{\code{modellingParams}}{Stores the object used for defining the model building to enable future reuse.}
\item{\code{finalModel}}{A model built using all of the samples for future use. For any tuning parameters, the
most popular value of the parameter in cross-validation is used. May be missing if some cross-validated fittings
failed. Could be of any class, depending on the R package used to fit the model.}
}
}

\section{Summary}{

\describe{
\item{\code{result} is a \code{ClassifyResult} object.}{
    \code{show(result)}: Prints a short summary of what \code{result} contains.
}}
}

\section{Accessors}{

\code{result} is a \code{ClassifyResult} object.
\describe{
\item{\code{sampleNames(result)}}{Returns a vector of sample names present in the data set.}}
\describe{
\item{\code{actualOutcome(result)}}{Returns the known outcome of each sample.}}
\describe{
\item{\code{models(result)}}{A \code{list} of the models fitted for each training.}}
\describe{
\item{\code{finalModel(result)}}{A deployable model fitted on all of the data for use on future data.}}
\describe{
\item{\code{chosenFeatureNames(result)}}{A \code{list} of the features selected for each training.}}
\describe{
\item{\code{predictions(result)}}{Returns a \code{DataFrame} which has columns with test sample,
cross-validation and prediction information.}}
\describe{
\item{\code{performance(result)}}{Returns a \code{list} of performance measures. This is
empty until \code{calcCVperformance} has been used.}}
\describe{
\item{\code{tunedParameters(result)}}{Returns a \code{list} of tuned parameter values.
If cross-validation is used, this list will be large, as it stores chosen values
for every iteration.}}
\describe{
\item{\code{totalPredictions(result)}}{A single number representing the total number.
of predictions made during the cross-validation procedure.}}
}

\examples{

  #if(require(sparsediscrim))
  #{
    data(asthma)
    classified <- crossValidate(measurements, classes, nRepeats = 5)
    class(classified)
  #}
  
}
\author{
Dario Strbenac
}
