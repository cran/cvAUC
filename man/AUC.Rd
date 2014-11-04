\name{AUC}
\alias{AUC}
\title{Area Under the Curve}
\description{
This function calculates Area Under the ROC Curve (AUC).  The AUC can be defined as the probability that the fit model will score a randomly drawn positive sample higher than a randomly drawn negative sample.  This is also equal to the value of the Wilcoxon-Mann-Whitney statistic.  This function is a wrapper for functions from the ROCR package.
}
\usage{
AUC(predictions, labels)
}
\arguments{
  \item{predictions}{
A vector of predictions, or predicted probabilities, for each observation.
}
  \item{labels}{
A binary vector containing the true values for each observation.  Must have the same length as \code{predictions}.
}
}
\value{
The value returned is the Area Under the ROC Curve (AUC).
}
\references{
References to the underlying ROCR code, used to calculate area under the ROC curve, can be found on the ROCR homepage at:
 \url{http://rocr.bioinf.mpi-sb.mpg.de}
}
\author{
Erin LeDell \email{ledell@berkeley.edu}
}

\seealso{
\code{\link[ROCR]{prediction}}, \code{\link[ROCR]{performance}}, \code{\link{cvAUC}}, \code{\link{ci.cvAUC}}, \code{\link{ci.pooled.cvAUC}}
}
\examples{
library(cvAUC)

data(ROCR.simple)
auc <- AUC(ROCR.simple$predictions, ROCR.simple$labels)
# [1] 0.8341875

}
