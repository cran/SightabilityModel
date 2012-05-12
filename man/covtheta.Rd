\name{covtheta}
\alias{covtheta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimates var/cov matrix of inflation factors (1/prob detection) using a non-parametric bootstrap.   
}
\description{
Estimates var/cov matrix of inflation factors (1/prob detection) using a non-parametric bootstrap.  Called by function Sight.Est if Vm.boot=TRUE.
}
\usage{
covtheta(total, srates, stratum, subunit, covars, betas, varbetas, nboots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{total}{
 \code{total} Number of animals in each independently sighted group
}
  \item{srates}{
  \code{srates} Plot sampling probability (associated with the independently observed animal groups)
}
  \item{stratum}{
\code{stratum}  Stratum identifiers (associated with the independently observed animal groups)
}
  \item{subunit}{
  \code{subunit} Plot ID (associated with the independently observed animal groups)
}
  \item{covars}{
   \code{covars} Matrix of sightability covariates (associated with the independently observed animal groups)
}
  \item{betas}{
   \code{betas} Logistic regression parameter estimates (from fitted sightability model)
}
  \item{varbetas}{
   \code{varbetas} Estimated variance-covariance matrix for the logistic regression parameter estimates (from fitted sightability model)
}
  \item{nboots}{
 \code{nboots} Number of bootstrap resamples.
}
}


\value{
  \item{smat }{Estimated variance-covariance matrix for the inflation factors theta = (1/probability of detection).  This is an n.animal x n.animal matrix.
}
}
\author{
John Fieberg
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{Sight.Est}}
}
