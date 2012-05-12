\name{summary.sightest}
\alias{summary.sightest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarize sightability estimator
}
\description{
Calculates confidence interval (based on asympototic [normal or log-normal assumption])
}
\usage{
 \method{summary}{sightest}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Sightability object, output from call to Sight.Est function.}
  \item{...}{arguments to be passed to or from other methods}
}
\value{
  \item{Nhat}{Sightability population estimate}
  \item{lcl}{Lower confidence limit}
  \item{ucl}{Upper confidence limit}
}
\author{
John Fieberg
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{Sight.Est}}
}
 
