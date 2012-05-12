\name{SightabilityModel-package}
\alias{SightabilityModel-package}
\alias{SightabilityModel}
\docType{package}
\title{
Wildlife Sightability Modeling
}
\description{
Uses logistic regression to model the probability of detection as a function of covariates. 
             This model is then used with observational survey data to estimate population size, while
             accounting for uncertain detection.  See Steinhorst and Samuel (1989).
}
\details{
\tabular{ll}{
Package: \tab SightabilityModel\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-09-06\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
See vignette (to come)
}
\author{
John Fieberg

Maintainer: John Fieberg <john.fieberg@state.mn.us>

}
\references{
 Steinhorst, Kirk R. and Samuel, Michael D. 1989. Sightability Adjustment Methods for Aerial Surveys of Wildlife Populations.
Biometrics 45:415--425.
}

\keyword{ package }
\examples{
data(exp.m)
}
