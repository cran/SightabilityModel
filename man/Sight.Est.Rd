\name{Sight.Est}
\alias{Sight.Est}
%%\alias{print.sightest}
\title{Sightability Model Estimator}
\description{Estimates population abundance by 1) fitting a sightability (logistic regression) model to "test trial" data;
  2) applying the fitted model to independent (operational) survey data to correct for detection rates < 1.}
\usage{Sight.Est(form, sdat, odat, sampinfo, method = "Wong", logCI=TRUE, alpha=0.05, Vm.boot=FALSE, nboot=1000, bet=NULL, varbet=NULL)}

\arguments{
  \item{form}{
   a symbolic description of the sightability model to be fit (e.g., "y~x1 + x2 + ..."), where y is a binary response variable (=1 if the animal is seen and 0 otherwise) and x1, x2, ... are
   a set of predictor variables thought to influence detection
}
  \item{sdat}{
   sightability data set containing detections (Y/N) and covariates used to model detection probabilities
}
  \item{odat}{
   observational survey data set containing the following variable names (\emph{stratum, subunit, total}) along with 
   the same covariates used to model detection probabilities (each record corresponds to an independently sighted group of animnals).  \emph{stratum} = stratum identifier (will take on a single value for non-stratified surveys); \emph{subunit} = numeric plot unit identifier; 
   \emph{total} = total number of observed animals (for each independently sighted group of animals). 
 }
  \item{sampinfo}{
    data set containing sampling information pertaining to the observational survey.  Must include the following variables (\emph{stratum,nh, Nh}). 
    \emph{stratum} = stratum identifier (must take on the same values as \emph{stratum} variable in observational data set),
    \emph{nh} = number of sampled units in stratum h,
    \emph{Nh} = number of population units in stratum h;  note (this dataset will contain a single record for non-stratified designs).   

 }
 \item{method}{
   method for estimating variance of the abundance estimator.  Should be one of ("Wong", "SS").  See details for more information.
  }
  \item{logCI}{
    Boolean variable, default (= TRUE), indicates the confidence interval should be constructed under the assumption that
      (tau^=T) is lognormally distributed, where T is the total number of animals observed (see details)
   }
  \item{alpha}{
     type I error rate for confidence interval construction
  }
  \item{Vm.boot}{
    Boolean variable, when = TRUE indicates a bootstrap should be used to estimate cov(theta[i,j],theta[i',j']), var/cov matrix of the expansion factors (1/detection prob)     
  }
  \item{nboot}{
   number of bootstrap replicates to use if Vm.boot=TRUE
}
  \item{bet}{ 
   regression parameters (if the sightability model is not to be fit by Sight.Est).  Make sure the order is consistent with the specification in the "form" argument.
  }
  \item{varbet}{
   variance-covariance matrix for beta^ (if the sightability model is not to be fit by Sight.Est).Make sure the order is consistent with the specification in the "form" argument.
  }
}

\details{
  Variance estimation methods:  method = Wong implements the variance estimator from Wong (1996) and is the recommended approach.
  Method = SS implements the variance estimator of Steinhorst and Samuel (1989), with a modification detailed in the Appendix of Samuel et al. (1992).

  Estimates of the variance may be biased low when the number of test trials used to estimate model parameters is small (see Wong 1996, Fieberg and Guidice 2008).
  A bootstrap can be used to aid the estimation process by specifying Vm.boot=TRUE [note: this method is experimental, and can be time intensive].

  Confidence interval construction:  often the sampling distribution of tau^ is skewed right.  If logCI=TRUE,
  the confidence interval for tau^ will be constructed under an assumption that log(tau^-T), where T is the total number of animals seen,
  is lognormally distributed.  In this case, the upper and lower limits are constructed as follows [see Wong(1996, p. 64-67)]:  

  LCL = T + [(tau^-T)/C]*sqrt(1+cv^2),  UCL = T+[(tau^-T)*C]*sqrt(1+cv^2),  where cv^2 = var(tau^)/(tau^-T)^2 and C = exp[z[alpha/2]*sqrt{ln(1+cv^2)}].
  
}
\value{
  An object of class \code{sightest}, a list that includes the following elements:
  \item{sight.model}{the fitted sightability model}
  \item{est}{abundance estimate [tau.hat] and its estimate of uncertainty [Vartot] as well as variance components due to 
   sampling [Varsamp], detection [VarSight], and model uncertainty [VarMod]}
  The list also includes the original test trial and operational survey data, sampling information, and information needed to construct 
  a confidence interval for the population estimate. 
}
\references{

Fieberg, John and Giudice, John. 2008 Variance of Stratified Survey Estimators With Probability of Detection Adjustments. Journal of Wildlife Management 72:837-844. 
   
 Samuel, Michael D. and Steinhorst, R. Kirk and Garton, Edward O. and Unsworth, James W. 1992. 
     Estimation of Wildlife Population Ratios Incorporating Survey Design and Visibility Bias.
     Journal of Wildlife Management 56:718-725.

 Steinhorst, R. K., and M.D. Samuel. 1989.  Sightability adjustment methods for aerial surveys of wildlife populations.  Biometrics 45:415-425.

 Wong, C. 1996.  Population size estimation using the modified Horvitz-Thompson estimator with estimated sighting probabilities.  Dissertation, Colorado State University, Fort Collins, USA.

}
\author{
John Fieberg, Wildlife Biometrician, Minnesota Department of Natural Resources
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(obs.m) # observational survey data
data(exp.m) # experimental survey data
data(sampinfo.m) # information on sampling rates
 
## Estimate population size in 2007 only
 sampinfo<-sampinfo.m[sampinfo.m$year==2007,]
 Sight.Est(observed~ voc, odat=obs.m[obs.m$year==2007,], sdat=exp.m, sampinfo, method="Wong", logCI=TRUE, alpha=0.05, Vm.boot=FALSE) 


## BELOW CODE IS SOMEWHAT TIME INTENSIVE (fits models using 2 variance estimators to 3 years of data)
## Estimate population size for 2004-2007
## Compare Wong's and Steinhorst and Samuel variance estimators

Est.W<-Est.SS<-matrix(NA,4,3)
count<-1
for(i in 2004:2007){
 sampinfo<-sampinfo.m[sampinfo.m$year==i,]
## Wong's variance estimator 
 z<- Sight.Est(observed~ voc, odat=obs.m[obs.m$year==i,], sdat=exp.m, sampinfo, method="Wong", logCI=TRUE, alpha=0.05, Vm.boot=FALSE) 
 Est.W[count,]<-unlist(summary(z)) 
 
## Steinhorst and Samuel (with Samuel et al. 1992 modification) 
 y<- Sight.Est(observed~ voc, odat=obs.m[obs.m$year==i,],  sdat=exp.m,sampinfo, method="SS")
 Est.SS[count,]<-unlist(summary(y)) 
 
 count<-count+1
}  
rownames(Est.W)<-rownames(Est.SS)<-2004:2007
colnames(Est.W)<-colnames(Est.SS)<-c("Nhat","LCL","UCL")
Est.W
Est.SS

\dontrun{
require(gplots)
par(mfrow=c(1,1))
 plotCI(2004:2007, Est.W[,1], ui=Est.W[,3], li=Est.W[,2], type="l", xlab="", ylab="Population estimate", xaxt="n")
 plotCI(2004:2007, Est.SS[,1], ui=Est.SS[,3], li=Est.SS[,2], type="l", lty=2, add=True)
 axis(side=1, at=2004:2007, labels=2004:2007)  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{~detection model}
\keyword{ ~sightability model }
\keyword{~ wildlife survey}
