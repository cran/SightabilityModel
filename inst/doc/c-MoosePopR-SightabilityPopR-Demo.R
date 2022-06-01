## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width=200)

library(car)
library(ggplot2)
library(kableExtra)
library(plyr)
library(readxl)
library(reshape2)
library(SightabilityModel)



## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Get the actual survey data
dir(system.file("extdata", package = "SightabilityModel"))

survey.data <- readxl::read_excel(system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
                                  sheet="BlockData",
                                  skip=1,.name_repair="universal")
# Skip =xx says to skip xx lines at the top of the worksheet. In this case skip=1 implies start reading in line 2 of
# the worksheet.
# .name_repair="universal" changes all variable names to be compatible with R, e.g., no spaces, no special characters, etc

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Check the variable names in the input data
names(survey.data)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# convert all NA in moose counts to 0
survey.data$Bulls          [ is.na(survey.data$Bulls)         ] <- 0
survey.data$Lone.Cows      [ is.na(survey.data$Lone.Cows)     ] <- 0
survey.data$Cow.W.1...calf [ is.na(survey.data$Cow.W.1...calf)] <- 0
survey.data$Cow.W.2.calves [ is.na(survey.data$Cow.W.2.calves)] <- 0
survey.data$Lone...calf    [ is.na(survey.data$Lone...calf)   ] <- 0
survey.data$Unk.Age.Sex    [ is.na(survey.data$Unk.Age.Sex)   ] <- 0
#head(survey.data)

## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
# check the total moose read in vs computed number
survey.data$myNMoose <- survey.data$Bulls +
                       survey.data$Lone.Cows+
                       survey.data$Cow.W.1...calf*2+
                       survey.data$Cow.W.2.calves*3+
                       survey.data$Lone...calf+
                       survey.data$Unk.Age.Sex

ggplot(data=survey.data, aes(x=myNMoose, y=NMoose))+
  ggtitle("Compare my count vs. count on spreadsheet")+
  geom_point(position=position_jitter(h=.1,w=.1))+
  geom_abline(intercept=0, slope=1)

## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
xtabs(~Stratum, data=survey.data, exclude=NULL, na.action=na.pass)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
survey.data$Cows <- survey.data$Lone.Cows + survey.data$Cow.W.1...calf + survey.data$Cow.W.2.calves

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
head(as.data.frame(survey.data[,c("Block.ID","Stratum","Bulls","Lone.Cows","Cow.W.1...calf",
                    "Cow.W.2.calves","Cows","Lone...calf","Unk.Age.Sex","NMoose")]))

## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get the area of each block
survey.block.area <- readxl::read_excel(
                        system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
                        sheet="BlockArea",
                        skip=1,.name_repair="universal")
head(survey.block.area)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
names(survey.block.area)
names(survey.data)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Check that every surveyed block has an area. The setdiff() should return null.
setdiff(survey.data$Block.ID, survey.block.area$Block.ID)

# It is ok if the block area file has areas for blocks not surveyed
setdiff(survey.block.area$Block.ID, survey.data$Block.ID)

## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
stratum.data <- readxl::read_excel(
      system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
      sheet="Stratum",
      skip=2,.name_repair="universal")
stratum.data

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# number of beta coefficients
nbeta <- readxl::read_excel(
   system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
   sheet="SightabilityModel",
   range="B2", col_names=FALSE)[1,1,drop=TRUE]
# Here Range="B2" refers to a SINGLE cell (B2) on the spreadsheet

# extract the names of the terms of the model
beta.terms <- unlist(readxl::read_excel(
     system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
     sheet="SightabilityModel",
     range=paste0("B3:",letters[1+nbeta],"3"), 
     col_names=FALSE, col_type="text")[1,,drop=TRUE], use.names=FALSE)
# Here the range is B3: C3 in the case of nbeta=2 etc
cat("Names of the variables used in the sightability model:", beta.terms, "\n")

# extract the beta coefficients
beta <- unlist(readxl::read_excel(
    system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
    sheet="SightabilityModel",
    range=paste0("B4:",letters[1+nbeta],"4"), 
    col_names=FALSE, col_type="numeric")[1,,drop=TRUE], use.names=FALSE)
# Here the range is B4: C4 in the case of nbeta=2 etc
cat("Beta coefficients used in the sightability model:", beta, "\n")

# extract the beta covariance matrix
beta.cov <- matrix(unlist(readxl::read_excel(
    system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
    sheet="SightabilityModel",
    range=paste0("B5:",letters[1+nbeta],4+nbeta), 
    col_names=FALSE, col_type="numeric"), use.names=FALSE), ncol=nbeta, nrow=nbeta)
cat("Beta covariance matrix used in the sightability model:\n")
beta.cov



## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
Cover <- data.frame(intercept=1, VegCoverClass=1:5)
Cover$logit.p <- as.matrix(Cover[,1:nbeta ]) %*% beta
Cover$logit.p.se <- diag(sqrt( as.matrix(Cover[,1:nbeta ]) %*% beta.cov %*% t(as.matrix(Cover[,1:nbeta ]))))
Cover$p       <- 1/(1+exp(-Cover$logit.p))
Cover$p.se    <- Cover$logit.p.se * Cover$p * (1-Cover$p)
Cover$expansion <- 1/Cover$p
Cover$expansion.se <- Cover$p.se / Cover$p^2

temp <- Cover[, -1]
temp[,2:7]<- round(temp[,2:7],3)
#temp

kable(temp, row.names=FALSE,
      caption="Estimated sightability of groups and expansion by Vegetation Cover Class",
      col.names=c("Vegetation Cover Class","logit(p)","SE logit(p)","p","SE(p)","Expansion Factor","SE(EF)")) %>%
      column_spec(column=c(1:7),         width="2cm") %>%
      kable_styling("bordered",position = "center", full_width=FALSE)



## ----echo=TRUE, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# convert the Veg Cover to Veg.Cover.Class

xtabs(~..Veg.Cover, data=survey.data, exclude=NULL, na.action=na.pass)
survey.data$VegCoverClass <- car::recode(survey.data$..Veg.Cover,
                                " lo:20=1; 20:40=2; 40:60=3; 60:80=4; 80:100=5")
xtabs(~VegCoverClass+..Veg.Cover, data=survey.data, exclude=NULL, na.action=na.pass)

## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
result <- NULL
result$"All.Density.UC" <- MoosePopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,density=~NMoose                         # which density
)
result$"All.Density.UC"

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
result$"All.Abund.UC" <- MoosePopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~NMoose                      # which density
)
result$"All.Abund.UC"

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
result$"Bulls.Abund.UC" <- MoosePopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~Bulls                      # which density
)
result$"Bulls.Abund.UC" 
  
result$"Cows.Abund.UC" <- MoosePopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~Cows                      # which density
)  
result$"Cows.Abund.UC"

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
result$"Bulls/Cow.UC" <- MoosePopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,numerator=~Bulls, denominator=~Cows    # which density
) 
result$"Bulls/Cow.UC"

## ----echo=TRUE, warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
names(result)

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
sightability.model <- ~VegCoverClass
sightability.beta  <-  c(4.2138, -1.5847)
sightability.beta.cov <- matrix(c(0.78216336,	-0.282,	-0.282,	0.11148921), nrow=2, ncol=2)


sightability.table <- data.frame(VegCoverClass=1:5,
                                 VegPercent=c("00-20","21-40","41-60","61-80","81-100"))
sightability.table$detect.prob <- SightabilityModel::compute.detect.prob(sightability.table, 
                                                      sightability.model, 
                                                      sightability.beta, 
                                                      sightability.beta.cov)

sightability.table$SCF <- SightabilityModel::compute.SCF(sightability.table, 
                                                      sightability.model, 
                                                      sightability.beta, 
                                                      sightability.beta.cov)


kable(sightability.table, row.names=FALSE,
      caption="Estimated sightability correction factor for each vegetation cover class",
      col.names=c("Veg Cover Class","Veg Cover %","Detection probability","Sightability Correction Factor"),
      digits=c(0,0, 3,2)) %>%
      kable_styling("bordered",position = "center", full_width=FALSE, latex_options = "HOLD_position")



## ----echo=FALSE,message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Look at correlations between total number of moose, bulls, and cows and block area
survey.block.data <- plyr::ddply(survey.data, 
                                 c("Block.ID","Stratum"),
                                 plyr::summarize,
                                 Bulls          = sum(Bulls,           na.rm=TRUE),
                                 Lone.cows      = sum(Lone.Cows,       na.rm=TRUE),
                                 Cow.W.1...calf = sum(Cow.W.1...calf,  na.rm=TRUE),
                                 Cow.W.2.calves = sum(Cow.W.2.calves,  na.rm=TRUE),
                                 Lone...calf    = sum(Lone...calf,     na.rm=TRUE),
                                 Unk.Age.Sex    = sum(Unk.Age.Sex,     na.rm=TRUE),
                                 Cows           = sum(Cows,            na.rm=TRUE),
                                 NMoose         = sum(NMoose,          na.rm=TRUE))
# add the area to the block totals
survey.block.data <- merge(survey.block.data, survey.block.area, all.x=TRUE)

# What is correlation between block area and number of moose etc
survey.block.data.melt <- reshape2::melt(survey.block.data,
                        id.vars=c("Stratum","Block.ID","Block.Area"),
                        measure.vars=c("Bulls","Lone.cows","Cow.W.1...calf","Cow.W.2.calves",
                                       "Lone...calf","Unk.Age.Sex","Cows","NMoose"),
                        variable.name="Classification",
                        value.name="N.Animals")

# find correlation between number of animals and area
res <- plyr::ddply(survey.block.data.melt, c("Stratum","Classification"), plyr::summarize,
                   corr=cor(N.Animals, Block.Area),
                   cv.N.Animals=sd(N.Animals)/mean(N.Animals),
                   cv.Area     =sd(Block.Area)/mean(Block.Area),
                   cut.off      = cv.Area/2/cv.N.Animals)

temp <- res[,c(1,2,3,6)]
temp[,3:4] <- round(temp[,3:4],2)
kable(temp, row.names=FALSE,
      caption="Estimated correlation between animal counts and block area",
      col.names=c("Stratum","Type of Animal","Corr","Cutoff")) %>%
      column_spec(column=c(1:2),         width="3cm") %>%
      column_spec(column=c(3:4),         width="1cm") %>%
      kable_styling("bordered",position = "center", full_width=FALSE) %>%
      footnote(threeparttable=TRUE,
               general=paste0("Cutoff is 0.5 ratio of sd/mean of area and abundance of each type of animal",
                              " and unless the correlation exceeds the cutoff, there is no advantage",
                              " in using the block area in the analysis"))


## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=survey.block.data.melt, aes(x=Block.Area, y=N.Animals, color=Stratum))+
  geom_point()+
  facet_wrap(~Classification, ncol=3, scales="free")+
  theme(legend.position=c(1,0), legend.justification=c(1,0))

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
select <- is.na(survey.data$VegCoverClass)
survey.data[select,]

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# change the missing values to cover class with highest sightability
survey.data$VegCoverClass[select] <- 1
survey.data[select,]


## ----echo=TRUE, message=TRUE, warning=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
result <- NULL
result$"All.Abund.C" <- SightabilityPopR(
      survey.data        = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~NMoose                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
temp <- result$"All.Abund.C"
temp[,-c(1:3,8)] <- round(temp[,-c(1:3,8)]+.00001,0)  # force to be numeric
temp

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# mimic impact of deleting observations with missing VegCoverClass
survey.data2 <- survey.data
survey.data2$NMoose[select] <- .001
wrong.way <- SightabilityPopR( # show impact of deleting observations but keeping # blocks the same
      survey.data        = survey.data2         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~NMoose                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
wrong.way

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
result$"All.Density.C" <- SightabilityPopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,density=~NMoose                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
temp<- result$"All.Density.C"
temp[,-(1:3)] <- round(temp[,-(1:3)]+.00001,2)  # force to be numeric
temp

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
result$"Bulls.Abund.C" <- result$"All.Density.C" <- SightabilityPopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,density=~Bulls                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
result$"Bulls.Abund.C"

result$"Cows.Abund.C" <- SightabilityPopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,density=~Cows                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
result$"Cows.Abund.C"

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
result$"Bulls/Cow.C" <- SightabilityPopR(
      survey.data       = survey.data         # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,numerator=~Bulls, denominator=~Cows                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
  
temp<- result$"Bulls/Cow.C"
temp[,-(1:3)] <- round(temp[,-(1:3)]+.00001,3)  # force to be numeric
temp

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
names(result)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# PSU's are split between the domains
xtabs(~Domain+Block.ID, data=survey.data, exclude=NULL, na.action=na.pass)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Subset the survey data
survey.data.A <- survey.data[ survey.data$Domain == "A",]

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Domain information for the population for each stratum
stratum.data.A <- readxl::read_excel(
      system.file("extdata", "SampleMooseSurveyData.xlsx", package = "SightabilityModel", mustWork=TRUE),
      sheet="Stratum-DomainA",
      skip=2,.name_repair="universal")

kable(stratum.data.A, row.names=FALSE,
      caption="Stratum totals for Domain A",
      col.names=c("Stratum","Stratum Area","Stratum # blocks")) %>%
      column_spec(column=c(1:3),         width="2cm") %>%
      kable_styling("bordered",position = "center", full_width=FALSE)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
All.Abund.A.corrected <- SightabilityPopR(
      survey.data       = survey.data.A         # raw data for domain A
      ,survey.block.area = survey.block.area    # area of each block
      ,stratum.data      = stratum.data.A       # stratum information for domain A
      ,abundance=~NMoose                        # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
temp <- All.Abund.A.corrected
temp[,-c(1:3,8)] <- round(temp[,-c(1:3,8)]+.00001,0)  # force to be numeric
temp[,1:10]


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
# Set number of moose to zero if not part of domain A
survey.data.A.z <- survey.data
survey.data.A.z$NMoose[ survey.data$Domain != "A"] <- 0

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
All.Abund.A.corrected.z <- SightabilityPopR(
      survey.data        = survey.data.A.z    # raw data with non-domain counts set to zero
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~NMoose                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
temp <- All.Abund.A.corrected.z
temp[,-c(1:3,8)] <- round(temp[,-c(1:3,8)]+.00001,0)  # force to be numeric
temp[,1:10]


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
All.Abund <- SightabilityPopR(
      survey.data        = survey.data        # raw data
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~NMoose                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
)
temp <- All.Abund
temp[,-c(1:3,8)] <- round(temp[,-c(1:3,8)]+.00001,0)  # force to be numeric
temp[,1:10]


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
All.Abund.vc <- plyr::llply(unique(survey.data$VegCoverClass), function(VegCover){
   # Set number of moose to zero if not part of domain A
   survey.data.z <- survey.data
   survey.data.z$NMoose[ survey.data$VegCoverClass != VegCover] <- 0  # set to zero
   #browser()
   All.Abund.vc <- SightabilityPopR(
      survey.data        = survey.data.z      # raw data that has been zeroed out
      ,survey.block.area = survey.block.area  # area of each block
      ,stratum.data      = stratum.data       # stratum information
      ,abundance=~NMoose                      # quantifies to estimate
      ,sight.formula     = observed ~ VegCoverClass # sightability mode;
      ,sight.beta        = beta                     # the beta coefficients for the sightability model
      ,sight.beta.cov    = beta.cov                 # the covariance matrix for the beta coefficients
      ,sight.var.method  = "Wong"                   # method  used to estimate the variances 
   )
   list(VegCoverClass=VegCover, est=All.Abund.vc)
})

# extract the total abundance

All.Abund.vc.df <- plyr::ldply(All.Abund.vc, function(x){
   #browser()
   res <- data.frame(VegCoverClass=as.character(x$VegCoverClass), stringsAsFactors=FALSE)
   res$estimate  <- x$est[x$est$Stratum==".OVERALL", c("estimate")]
   res$SE        <- x$est[x$est$Stratum==".OVERALL", c("SE"      )]
   res
   
})
All.Abund.vc.df <- plyr::rbind.fill( All.Abund.vc.df,
                                     data.frame(VegCoverClass=".OVERALL", estimate=sum(All.Abund.vc.df$estimate, stringAsFactors=FALSE)))
All.Abund.vc.df


