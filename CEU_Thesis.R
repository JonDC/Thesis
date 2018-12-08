############################################ CEU Thesis Coding ################################################

LoadLibs = function() {
  library(dplyr)
  library(psych)
  library(car)
  library(stargazer)
  library(ggplot2)
  library(Hmisc)
  library(MASS)
  print("Ready chief!!")
}

LoadLibs()

setwd("D:/desktop/thesis/data")

# loading data, getting rid of scientific notation (25 variables, 845 observations)
MRC <- read.csv("MRCData.csv", sep = ",")
options(scipen = 999)

# cleaning data, getting rid of observations with missing data
MRCData <- as.data.frame(MRC)

colnames(MRCData)[1] <- "PSD.PNL"
colnames(MRCData)[2] <- "USR.ALDE"
colnames(MRCData)[3] <- "PSD.PMP"
colnames(MRCData)[4] <- "PNL.USR"
colnames(MRCData)[5] <- "PSD.ALDE"
colnames(MRCData)[6] <- "PNL.PMP"
colnames(MRCData)[7] <- "PSD.USR"
colnames(MRCData)[8] <- "USR.PMP"
colnames(MRCData)[9] <- "PNL.ALDE"
colnames(MRCData)[10] <- "ALDE.PMP"
colnames(MRCData)[11] <- "Main_Diff"
colnames(MRCData)[12] <- "Interest"
colnames(MRCData)[13] <- "PSD.Prob"
colnames(MRCData)[14] <- "PNL.Prob"
colnames(MRCData)[15] <- "USR.Prob"
colnames(MRCData)[16] <- "ALDE.Prob"
colnames(MRCData)[17] <- "PMP.Prob"
colnames(MRCData)[18] <- "UDMR.Prob"

# the final.cases functions allows obs removal based only on certain cases (columns)
MRCData[MRCData == 999] <- NA
MRC_final <- MRCData[complete.cases(MRCData[ , 1:18]),] # 333 oservations left, 25 variables
MRC2 <- MRC_final[-c(1), ] # dropping random obs as even number of obs is needed

# creating difference matrix and plotting the distances
# install.packages('caret')
set.seed(166)
samp_size <- floor(0.5 * nrow(MRC2)) # set sample size
Diff_Mat1 <- sample(seq_len(nrow(MRC2)), size = samp_size) # create randomized matrix
Mat1 <- MRC2[Diff_Mat1, ] # create 1st randomized matrix from data set (166 obs, 25 variables)
Mat2 <- MRC2[-Diff_Mat1, ] # create 2nd randomized matrix from data set (166 obs, 25 variables)

# creating distance matrix by subtracting Mat2 from Mat1 in all relevant variables
Rand1 <- Mat1[,!names(Mat1) %in% c("Main_Diff","X571_LOCATION","Age.group","Sex","City","County","Rural..Urban","user_id")]
Rand2 <- Mat2[,!names(Mat2) %in% c("Main_Diff","X571_LOCATION","Age.group","Sex","City","County","Rural..Urban","user_id")]
fullDistMat <- Rand1 - Rand2 # distance matrix for "interest' facet analysis

# analysis of MDS spce based on interest interpretation
# install.packages("smacof")
library(smacof)

intModelMat <- fullDistMat
intModelMat <- abs(as.matrix(intModelMat[,1:10])) # absolute distances

mds1 <- smacofSym(intModelMat,  ndim = 1, type="interval")
mds2 <- smacofSym(intModelMat,  ndim = 2, type="interval")
mds3 <- smacofSym(intModelMat,  ndim = 3, type="interval")
mds4 <- smacofSym(intModelMat,  ndim = 4, type="interval")
mds5 <- smacofSym(intModelMat,  ndim = 5, type="interval")
mds6 <- smacofSym(intModelMat,  ndim = 6, type="interval")
mds7 <- smacofSym(intModelMat,  ndim = 7, type="interval")
mds8 <- smacofSym(intModelMat,  ndim = 8, type="interval")
mds9 <- smacofSym(intModelMat,  ndim = 9, type="interval")
mds10 <- smacofSym(intModelMat,  ndim = 10, type="interval")
mds11 <- smacofSym(intModelMat,  ndim = 11, type="interval")

mds1
mds2
mds3
mds4
mds5
mds6
mds7
mds8
mds9
mds10
mds11