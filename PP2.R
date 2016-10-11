###############################################################
#     R script for Post-processing of NONMEM run
#
#                    Programmed by Kyun-Seop Bae
#                    Date : 2013.1.5
#
# The following script is applicable only when it follows NONMEM programming
# convention of Kyun-Seop Bae
#
# Usage: >rcmd BATCH postproc.R in the working directory
#
# Overview of Single-Run Based Diagnostics

#
# 1) Objective Function Value Related Diagnostics
#    Total OFV, AICc
#    Condition number
#    Box plot, histogram, QQ plot, normality(S-W) test, summary statistics of Individual OFV per DV
#
# 2) Wald Type Tests on Point Estimates and Standard Errors
#    Significance, Unitary
#    Plausibility of Omega matrix
#
# 3) Observation Prediction Based Diagnostics:
#    Superposed fitting curve (DV,PRED,IPRE vs TaLD) including log="y"
#    (DV) vs (PRED, IPRE)
#    Individual Fitting Curve (DV,PRED,IPRE vs TIME for each individual)
#      (Mark also dosing event !)
#    including log="y", runtest result on residual, individual OFV, OFV per DV
#    (DV, PRED, IPRE) vs (TIME after Latest Dosing Start; TaLD)
#    In one page and in each page(including log scale)
#
# 4) Residual Based Diagnostics:
#    (WRES, IWRE, CWRES and their absolute values) vs
#    (PRED, IPRE, independent variables like TIME, DOSE)
#    Absolute residuals vs PRED,IPRE,TIME
#    Histogram and Normality of RES, WRES, IWRE, CWRES
#    Epsilon Shrinkage
#    Mean Percent Residual
#    Run test on RES, WRES, IWRE, CWRE
#
# 5) EBE Based Diagnostics:
#    Histogram, Q-Q plot, Normality (,and shrinkage) of EBE (in one graph)
#      and Summary Statistics (including SD)
#    Population Shrinkage of EBE
#    EBE vs covariates
#       ALL and Each plot
#       Correlation Matrix
#       Multiple Linear Regression
#    Histogram and Normality of Individual PK Parameters
#      and Summary Statistics (including SD, CV)
#    SE, CV, Individual Shrinkage of EBE
#
# Appendix 1: Input Data Summary
#    Data Dependency, Normalization Check
#    Summary of Records: Total, Individual, Dosing History
#    Summary Statistics of Categorical and Continuous Variables
#
# Appendix 2: Control file with numbers and Output File
#
###############################################################

#library(compiler)
source('C:/NMa/UtilLib.R')

defpar <- par()
CurWorkDir <- getwd()
FoldNames <- strsplit(CurWorkDir,"/")[[1]]
nFold <- length(FoldNames)

CtlName = substr(FoldNames[nFold], 1, nchar(FoldNames[nFold])-4)
XML = readLines(paste(CtlName,".xml",sep=""))

EXT = read.table(paste(CtlName,".ext",sep=""), skip=1, header=TRUE)
EXT = EXT[EXT[,"ITERATION"] >= 0,]
ColNamesAll = colnames(EXT)
nThetaAll = 0
nEtaAll = 0
nEpsAll = 0
for (i in 1:length(ColNamesAll)) {
  if(substr(ColNamesAll[i],1,5)=="THETA") nThetaAll = nThetaAll + 1
  if(substr(ColNamesAll[i],1,5)=="SIGMA") nEpsAll = nEpsAll + 1
  if(substr(ColNamesAll[i],1,5)=="OMEGA") nEtaAll = nEtaAll + 1
}
nEtaAll = (sqrt(1 + 8*nEtaAll) - 1)/2
nEpsAll = (sqrt(1 + 8*nEpsAll) - 1)/2

THETA = as.double(BtwTagVals("nm:theta", XML))
THETASE = as.double(BtwTagVals("nm:thetase", XML))
OMEGA = BtwTagMat("omega", XML, nEtaAll)
OMEGAse = BtwTagMat("omegase", XML, nEtaAll)
SIGMA = BtwTagMat("sigma", XML, nEpsAll)
SIGMAse = BtwTagMat("sigmase", XML, nEpsAll)

Thetas = cbind(THETA, THETASE)
OMa = rbind(OMEGA, OMEGAse)
SGa = rbind(SIGMA, SIGMAse)

################################################################################
# Summary 2 - Parameters
################################################################################


# 2) Wald Type Tests on Point Estimates and Standard Errors
#    Significance, Unitary
#    Plausibility of Omega matrix

nThAll <- length(Thetas[,1])                 
Fixed <- vector()
Unfixed <- vector()

for (i in 1:nThAll) {
  if (Thetas[i,2] == 1e+10) {              
    Fixed <- c(Fixed, i)               
  } else {
    Unfixed <- c(Unfixed, i)           
  }
}
nFixedTh <- length(Fixed)
nUnfixedTh <- length(Unfixed)
ThRowName <- character()
for (i in 1:nThAll) {
  ThRowName <- c(ThRowName, paste("Theta", i))
}
rownames(Thetas) <- ThRowName
colnames(Thetas) <- c("Point Estitmate","Standard Error")
LL <- Thetas[,1] - 2*Thetas[,2]
UL <- Thetas[,1] + 2*Thetas[,2]
ZERO <- Thetas[,2] / abs(Thetas[,1]) > 0.5
ONE <- (Thetas[,1] - 2*Thetas[,2] - 1) * (Thetas[,1] + 2*Thetas[,2] - 1) < 0 | (Thetas[,1] - 2*Thetas[,2] + 1) * (Thetas[,1] + 2*Thetas[,2] + 1) < 0

Thetas <- cbind(Thetas, LL, UL, ZERO, ONE)
UnfixedThetas <- Thetas[Unfixed,]

nEta <- length(OMa[1,])
OM <- OMa[1:nEta,]
SeOM <- OMa[(nEta+1):(2*nEta),] #
EtaNames <- character()

for (i in 1:nEta) {
  EtaNames <- c(EtaNames, paste("Eta",i))
}
rownames(OM) <- EtaNames
colnames(OM) <- EtaNames
rownames(SeOM) <- EtaNames
colnames(SeOM) <- EtaNames

RSEOM <- SeOM / abs(OM) * 100

for (i in 1:nEta) {
  for (j in i:nEta) {
    if (j > i) OM[i,j] <- OM[i,j] / sqrt(OM[i,i]*OM[j,j])
  }
}

nEps <- length(SGa[1,])
SG <- SGa[1:nEps,]
SeSG <- SGa[(nEps+1):(2*nEps),] #

if (nEps > 1) {
  EpsNames <- character()

  for (i in 1:nEps) {
    EpsNames <- c(EpsNames, paste("Eps",i))
  }
  rownames(SG) <- EpsNames
  colnames(SG) <- EpsNames
  rownames(SeSG) <- EpsNames
  colnames(SeSG) <- EpsNames

  for (i in 1:nEps) {
    for (j in i:nEps) {
      if (j > i) SG[i,j] <- SG[i,j] / sqrt(SG[i,i]*SG[j,j])
    }
  }
}

PrepPDF("S2-Parameters.PDF")

AddPage()

PrinTxt(1,1,"Summary 2 - Parameters", Cex=1.2)
PrinTxt(3,1,"Thetas",Cex=1.0)
PrinTxt(5,3,paste("Number of All Thetas     :",nThAll))
PrinTxt(6,3,paste("Number of Fixed Thetas   :",nFixedTh))
PrinTxt(7,3,paste("Number of Unfixed Thetas :",nUnfixedTh))

if (nFixedTh > 0) {
  PrinTxt(9,2,"Fixed Theta Values", Cex=0.9)
  for (i in 1:nFixedTh) {
    PrinTxt(9+i,5,paste("Theta",Fixed[i],":",Thetas[Fixed[i],1]))
  }
}

PrinTxt(9+nFixedTh+2,2,"Estimated Thetas", Cex=0.9)

sUnfixed <- capture.output(UnfixedThetas)
for (i in 1:length(sUnfixed)) {
  PrinTxt(9+nFixedTh+2+i,5,sUnfixed[i])
}

PrinTxt(nThAll+13.5, 6, "*LL  : Lower Limit", Cex=0.7)
PrinTxt(nThAll+14, 6, " UL  : Upper Limit", Cex=0.7)
PrinTxt(nThAll+14.5, 6, " ZERO: Is this maybe zero? 0:No, 1:Yes", Cex=0.7)
PrinTxt(nThAll+15, 6, " ONE : Is this maybe one?  0:No, 1:Yes", Cex=0.7)


AddPage()
PrinTxt(3,1,"Omegas",Cex=1.0)
PrinTxt(5,3,paste("Number of Etas           :",nEta))

PrinTxt(7,2,"Omega Matrix", Cex=0.9)
sOM <- capture.output(OM)
for (i in 1:length(sOM)) {
  PrinTxt(8+i,5,sOM[i])
}
PrinTxt(nEta+10.5,6, "*Lower triangle is covariance matrix.", Cex=0.7)
PrinTxt(nEta+11,6, " Upper triangle is correlation matrix.", Cex=0.7)
PrinTxt(nEta+11.5,6, " Diagnoal elements are variances.", Cex=0.7)

PrinTxt(nEta+13,3,"Square root of diagonal elements (x100)")
for (i in 1:nEta) {
  PrinTxt(nEta+14,i*8,paste("Eta",i))
  PrinTxt(nEta+15,i*8,format(sqrt(OM[i,i])*100, digits=4))
}


PrinTxt(nEta+18,2,"Standard Error of Omega Matrix", Cex=0.9)
sSeOM <- capture.output(SeOM)
for (i in 1:length(sSeOM)) {
  PrinTxt(nEta+19+i,5,sSeOM[i])
}

PrinTxt(2*nEta+23,2,"Relative Standard Error(%) of Omega Matrix", Cex=0.9)
sRSEOM <- capture.output(RSEOM)
for (i in 1:length(sRSEOM)) {
  PrinTxt(2*nEta+24+i,5,sRSEOM[i])
}

PrinTxt(3*nEta+27,1,"Sigmas", Cex=1)
PrinTxt(3*nEta+29,3,paste("Number of Epsilons       :",nEps))
if (SG==1 & SeSG==1e+10) {
  PrinTxt(3*nEta+31,3,"Fixed as 1")
} else {
  sSG <- capture.output(SG)
  for (i in 1:length(sSG)) {
    PrinTxt(3*nEta+30+i,5,sSG[i])
  }
}
ClosePDF()


