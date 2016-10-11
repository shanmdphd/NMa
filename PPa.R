###############################################################
#     R script for Post-processing of NONMEM run
#
#                    Programmed by Kyun-Seop Bae
#                    Date : 05APR2008
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
RunNumber <- substr(FoldNames[nFold], 1, nchar(FoldNames[nFold])-4)

XML = readLines(paste(CtlName,".xml",sep=""))

TagList = c(
" NO. OF DATA RECS IN DATA SET:",
" NO. OF DATA ITEMS IN DATA SET:"
)

TagIndex = rep(NA, length(TagList))

for (i in 1:length(TagList)) {
  TagIndex[i] = ParseOut(TagList[i],XML)
}

nRec = as.integer(TagIndex[1])
nItem = as.integer(TagIndex[2])

FDATA <- read.csv("FDATA.CSV")

VarStat <- NMVarStat(FDATA)
IDStat <- NMIDStat(FDATA)

################################################################################
# Appendix A - Summary Statistics of Input Data
################################################################################
PrepPDF("SA-Input.PDF")

if (length(IDStat[[1]][IDStat[[1]][,"nAMT"]==0,1]) > 0) {
  PrinMTxt(capture.output(IDStat[[1]][IDStat[[1]][,"nAMT"]==0,]), Header1="Individuals with no dosing")
}
if (length(IDStat[[1]][IDStat[[1]][,"nDV"]==0,1]) > 0) {
  PrinMTxt(capture.output(IDStat[[1]][IDStat[[1]][,"nDV"]==0,]), Header1="Individuals with no DV")
}

DetNames <- c("ID","TIME","MDV")
DuplData <- NULL

i <- 1
while (i < nRec) {
  cID <- FDATA[i,"ID"]
  cTIME <- FDATA[i,"TIME"]
  cMDV <- FDATA[i,"MDV"]

  tDATA <- FDATA[FDATA[,"ID"]==cID & FDATA[,"TIME"]==cTIME & FDATA[,"MDV"]==cMDV,]
  if (length(tDATA[,1]) > 1) {
    DuplData <- rbind(DuplData, tDATA)
  }
  i <- i + length(tDATA[,1])
}

options(width=128)
if (!is.null(DuplData)) {
  PrinMTxt(capture.output(DuplData), Header1="Potentially Harmful Records", Cex=0.6)
}

options(width=90)
PrinMTxt(capture.output(summary(FDATA)))
PrinMTxt(capture.output(VarStat))
PrinMTxt(capture.output(IDStat))


ClosePDF()

