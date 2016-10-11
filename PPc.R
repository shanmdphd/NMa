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
CurWorkDir <- getwd()
FoldNames <- strsplit(CurWorkDir,"/")[[1]]
nFold <- length(FoldNames)
RunNumber <- substr(FoldNames[nFold], 1, nchar(FoldNames[nFold])-4)


################################################################################
# Appendix C - Individual PK Parameter
################################################################################
PrepPDF("SC-IndiPK.PDF")
IndiPK = unique(read.table("patab", skip=1, header=TRUE))

PKParas <- setdiff(colnames(IndiPK),"ID")
nPKPara <- length(PKParas)

RowPerPage <- 3
for (i in 1:nPKPara) {
  if (i%%RowPerPage == 1) {
    par(oma=c(1,1,3,1), mfrow=c(RowPerPage,3), lty=1)
  }
  var.data <- IndiPK[,PKParas[i]]
  sdvar <- sd(var.data)

  plot(0, 0, type = "n", ylim = c(1, 8), xlim = c(0,10), xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n")
  sData <- summary(var.data)
  text(0,8,paste("Minimum :", sData[1]), adj=0)
  text(0,7,paste("1st Qu. :", sData[2]), adj=0)
  text(0,6,paste("Median  :", sData[3]), adj=0)
  text(0,5,paste("Mean    :", sData[4]), adj=0)
  text(0,4,paste("3rd Qu. :", sData[5]), adj=0)
  text(0,3,paste("Maximum :", sData[6]), adj=0)
  text(0,2,paste("Std Dev :", format(sdvar, digits=4)), adj=0)
  text(0,1,paste("CV (%)  :", format(sdvar/sData[4]*100, digits=4)), adj=0)
  mtext(side=3,line=1,paste(PKParas[i]), adj=0)

  h.res <- hist(var.data, plot=F)
  d.res <- density(var.data, na.rm=T)
  h.rat <- max(h.res$counts)/max(h.res$density)
  xrange <- range(d.res$x)
  yrange <- c(0, max(h.res$counts, max(d.res$y)*h.rat))
  plot(h.res, xlim=xrange, ylim=yrange, xlab=PKParas[i], main="")
  lines(d.res$x, h.rat*d.res$y)
  
  if (sdvar > 0) {
    p.value <- format(shapiro.test(var.data)$p.value, digits=4)
    mtext(paste("Shapiro-Wilk test p-value =", p.value), cex=0.7, line=0, adj=0)
  }

  qqnorm(var.data, main="",ylab=PKParas[i], cex=0.4)

  if (i%%3==0 | i==nPKPara) {
    mtext(outer=T, side=3, "PK Parameter distribution")
  }
}

PrinMTxt(capture.output(IndiPK), Cex=0.8)
ClosePDF()

