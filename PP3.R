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
RunNumber <- substr(FoldNames[nFold], 1, nchar(FoldNames[nFold])-4)

fLogDV <- LogDV2()

sdtab =  read.table("sdtab", header=TRUE, skip=1, na.strings="***********")
nRec <- length(sdtab[,1])
FDATA <- read.csv("FDATA.CSV")
FDATA[is.na(FDATA)] <- 0
PrepPDF("S3-Predictions.PDF")
#ToLD: Time of Lastest Dosing Start
#TaLD: Time after Latest Dosing Start


LastID <- 0
ToLD <- vector()
TaLD <- vector()
DOCC <- vector() # Dosing Occasion Number
NewID <- vector()

for (i in 1:nRec) {
  CurID <- FDATA[i,"ID"]
  if (LastID != CurID) {
    pToLD <- 0
    DOCC <- -1
    if (length(intersect(names(FDATA),"ADDL")) == 1) {
      DosingHist <- FDATA[FDATA[,"ID"]==CurID & FDATA[,"AMT"] > 0 & FDATA[,"MDV"]==1, c("AMT","TIME","II","ADDL")]
      nDoseRec <- length(DosingHist[,"AMT"])
      for (j in 1:nDoseRec) {
        cADDL <- DosingHist[j,"ADDL"]
        if (cADDL > 0) {
          cAMT <- DosingHist[j,"AMT"]
          cTIME <- DosingHist[j,"TIME"]
          cII <- DosingHist[j,"II"]
          for (k in 1:cADDL) {
            DosingHist <- rbind(DosingHist, c(cAMT, cTIME + k*cII, NA, NA))
          }
        }
      }
      DosingHist2 <- DosingHist[order(DosingHist[,"TIME"]),c("AMT","TIME")]
    }
  }


  if (FDATA[i,"AMT"] > 0 & FDATA[i,"MDV"] == 1) {
    cToLD <- FDATA[i,"TIME"]
    DOCC <- DOCC + 1
  } else {
    if (length(intersect(names(FDATA),"ADDL")) == 1) {
      CurTime <- FDATA[i,"TIME"]
      cToLD <- max(DosingHist2[DosingHist2[,"TIME"] < CurTime,"TIME"])
    } else {
      cToLD <- pToLD
    }
  }
  ToLD[i] <- cToLD
  TaLD[i] <- FDATA[i,"TIME"] - cToLD
  NewID[i] <- FDATA[i,"ID"] + max(0,DOCC)/100
  LastID <- CurID
  pToLD <- cToLD
}

FITDATA <- cbind(NewID, TaLD, sdtab[,c("MDV","DV","PRED","IPRE")])
colnames(FITDATA) <- c("ID","TALD","MDV","DV","PRED","IPRE")

  if (! is.null(FITDATA$MDV)) {
    FITDATA <- FITDATA[FITDATA$MDV==0,]
  }

  n.CMT <- 1
  if(! is.null(FITDATA$CMT)) {
    CMTs <- unique(FITDATA$CMT)
    n.CMT <- length(CMTs)
  }

  if (n.CMT == 1) {
    attach(FITDATA)
    par(defpar)

    par(oma=c(1,1,3,1), mfrow=c(2,1), lty=1)
    DxPlotPost(PRED, DV, mat=FITDATA, xlbl="PRED", ylbl="DV", smooth="F")
    DxPlotPost(IPRE, DV, mat=FITDATA, xlbl="IPRE", ylbl="DV", smooth="F")
    mtext(outer=T, side=3, paste("Spaghetti Plot of Model", toupper(RunNumber)))

    par(oma=c(1,1,3,1), mfrow=c(3,1), lty=1)
    DxPlotPost(TALD, DV, mat=FITDATA, xlbl="Time after Latest Dose", ylbl="DV", smooth="F")
    DxPlotPost(TALD, PRED, mat=FITDATA, xlbl="Time after Latest Dose", ylbl="PRED", smooth="F")
    DxPlotPost(TALD, IPRE, mat=FITDATA, xlbl="Time after Latest Dose", ylbl="IPRE", smooth="F")
    mtext(outer=T, side=3, paste("Spaghetti Plot of Model", toupper(RunNumber)))

    
    par(oma=c(1,1,3,1), mfrow=c(3,1), lty=1)
    if (fLogDV==FALSE) {
      DxPlotPost(TALD, DV, mat=FITDATA, xlbl="Time after Latest Dose", ylbl="DV", smooth="F", Log="y")
      DxPlotPost(TALD, PRED, mat=FITDATA, xlbl="Time after Latest Dose", ylbl="PRED", smooth="F", Log="y")
      DxPlotPost(TALD, IPRE, mat=FITDATA, xlbl="Time after Latest Dose", ylbl="IPRE", smooth="F", Log="y")
      mtext(outer=T, side=3, paste("Spaghetti Plot of Model", toupper(RunNumber), "(log scale on Y)"))
    } else {
      DxPlotPost(TALD, exp(DV), mat=FITDATA, xlbl="Time after Latest Dose", ylbl="DV", smooth="F")
      DxPlotPost(TALD, exp(PRED), mat=FITDATA, xlbl="Time after Latest Dose", ylbl="PRED", smooth="F")
      DxPlotPost(TALD, exp(IPRE), mat=FITDATA, xlbl="Time after Latest Dose", ylbl="IPRE", smooth="F")
      mtext(outer=T, side=3, paste("Spaghetti Plot of Model", toupper(RunNumber), "(original scale on Y)"))
    }
    detach(FITDATA)

  } else {
    for (i in 1:n.CMT) {
      FITDATA2 <- FITDATA[FITDATA$CMT == CMTs[i],]
      attach(FITDATA2)
      par(defpar)
      par(oma=c(1,1,3,1), mfrow=c(2,1), lty=1)
      DxPlotPost(PRED, DV, mat=FITDATA2, xlbl="PRED", ylbl="DV", smooth="F")
      DxPlotPost(IPRE, DV, mat=FITDATA2, xlbl="IPRE", ylbl="DV", smooth="F")
      mtext(outer=T, side=3, paste("Spaghetti Plot of Model", toupper(RunNumber)))

      par(oma=c(1,1,3,1), mfrow=c(3,1), lty=1)
      DxPlotPost(TALD, DV, mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="DV", smooth="F")
      DxPlotPost(TALD, PRED, mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="PRED", smooth="F")
      DxPlotPost(TALD, IPRE, mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="IPRE", smooth="F")
      mtext(outer=T, side=3, paste("Spaghetti Plot of Model ", toupper(RunNumber), ", CMT=", CMTs[i], sep=""))

      par(oma=c(1,1,3,1), mfrow=c(3,1), lty=1)
      if (fLogDV==FALSE) {
        DxPlotPost(TALD, DV, mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="DV", smooth="F", Log="y")
        DxPlotPost(TALD, PRED, mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="PRED", smooth="F", Log="y")
        DxPlotPost(TALD, IPRE, mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="IPRE", smooth="F", Log="y")
        mtext(outer=T, side=3, paste("Spaghetti Plot of Model", toupper(RunNumber), "(log scale on Y)"))
      } else {
        DxPlotPost(TALD, exp(DV), mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="DV", smooth="F")
        DxPlotPost(TALD, exp(PRED), mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="PRED", smooth="F")
        DxPlotPost(TALD, exp(IPRE), mat=FITDATA2, xlbl="Time after Latest Dose", ylbl="IPRE", smooth="F")
        mtext(outer=T, side=3, paste("Spaghetti Plot of Model", toupper(RunNumber), "(original scale on Y)"))
      }
      detach(FITDATA2)
    }
  }

IDs <- unique(sdtab[,"ID"])
nID <- length(IDs)
resRun <- vector()

  tx <- sdtab[sdtab[,"MDV"]==0,"DV"]
  ty <- sdtab[sdtab[,"MDV"]==0,"PRED"]
  tz <- sdtab[sdtab[,"MDV"]==0,"IPRE"]
  maxy <- max(tx,ty,tz)

par(defpar)


for (i in 1:nID) {
  ID <- IDs[i]

  w <- sdtab[sdtab[,"ID"]==ID & sdtab[,"MDV"]==0,"TIME"]
  if (length(w) > 0) {

    x <- sdtab[sdtab[,"ID"]==ID & sdtab[,"MDV"]==0,"DV"]
    y <- sdtab[sdtab[,"ID"]==ID & sdtab[,"MDV"]==0,"PRED"]
    z <- sdtab[sdtab[,"ID"]==ID & sdtab[,"MDV"]==0,"IPRE"]

    if (fLogDV==FALSE) {
      w1 <- w[x>0]
      x1 <- x[x>0]
      y1 <- y[x>0]
      z1 <- z[x>0]

      w2 <- w[x>0 & y>0 & z>0]
      x2 <- x[x>0 & y>0 & z>0]
      y2 <- y[x>0 & y>0 & z>0]
      z2 <- z[x>0 & y>0 & z>0]
    } else {
      w2 <- w1 <- w
      x2 <- x1 <- x
      y2 <- y1 <- y
      z2 <- z1 <- z
    }
    
    maxx <- 1.05*max(w)
    maxy1 <- max(x1,y1,z1)
    miny2 <- min(x2,y2,z2)

    par(oma=c(1,1,3,1),mfrow=c(2,1),lty=1)

    if (fLogDV==FALSE) {
      plot(w1,x1, type="p", cex=0.7, xlim=c(0,maxx), ylim=c(0,maxy1), xlab="Time (Circle=DV, Dotted Line=PRED, Line=IPRE)", ylab="DV,PRED,IPRE")
    } else {
      plot(w1,x1, type="p", cex=0.7, xlim=c(0,maxx), ylim=c(miny2, maxy1), xlab="Time (Circle=DV, Dotted Line=PRED, Line=IPRE)", ylab="DV,PRED,IPRE (log scale)")
    }
    lines(w1,y1, type="b", cex=0.3, col="blue", lty=3)
    lines(w1,z1, type="b", cex=0.5,  col="red")

    if (length(intersect(names(FDATA),"ADDL")) == 1) {
      DosingHist <- FDATA[FDATA[,"ID"]==ID & FDATA[,"AMT"] > 0 & FDATA[,"MDV"]==1, c("AMT","TIME","II","ADDL")]
      nDoseRec <- length(DosingHist[,"AMT"])
      for (j in 1:nDoseRec) {
        cAMT <- DosingHist[j,"AMT"]
        cTIME <- DosingHist[j,"TIME"]
        cII <- DosingHist[j,"II"]
        cADDL <- DosingHist[j,"ADDL"]
        for (k in 0:cADDL) text(cTIME + k*cII, 0, cAMT, adj=c(0,1), cex=0.7)
      }
    } else {
      a <- FDATA[FDATA[,"ID"]==ID & FDATA[,"MDV"]==1 & FDATA[,"AMT"] > 0,"AMT"]
      d <- FDATA[FDATA[,"ID"]==ID & FDATA[,"MDV"]==1 & FDATA[,"AMT"] > 0,"TIME"]
      for (j in 1:length(d)) text(d[j], 0, a[j], adj=c(0,1), cex=0.7)
    }

    mtext(outer=F, side=1, line=4, cex=0.8, "*Numbers within graph are dosing amounts.")

    if (fLogDV==FALSE) {
      plot(w2,x2, type="p", cex=0.7, xlim=c(0,maxx), ylim=c(miny2,maxy1), xlab="Time (Circle=DV, Dotted Line=PRED, Line=IPRE)", ylab="DV,PRED,IPRE (log interval)", log="y")
      lines(w2,y2, type="b", cex=0.3, col="blue", lty=3)
      lines(w2,z2, type="b", cex=0.5, col="red")
    } else {
      plot(w2,exp(x2), type="p", cex=0.7, xlim=c(0,maxx), ylim=c(exp(miny2),exp(maxy1)), xlab="Time (Circle=DV, Dotted Line=PRED, Line=IPRE)", ylab="DV,PRED,IPRE (original scale)")
      lines(w2,exp(y2), type="b", cex=0.3, col="blue", lty=3)
      lines(w2,exp(z2), type="b", cex=0.5, col="red")
    }
    mtext(outer=T, side=3, paste("Individiual Fitting Curve, ID =", IDs[i]))
  }
}


ClosePDF()
