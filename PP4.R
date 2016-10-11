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

PHI = read.table(paste(RunNumber,".phi",sep=""), skip=1, header=TRUE)
XML = readLines(paste(RunNumber,".xml",sep=""))

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

sdtab <-  read.table("sdtab", header=TRUE, skip=1, na.strings="***********")
FDATA <- read.csv("FDATA.CSV")
FDATA[is.na(FDATA)] <- 0

VarStat <- NMVarStat(FDATA)
IDStat <- NMIDStat(FDATA)
nDV <- VarStat["MDV","nZero"]

tOFV = PHI[,c("ID","OBJ")]
names(tOFV) <- c("ID","iOFV")

IDStat2 <- cbind(rownames(IDStat[[1]]),IDStat[[1]])
IDStat2Name <- names(IDStat2)
IDStat2Name[1] <- "ID"
names(IDStat2) <- IDStat2Name
IDStat3 <- merge(tOFV,IDStat2)
IDStat4 <- IDStat3[,-10]
IDStat4$OFVpDV <- IDStat4[,"iOFV"] / IDStat4[,"nDV"]
IDStat5 <- IDStat4[order(IDStat4[,"OFVpDV"],decreasing=T),]

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

resRun <- vector()

CWRES = sdtab[sdtab[,"MDV"]==0,c("ID","TIME","DV","PRED","IPRE","WRES","CWRES","IWRE")] #
colnames(CWRES) = c("ID","TIME","DV","PRED","IPRE","WRES","CWRE","IWRE")
FITDATA3 <- cbind(NewID[sdtab[,"MDV"]==0], TaLD[sdtab[,"MDV"]==0], CWRES[,c("PRED","IPRE","WRES","CWRE","IWRE")])
colnames(FITDATA3) <- c("ID","TALD","PRED","IPRE","WRES","CWRE","IWRE")

PrepPDF("S4-Residuals.PDF")

    attach(FITDATA3)
    par(defpar)

    par(oma=c(1,1,3,1), mfrow=c(3,2), lty=1)
    DxPlotPost(PRED, WRES, mat=FITDATA3, xlbl="PRED", ylbl=paste("WRES, SD =",format(sd(WRES, na.rm=T),digits=4)), smooth="T")
    DxPlotPost(TALD, WRES, mat=FITDATA3, xlbl="Time after latest dose", ylbl="WRES", smooth="T")
    DxPlotPost(IPRE, CWRE, mat=FITDATA3, xlbl="IPRE", ylbl=paste("CWRES, SD =",format(sd(CWRE, na.rm=T),digits=4)), smooth="T")
    DxPlotPost(TALD, CWRE, mat=FITDATA3, xlbl="Time after latest dose", ylbl="CWRES", smooth="T")
    DxPlotPost(IPRE, IWRE, mat=FITDATA3, xlbl="IPRE", ylbl=paste("IWRES, SD =",format(sd(IWRE, na.rm=T),digits=4)), smooth="T")
    DxPlotPost(TALD, IWRE, mat=FITDATA3, xlbl="Time after latest dose", ylbl="IWRES", smooth="T")
    mtext(outer=T, side=3, paste("Residuals of Model", toupper(RunNumber)))

    par(oma=c(1,1,3,1), mfrow=c(3,2), lty=1)
    DxPlotPost(PRED, abs(WRES), mat=FITDATA3, xlbl="PRED", ylbl="|WRES|", smooth="T")
    DxPlotPost(TALD, abs(WRES), mat=FITDATA3, xlbl="Time after latest dose", ylbl="|WRES|", smooth="T")
    DxPlotPost(IPRE, abs(CWRE), mat=FITDATA3, xlbl="IPRE", ylbl="|CWRES|", smooth="T")
    DxPlotPost(TALD, abs(CWRE), mat=FITDATA3, xlbl="Time after latest dose", ylbl="|CWRES|", smooth="T")
    DxPlotPost(IPRE, abs(IWRE), mat=FITDATA3, xlbl="IPRE", ylbl="|IWRES|", smooth="T")
    DxPlotPost(TALD, abs(IWRE), mat=FITDATA3, xlbl="Time after latest dose", ylbl="|IWRES|", smooth="T")
    mtext(outer=T, side=3, paste("Absolute Values of Residuals of Model", toupper(RunNumber)))


par(oma=c(1,1,3,1), mfrow=c(3,2), lty=1)

  var.data <- WRES
  h.res <- hist(var.data, plot=F)
  d.res <- density(var.data, na.rm=T)
  h.rat <- max(h.res$counts)/max(h.res$density)
  xrange <- range(d.res$x)
  yrange <- c(0, max(h.res$counts, max(d.res$y)*h.rat))
  plot(h.res, xlim=xrange, ylim=yrange, xlab=paste("WRES SD =", format(sd(var.data),digits=4)), main="")
  lines(d.res$x, h.rat*d.res$y)
  p.value <- format(shapiro.test(var.data)$p.value, digits=4)
  mtext(paste("Shapiro-Wilk test p-value =", p.value), cex=0.7, line=0, adj=0)

  qqnorm(var.data, main="",ylab="WRES",cex=0.4)

  var.data <- CWRE
  h.res <- hist(var.data, plot=F)
  d.res <- density(var.data, na.rm=T)
  h.rat <- max(h.res$counts)/max(h.res$density)
  xrange <- range(d.res$x)
  yrange <- c(0, max(h.res$counts, max(d.res$y)*h.rat))
  plot(h.res, xlim=xrange, ylim=yrange, xlab=paste("CWRES SD =", format(sd(var.data),digits=4)), main="")
  lines(d.res$x, h.rat*d.res$y)
  p.value <- format(shapiro.test(var.data)$p.value, digits=4)
  mtext(paste("Shapiro-Wilk test p-value =", p.value), cex=0.7, line=0, adj=0)

  qqnorm(var.data, main="",ylab="CWRES",cex=0.4)

  var.data <- IWRE
  h.res <- hist(var.data, plot=F)
  d.res <- density(var.data, na.rm=T)
  h.rat <- max(h.res$counts)/max(h.res$density)
  xrange <- range(d.res$x)
  yrange <- c(0, max(h.res$counts, max(d.res$y)*h.rat))
  plot(h.res, xlim=xrange, ylim=yrange, xlab=paste("IWRES SD =", format(sd(var.data),digits=4)), main="")
  lines(d.res$x, h.rat*d.res$y)
  p.value <- format(shapiro.test(var.data)$p.value, digits=4)
  mtext(paste("Shapiro-Wilk test p-value =", p.value), cex=0.7, line=0, adj=0)

  qqnorm(var.data, main="",ylab="IWRES",cex=0.4)

  mtext(outer=T, side=3, paste("Distribution of Residuals of Model", toupper(RunNumber)))

    detach(FITDATA3)


## Count Print
BTest.Res <- ResTest(CWRES)
options(width=110)
PrinMTxt(capture.output(BTest.Res), Header1="Test of residual counts using binomial distribution", Cex=0.8)

## Count Graph
BTest.Res2 <- BTest.Res[BTest.Res[,"Expected"]>0,]
n.points <- length(BTest.Res2[,1])
x <- BTest.Res2[,"Z-value"]
y11 <- BTest.Res2[,"CWRE Cnt"] / BTest.Res2[,"Expected"]
y12 <- BTest.Res2[,"IWRE Cnt"] / BTest.Res2[,"Expected"]
y13 <- BTest.Res2[,"WRES Cnt"] / BTest.Res2[,"Expected"]
y2 <- BTest.Res[,"LB"] / BTest.Res[,"Expected"]
y3 <- BTest.Res[,"UB"] / BTest.Res[,"Expected"]
y.max <- max(BTest.Res2[,"CWRE Cnt"], BTest.Res2[,"WRES Cnt"], BTest.Res2[,"IWRE Cnt"])

par(defpar)

# WRES
plot(0, 0, type="n", xlim=c(-3, 3), ylim=c(0, max(y12)), xlab="Z-value", ylab="Observed Count/Expected Count", main="Ratio of WRES Count to Expected Count")
for (i in 1:n.points) {
  if (y13[i] < y2[i] | y13[i] > y3[i]) text(x[i], y13[i], "W", col="red")
  else text(x[i], y13[i], "w")  
}
lines(x, y13, lty=3)
lines(BTest.Res[,"Z-value"], y2, lty=1)
lines(BTest.Res[,"Z-value"], y3, lty=1)
abline(h=1, lty=2)

# CWRES
plot(0, 0, type="n", xlim=c(-3, 3), ylim=c(0, max(y11)), xlab="Z-value", ylab="Observed Count/Expected Count", main="Ratio of CWRES Count to Expected Count")
for (i in 1:n.points) {
  if (y11[i] < y2[i] | y11[i] > y3[i]) text(x[i], y11[i], "C", col="red")
  else text(x[i], y11[i], "c")
}
lines(x, y11, lty=3)
lines(BTest.Res[,"Z-value"], y2, lty=1)
lines(BTest.Res[,"Z-value"], y3, lty=1)
abline(h=1, lty=2)

# IWRES
plot(0, 0, type="n", xlim=c(-3, 3), ylim=c(0, max(y12)), xlab="Z-value", ylab="Observed Count/Expected Count", main="Ratio of IWRES Count to Expected Count")
for (i in 1:n.points) {
  if (y12[i] < y2[i] | y12[i] > y3[i]) text(x[i], y12[i], "I", col="red")
  else text(x[i], y12[i], "i")
}
lines(x, y12, lty=3)
lines(BTest.Res[,"Z-value"], y2, lty=1)
lines(BTest.Res[,"Z-value"], y3, lty=1)
abline(h=1, lty=2)


options(width=95)

PrinMTxt(capture.output(CWRES[abs(CWRES[,"WRES"])>3, c("ID","TIME","DV","PRED","IPRE","WRES","CWRE","IWRE")]), Header1="Extreme WRES values larger than 3")

PrinMTxt(capture.output(CWRES[abs(CWRES[,"WRES"])>2, c("ID","TIME","DV","PRED","IPRE","WRES","CWRE","IWRE")]), Header1="Extreme WRES values larger than 2")

PrinMTxt(capture.output(CWRES[abs(CWRES[,"CWRE"])>2, c("ID","TIME","DV","PRED","IPRE","WRES","CWRE","IWRE")]), Header1="Extreme CWRE values larger than 2")

PrinMTxt(capture.output(CWRES[abs(CWRES[,"IWRE"])>2, c("ID","TIME","DV","PRED","IPRE","WRES","CWRE","IWRE")]), Header1="Extreme IWRE values larger than 2")

##

ID2s <- unique(CWRES[,"ID"])
nID2 <- length(ID2s)
for (i in 1:nID2) {
  ID <- ID2s[i]

#  AddPage()
  par(defpar)
#split.screen(scrnmat)
  par(oma=c(1,1,3,1),mfrow=c(3,2),lty=1)

#screen(5)
  x1 <- CWRES[CWRES[,"ID"]==ID,"PRED"]
  y1 <- CWRES[CWRES[,"ID"]==ID,"WRES"]
  ord1 <- order(x1)
  plot(x1[ord1],y1[ord1], type="b", cex=0.5, xlab="PRED", ylab="WRES")
  abline(h=0,lty=3)

#screen(8)
  FITi <- FITDATA3[floor(FITDATA3[,"ID"])==ID,]
  x4 <- FITi[,"TALD"]
  y4 <- FITi[,"WRES"]
      DxPlotPost(x4, y4, mat=FITi, xlbl="Time after Latest Dose", ylbl="WRES", smooth="T")

#screen(6)
  x2 <- CWRES[CWRES[,"ID"]==ID,"IPRE"]
  y2 <- CWRES[CWRES[,"ID"]==ID,"CWRE"]
  ord2 <- order(x2)
  plot(x2[ord2], y2[ord2], type="b", cex=0.5, xlab="IPRE", ylab="CWRES")
  abline(h=0,lty=3)

#screen(9)
  y5 <- FITi[,"CWRE"]
      DxPlotPost(x4, y5, mat=FITi, xlbl="Time after Latest Dose", ylbl="CWRES", smooth="T")

#screen(7)
  y3 <- CWRES[CWRES[,"ID"]==ID,"IWRE"]
  plot(x2[ord2],y3[ord2], type="b", cex=0.5, xlab="IPRE", ylab="IWRES")
  abline(h=0,lty=3)

#screen(10)
  y6 <- FITi[,"IWRE"]
      DxPlotPost(x4, y6, mat=FITi, xlbl="Time after Latest Dose", ylbl="IWRES", smooth="T")
      mtext(outer=F, side=1, line=4, cex=0.7, "*Fractional number means dosing occasion.")

  mtext(outer=T, side=3, paste("Individiual Residual Curve, ID =", ID2s[i]))

  resRun[i] <- run.test.nm(CWRES[CWRES[,"ID"]==ID,"CWRE"])
  Col <- 'black'
  if (resRun[i] < 0.05) Col <- 'red'

  mtext(outer=T, side=3, line=-1, col=Col, cex=0.8, paste("IOFV =", format(IDStat5[IDStat5[,"ID"]==ID,"iOFV"],digits=6),", OFV per DV =", format(IDStat5[IDStat5[,"ID"]==ID,"OFVpDV"],digits=5), ", Run Test(CWRES) p =", format(resRun[i],digits=3)))


}

AddPage(Cex=0.8)
resRun <- cbind(ID2s,resRun)
colnames(resRun) <- c("ID","p.value")
PrinTxt(1,1,"Subjects with Bad Run Test Result on CWRES", Cex=1)
nBadRun <- sum(resRun[,"p.value"] < 0.05)

if (nBadRun > 0) {
  BadRun <- resRun[resRun[,"p.value"] < 0.05,]
  sBadRun <- capture.output(BadRun)
  for (i in 1:length(sBadRun)) {
    PrinTxt(3+i,3, sBadRun[i], Cex=0.8)
  }
} else {
    PrinTxt(4,3, "None")
}

ClosePDF()

