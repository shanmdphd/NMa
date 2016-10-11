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
GRD = read.table(paste(CtlName,".grd",sep=""), skip=1, header=TRUE)
PHI = read.table(paste(CtlName,".phi",sep=""), skip=1, header=TRUE)

EXT = EXT[EXT[,"ITERATION"] >= 0,]
EXT = RmvFixed(EXT)

IDs = PHI[,"ID"]
nID = length(IDs)

ColNames = colnames(EXT)
nTheta = 0
nEta = 0
nEps = 0
for (i in 1:length(ColNames)) {
  if(substr(ColNames[i],1,5)=="THETA") nTheta = nTheta + 1
  if(substr(ColNames[i],1,5)=="SIGMA") nEps = nEps + 1
  if(substr(ColNames[i],1,5)=="OMEGA") nEta = nEta + 1
}

nEta = (sqrt(1 + 8*nEta) - 1)/2
nEps = (sqrt(1 + 8*nEps) - 1)/2

nPara = dim(GRD)[2] - 1
nOff = nPara - nTheta - nEta - nEps

IterCnt = max(EXT[,"ITERATION"])
OFV = EXT[EXT[,"ITERATION"]==IterCnt,"OBJ"]
Grad = GRD[GRD[,"ITERATION"]==IterCnt,]

TagList = c(
" NO. OF DATA RECS IN DATA SET:",
" NO. OF DATA ITEMS IN DATA SET:",
" TOT. NO. OF OBS RECS:",
" TOT. NO. OF INDIVIDUALS:",
"0LENGTH OF THETA:",
" NO. OF FUNCTION EVALUATIONS USED:",
" NO. OF SIG. DIGITS IN FINAL EST.:"
)

TagIndex = rep(NA, length(TagList))

for (i in 1:length(TagList)) {
  TagIndex[i] = ParseOut(TagList[i],XML)
}

nRec = as.integer(TagIndex[1])
nItem = as.integer(TagIndex[2])
nObs = as.integer(TagIndex[3])
nID = as.integer(TagIndex[4])
nTheta = as.integer(TagIndex[5])
nFuncEval = as.integer(TagIndex[6])
nSigDigit = as.double(TagIndex[7])
nDV = nObs


FDATA = readLines("FDATA")
nLine = length(FDATA)
Dupl = nLine/nRec

if (Dupl > 1 & length(FDATA) %% nRec==0) {
  FDATA2 = rep("", nRec)
  for (j in 1:nLine) {
    i = floor((j + 1) / Dupl)
    FDATA2[i] = paste(FDATA2[i], FDATA[j], sep="")
  }
  FDATA = FDATA2
}
write(FDATA, "FDATA2", append=FALSE)

RecLen = nchar(FDATA[1])

Widths = ParseItemWidth(XML)

FDATA = read.fwf("FDATA2", Widths, na.strings=" ")
colnames(FDATA) = ParseItemName(XML)
write.csv(FDATA, "FDATA.CSV", quote=FALSE, na="", row.names=FALSE)


################################################################################
# Summary Part 1 - Objective Function Value (OFV)
################################################################################
PrepPDF("S1-OFV.PDF")

scrnmat <- matrix(0, 5, 4)
scrnmat[1,] <- c(0, 1, 0, 1)
scrnmat[2,] <- c(0.55, 0.97, 0.6, 0.95)
scrnmat[3,] <- c(0.55, 0.97, 0.45, 0.8)
scrnmat[4,] <- c(0.55, 0.97, 0.1, 0.45)
split.screen(scrnmat)
options(width=90)

VarStat <- NMVarStat(FDATA)
IDStat <- NMIDStat(FDATA)
nDV = nObs

IDStat2 <- cbind(rownames(IDStat[[1]]),IDStat[[1]])
IDStat2Name <- names(IDStat2)
IDStat2Name[1] <- "ID"
names(IDStat2) <- IDStat2Name

tOFV = PHI[,c("ID","OBJ")]
colnames(tOFV) = c("ID","iOFV")

IDStat3 <- merge(tOFV,IDStat2)
IDStat4 <- IDStat3[,-10]
IDStat4$OFVpDV <- IDStat4[,"iOFV"] / IDStat4[,"nDV"]
IDStat5 <- IDStat4[order(IDStat4[,"OFVpDV"],decreasing=T),]
IDStat6 <- capture.output(IDStat5)

AICc <- OFV + 2 * nPara + 2*nPara*(nPara+1)/(nDV-nPara-1);
SBIC <- OFV + nPara * log(nDV)

sIOFVpDV <- summary(IDStat5[,"OFVpDV"])
sdIOFVpDV <- sd(IDStat5[,"OFVpDV"])

AddPage()

screen(1)
PrinTxt(1,1,"Summary 1 - Objective Function Values", Cex=1.2)

PrinTxt(3,1,paste("PROBLEM :", XML[grep(" PROBLEM NO.:",XML) + 1]), Cex=0.9)
PrinTxt(5,3,paste("Number of Total Records :", nRec))
PrinTxt(6,3,paste("Number of DV Records    :", nObs))
PrinTxt(7,3,paste("Number of Items(Columns):", nItem))
PrinTxt(8,3,paste("Number of Parameters    :", nPara))
PrinTxt(9,3,paste("Objective Function Value:", OFV))
PrinTxt(10,3,paste("OFV per DV              :", format(OFV/nDV, digist=5)))
PrinTxt(11,3,paste("Corrected AIC Value     :", format(AICc, digits=10)))
PrinTxt(12,3,paste("Schwartz Criterion(BIC) :", format(SBIC, digits=10)))
PrinTxt(13,3,paste("# of Gradients Over |1| :", sum(abs(Grad[-1])>1)))
PrinTxt(14,3,paste("Number of Sig Digits    :", nSigDigit))

PrinTxt(16,1,"Summary of Individual OFV per DV", Cex=0.9)
PrinTxt(18,3,paste("Minimum :", sIOFVpDV[1]))
PrinTxt(19,3,paste("1st Qu. :", sIOFVpDV[2]))
PrinTxt(20,3,paste("Median  :", sIOFVpDV[3]))
PrinTxt(21,3,paste("Mean    :", sIOFVpDV[4]))
PrinTxt(22,3,paste("3rd Qu. :", sIOFVpDV[5]))
PrinTxt(23,3,paste("Maximum :", sIOFVpDV[6]))
PrinTxt(24,3,paste("Std Dev :", format(sdIOFVpDV, digits=4)))
PrinTxt(25,3,paste("Coe Var :", format(sdIOFVpDV/sIOFVpDV[4], digits=4)))
PrinTxt(26,3,paste("S-W test:", format(shapiro.test(IDStat5[,"OFVpDV"])$p.value, digits=4)))

PrinTxt(28,1,"Header information for the next table", Cex=0.9)
PrinTxt(30,3,"ID     : Subject ID")
PrinTxt(31,3,"iOFV   : Individual Objective Function Value")
PrinTxt(32,3,"nRec   : Number of Records")
PrinTxt(33,3,"nDV    : Number of DV(Dependent Variable) Records")
PrinTxt(34,3,"nMDV   : Number of Missing DV Records")
PrinTxt(35,3,"nAMT   : Number of Dosing(AMT) Recods")
PrinTxt(36,3,"nEVID2 : Number of Records with EVID > 1")
PrinTxt(37,3,"FRec   : Row Number of First Record")
PrinTxt(38,3,"FDRec  : Row Number of First Dosing Record")
PrinTxt(39,3,"FDDT   : First Dosing Record Time")
PrinTxt(40,3,"OFVpDV : Objective Function Value per DV")
PrinTxt(42,3,"*Table is ordered by decreasing OFVpDV", Cex=0.7)

PrinTxt(44,1,"Abbreviations for Tables", Cex=0.9)
PrinTxt(46,3,"PRED   : Typical Prediction")
PrinTxt(47,3,"IPRE   : Individual Prediction")
PrinTxt(48,3,"WRES   : Weighted Residual")
PrinTxt(49,3,"CWRE   : Conditional Weighted Residual")
PrinTxt(50,3,"IWRE   : Individual Weighted Residual")
PrinTxt(51,3,"LL     : Lower Limit of Confidence Interval")
PrinTxt(52,3,"UL     : Upper Limit of Confidence Interval")
PrinTxt(53,3,"RSE    : Relative Standard Error (SE / Point Estimate)")
PrinTxt(54,3,"SHR    : Shrinkage (Observed SD / Estimated SD)")
PrinTxt(55,3,"ZERO   : Dose confidence interval include zero?")


par(defpar)

screen(2)
  x <- IDStat5[,"OFVpDV"]
  y <- jitter(rep(0, length(x)))
  plot(x,y,ylim=c(-0.2,0.2), xaxt = "n", yaxt = "n", xlab="",ylab="",bty="n",cex=0.4)
  lines(c(sIOFVpDV[2],sIOFVpDV[2]),c(-0.05,+0.05))
  lines(c(sIOFVpDV[3],sIOFVpDV[3]),c(-0.05,+0.05))
  lines(c(sIOFVpDV[4],sIOFVpDV[4]),c(-0.07,+0.07))
  lines(c(sIOFVpDV[5],sIOFVpDV[5]),c(-0.05,+0.05))
  lines(c(sIOFVpDV[2],sIOFVpDV[5]),c(-0.05,-0.05))
  lines(c(sIOFVpDV[2],sIOFVpDV[5]),c(+0.05,+0.05))
  lines(c(sIOFVpDV[4]-2*sdIOFVpDV,sIOFVpDV[4]-2*sdIOFVpDV),c(-0.03,+0.03))
  lines(c(sIOFVpDV[4]+2*sdIOFVpDV,sIOFVpDV[4]+2*sdIOFVpDV),c(-0.03,+0.03))


par(defpar)

screen(3)
  var.data <- IDStat5[,"OFVpDV"]
  h.res <- hist(var.data, plot=F)
  d.res <- density(var.data, na.rm=T)
  h.rat <- max(h.res$counts)/max(h.res$density)
  xrange <- range(d.res$x)
  yrange <- c(0, max(h.res$counts, max(d.res$y)*h.rat))
  plot(h.res, xlim=xrange, ylim=yrange, xlab="IOFV per DV", main="")
  lines(d.res$x, h.rat*d.res$y)

screen(4)
  qqnorm(IDStat5[,"OFVpDV"],datax=T, main="",ylab="IOFV per DV",cex=0.4)


nRow <- 55
AddPage(Cex=0.8)
PrinTxt(1,1, "Table of Individual Objective Function Values and Records Summary", Cex=1)
PrinTxt(2,1, IDStat6[1])

for (i in 2:length(IDStat6)) {
  if (i%%(nRow-1)==1) {
    AddPage(Cex=0.8)
    PrinTxt(1,1, IDStat6[1])
  }
  PrinTxt((i-1)%%(nRow-1)+2, 1, IDStat6[i])
}

ClosePDF()



