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

FDATA <- read.csv("FDATA.CSV")
VarStat <- NMVarStat(FDATA)

XML = readLines(paste(RunNumber,".xml",sep=""))
PHI = read.table(paste(RunNumber,".phi",sep=""), skip=1, header=TRUE)
EXT = read.table(paste(RunNumber,".ext",sep=""), skip=1, header=TRUE)
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

OMEGA = BtwTagMat("omega", XML, nEtaAll)
OMEGAse = BtwTagMat("omegase", XML, nEtaAll)
SIGMA = BtwTagMat("sigma", XML, nEpsAll)
SIGMAse = BtwTagMat("sigmase", XML, nEpsAll)

OMa = rbind(OMEGA, OMEGAse)
SGa = rbind(SIGMA, SIGMAse)

nEta <- length(OMa[1,])                        
OM <- OMa[1:nEta,]                        
SeOM <- OMa[(nEta+1):(2*nEta),]          

EtaNames <- character()
EtaNamesRaw = character()
EtaErrorRaw = character()

for (i in 1:nEta) {
  EtaNames <- c(EtaNames, paste("Eta",i))
  EtaNamesRaw = c(EtaNamesRaw, paste("ETA.",i,".",sep=""))
  EtaErrorRaw = c(EtaErrorRaw, paste("ETC.",i,".",i,".",sep=""))
}
rownames(OM) <- EtaNames
colnames(OM) <- EtaNames
rownames(SeOM) <- EtaNames
colnames(SeOM) <- EtaNames

sdtab =  read.table("sdtab", header=TRUE, skip=1, na.strings="***********")
IDs <- unique(sdtab[,"ID"])
nID <- length(IDs)

PrepPDF("S5-EBE.PDF")

EtaNames2 <- "ID"
seEtaNames <- character()
for (i in 1:nEta) {
  EtaNames2 <- c(EtaNames2, paste("ETA",i,sep=""))
  seEtaNames <- c(seEtaNames, paste("seETA",i,sep=""))
}

tabEta = PHI[,c("ID",EtaNamesRaw)]
colnames(tabEta) = EtaNames2
seEtaRaw = PHI[,c("ID",EtaErrorRaw)]  #######
colnames(seEtaRaw) <- c("ID", seEtaNames)

seEta <- PHI[,EtaErrorRaw]
colnames(seEta) = seEtaNames

EtaRep <- PHI[,c("ID",EtaNamesRaw,EtaErrorRaw)]
colnames(EtaRep) = c(EtaNames2, seEtaNames)

RowPerPage <- 4
for (i in 1:nEta) {
  if (i%%RowPerPage == 1) {
    par(defpar)
    par(oma=c(1,1,3,1), mfrow=c(RowPerPage,RowPerPage-1), lty=1)
  }
  var.data <- EtaRep[,paste("ETA",i,sep="")]
  sdvar <- sd(var.data)

  plot(0, 0, type = "n", ylim = c(1, 8), xlim = c(0,10), xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n")
  sEta <- summary(var.data)
  text(0,8,paste("Minimum :", sEta[1]), adj=0)
  text(0,7,paste("1st Qu. :", sEta[2]), adj=0)
  text(0,6,paste("Median  :", sEta[3]), adj=0)
  text(0,5,paste("Mean    :", sEta[4]), adj=0)
  text(0,4,paste("3rd Qu. :", sEta[5]), adj=0)
  text(0,3,paste("Maximum :", sEta[6]), adj=0)
  text(0,2,paste("Std Dev :", format(sdvar, digits=4)), adj=0)
  ttest.pvalue <- format(t.test(var.data)$p.value, digits=4)
  text(0,1,paste("t-test p=", ttest.pvalue), adj=0)
  mtext(side=3,line=1,paste("Eta",i), adj=0)


  h.res <- hist(var.data, plot=F)
  d.res <- density(var.data, na.rm=T)
  h.rat <- max(h.res$counts)/max(h.res$density)
  xrange <- range(d.res$x)
  yrange <- c(0, max(h.res$counts, max(d.res$y)*h.rat))
  plot(h.res, xlim=xrange, ylim=yrange, xlab=paste("ETA",i,sep=""), main="")
  lines(d.res$x, h.rat*d.res$y)
  p.value <- format(shapiro.test(var.data)$p.value, digits=4)
  mtext(paste("Shapiro-Wilk test p-value =", p.value), cex=0.7, line=0, adj=0)
  shrink <- sdvar / sqrt(OM[i,i])
  shrinkage <- format(shrink, digits=4)
  mtext(paste("Shrinkage =", shrinkage), cex=0.7, line=1, adj=0)

  qqnorm(var.data, main="",ylab=paste("ETA",i,sep=""),cex=0.4)

  if (i%%RowPerPage==0 | i==nEta)  mtext(outer=T, side=3, "Normality and Population Shrinkage of Etas")

}



ResWords <- c("ID","DV","MDV","EVID","AMT","RATE","CMT","II","SS","TIME","DATE","LNDV")
RmvList <- union(ResWords, rownames(VarStat[VarStat[,"nUniq"]==1,]))
CatList <- setdiff(rownames(VarStat[VarStat[,"ALLInt"]==1 & VarStat[,"nUniq"]<8 & VarStat[,"nUniq"]>1,]),RmvList)
ContList <- setdiff(setdiff(rownames(VarStat),RmvList),c(CatList,"SITE","CENT"))

CovariateList = union(CatList,ContList)
tabCovariate = matrix(nrow=nID, ncol=length(CovariateList))
colnames(tabCovariate) = CovariateList
nRec = dim(FDATA)[1]
PrevID = 0
j = 1
for (i in 1:nRec) {
  if (PrevID != FDATA[i,"ID"]) {
    for (k in 1:length(CovariateList)) {
      tabCovariate[j,CovariateList[k]] = FDATA[i,CovariateList[k]]
    }
    j = j + 1
  }
  PrevID = FDATA[i,"ID"]
}
tabEta = cbind(tabEta, tabCovariate)

VarEta <- names(tabEta)
Cat1 <- intersect(VarEta, CatList)
Cont1  <- intersect(VarEta, ContList)

nCat <- length(Cat1)
nCont <- length(Cont1)


#########
if (nCat > 0) {
  par(defpar)
  par(oma=c(1,1,3,1),mar=c(4,4,2,2), mfrow=c(nEta,nCat),lty=1)

  for (i in 1:nEta) {
    for (j in 1:nCat) {
      form <- paste("ETA",i,"~",Cat1[j],sep="")
      boxplot(formula=formula(form),data=tabEta, xlab=Cat1[j], ylab=paste("ETA",i))
      Anova.pvalue <- anova(lm(formula(form),data=tabEta))[[5]][1]
      if (Anova.pvalue < 0.05) { txtcol <- "red" }
      else {txtcol <- "black"}
      mtext(side=3,cex=0.6,col=txtcol, paste("ANOVA p =", format(Anova.pvalue,digits=4)))
    }
  }
  mtext(outer=T, side=3, "ETA vs Categorical Variables")
}
############

EBEpair(tabEta[,c("ID",Cont1,EtaNames2[-1])], RunNumber)


#for (i in 1:nEta) {
#  EBEpair(tabEta[,c("ID",Cont1,paste("ETA",i,sep=""))], RunNumber)
#}

IDdep <- rownames(VarStat[VarStat[,"DepID"]==1,])
IDTimeMDVdep <- rownames(VarStat[VarStat[,"DepIDTimeMDV"]==1,])
IDTimeEVIDdep <- rownames(VarStat[VarStat[,"DepIDTimeEVID"]==1,])

OnlyIDTime0 <- union(IDTimeMDVdep, IDTimeEVIDdep)
OnlyIDTime1 <- setdiff(OnlyIDTime0, IDdep)
OnlyIDTime2 <- setdiff(OnlyIDTime1, ResWords)


par(oma=c(1,1,3,1),mar=c(0,0,0,0), mfrow=c(nEta+1,nCont+1),lty=1)
for (i in 1:nEta) {
  for (j in 0:nCont) {
    if (j==0) {
      plot(0, 0, type = "n", ylim = c(0, 1), xlim = c(0,1), xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n")
      text(0, 0.5, paste("ETA",i,sep=""), adj=0)
    } else {

      if (i < nEta) {
        Xaxt <- "n"
        Xlab <- ""
      } else {
        Xaxt <- "s"
        Xlab=Cont1[j]
      }
      if (j == 1) {
        Yaxt <- "s"
        Ylab <- paste("ETA",i)
      } else {
        Yaxt <- "n"
        Ylab <- ""
      }

      x <- tabEta[,Cont1[j]]
#      minx <- min(FDATA[,Cont1[j]],na.rm=T)
#      maxx <- max(FDATA[,Cont1[j]],na.rm=T)
      minx <- min(x,na.rm=T)
      maxx <- max(x,na.rm=T)
      y <- tabEta[,paste("ETA",i,sep="")]
#      plot(0,0,type="n", xaxt=Xaxt, xlab=Xlab, yaxt=Yaxt, ylab=Ylab)
#      plot(x,y,type="p",xlim=c(minx,maxx),cex=0.7, xaxt=Xaxt, xlab=Xlab, yaxt=Yaxt, ylab=Ylab)

      IDList <- tabEta[,"ID"]
      n <- length(x)
      ordx <- order(x)
      ordy <- order(y)

      OutSide <- union(union(ordx[1:max(1,round(0.025*n))], ordx[round(0.975*n):n]), union(ordy[1:max(1,round(0.025*n))], ordy[round(0.975*n):n]))
      InSide <- intersect(ordx[max(2,round(0.025*n)):min(round(0.975*n),n-1)], ordy[max(2,round(0.025*n)):min(round(0.975*n),n-1)])
      plot(x,y,type="n", xlim=c(minx-0.1*(maxx-minx),maxx+0.1*(maxx-minx)),xaxt=Xaxt, xlab=Xlab, yaxt=Yaxt, ylab=Ylab)
      text(x[OutSide],y[OutSide], IDList[OutSide], cex=0.8)
      points(x[InSide],y[InSide],cex=0.5)

#      plot(x,y,type="p",cex=0.7, xaxt=Xaxt, xlab=Xlab, yaxt=Yaxt, ylab=Ylab)

      if (length(intersect(Cont1[j], OnlyIDTime2)) == 1) {
        for (k in 1:nID) {
          cID <- IDs[k]
          x2 <- unique(FDATA[FDATA[,"ID"]==cID,Cont1[j]])
          nx2 <- length(x2)
          y2 <- rep(tabEta[tabEta[,"ID"]==cID,paste("ETA",i,sep="")],nx2)
          lines(x2,y2,lty=1)

#          n2 <- length(x2)
          points(x2[-1],y2[-1], cex=0.5, pch=2)
#          text(x2[n],y2[n],cID,cex=0.5)

        }
#      } else {
#        text(x, y, tabEta[,"ID"], cex=0.7)
      }
      abline(h=0, lty=2)
      lines(lowess(x, y), col="red", lty=1)
    }
  }
}

plot(0, 0, type = "n", ylim = c(0, 1), xlim = c(0,1), xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n")

if (nCont > 0) {
  for (j in 1:nCont) {
    plot(0, 0, type = "n", ylim = c(0, 1), xlim = c(0,1), xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n")
    text(0.5,0.8,Cont1[j])
  }
}

options(width=95)
corEtaA <- cor(tabEta[,c(Cont1,EtaNames2[-1])])

covEta2 <- cov(tabEta[,EtaNames2[-1]])
corEta2 <- cov2cor(covEta2)
corOM <- cov2cor(OM)

sOut0t <- "Correlation of Covariates and EBE"
sOut1t <- "Covariance of EBE"
sOut2t <- "Omega Matrix"
sOut3t <- "Ratios of Cov(EBE)/OM"
sOut4t <- "Correlation of EBE"
sOut5t <- "Correlation from Omega Matrix"
sOut6t <- "Ratios of Cor(EBE)/(Cor from OM)"

oAll <- list(corEtaA, covEta2, OM, covEta2/OM, corEta2, corOM, corEta2/corOM)
names(oAll) <- c(sOut0t, sOut1t, sOut2t, sOut3t, sOut4t, sOut5t, sOut6t) 

PrinMTxt(capture.output(oAll), Header1="Estimation vs EBE")


#ClosePDF()

library(MASS)

tabEta2 = RemoveNA(tabEta)
result <- list()
result2 <- list()

CovarList <- c(Cat1, Cont1)

if (length(CovarList) > 0) {

  modelstr <- character()
  for (i in 1:length(CovarList)) {
    modelstr <- paste(modelstr, CovarList[i])
    if (i < length(CovarList)) modelstr <- paste(modelstr, "+")
  }

  for (j in 1:nEta) {
    formstr <- paste("ETA",j,"~", modelstr, sep="");
    result[j] <- list(summary(lm(formula(formstr), data=tabEta)))
    result2[j] <- list(mlr2(tabEta2[,paste("ETA",j,sep="")], tabEta2[CovarList]))
  }

  names(result) <- EtaNames
  names(result2) <- EtaNames

  for (i in 1:nEta) {
    sOut <- capture.output(result[[i]])
    PrinMTxt(sOut[4:length(sOut)], Header1=paste("Multiple Linear Regression : ETA",i))
    sOut2 <- capture.output(result2[[i]])
    PrinMTxt(sOut2, Header1=paste("Multiple Linear Regression - Influence : ETA",i))

    D     <- result2[[i]][[2]][,"Cook's D"]
    y.hat <- result2[[i]][[2]][,"Yhat"]
    sdr   <- result2[[i]][[2]][,"R-Student"]
    h     <- result2[[i]][[2]][,"hat"]
    e     <- result2[[i]][[2]][,"Residual"]
    SSE   <- result2[[i]][[6]]
    COVRATIO <- result2[[i]][[2]][,"COV-Ratio"]
    DFFITS   <- result2[[i]][[2]][,"DFFITS"]
    n  <-  result2[[i]][[3]]
    p  <-  result2[[i]][[4]]

    par(defpar)
    par(mfrow=c(2,2), oma=c(1,1,3,1))
    plot(D, type="n", xlab="Index", ylab="Cook's Distance")
    for(j in 1:n) {
      if(D[j] == max(D)) text(j, D[j], j)
      else points(j, D[j])
    }

    plot(y.hat, sdr, type="n", xlab="Predicted Value", ylab="Studentized deleted residuals")
    for(j in 1:n) {
      if(abs(sdr[j]) > 2) text(y.hat[j], sdr[j], j)
      else points(y.hat[j], sdr[j])
    }

    plot(h, e^2/SSE, type="n", xlab="hat", ylab="e^2/SSE")
    for(j in 1:n) {
      if(e[j]^2/SSE > 0.15) text(h[j], e[j]^2/SSE, j)
      else points(h[j], e[j]^2/SSE)
    }

    if (is.finite(COVRATIO)) {
      plot(COVRATIO, type="n", DFFITS, xlab="COVRATIO", ylab="DFFITS")
      for(j in 1:n) {
        if(abs(DFFITS[j]) > 1 | abs(COVRATIO[j]-1) > 3*p/n) text(COVRATIO[j], DFFITS[j], j)
        else points(COVRATIO[j], DFFITS[j])
      }
    }

    mtext(paste("Influence Diagnostics on Eta", i), outer=TRUE, side=3)
  }

}
############



LL   <- matrix(nrow=nID,ncol=nEta)
LLnames <- character()
for (i in 1:nEta) LLnames <- c(LLnames, paste("LL",i,sep=""))
colnames(LL) <- LLnames

for (i in 1:nEta) {
  LL[,i] <- EtaRep[,i+1] - 2 * EtaRep[,nEta+i+1]
}

UL   <- matrix(nrow=nID,ncol=nEta)
ULnames <- character()
for (i in 1:nEta) ULnames <- c(ULnames, paste("UL",i,sep=""))
colnames(UL) <- ULnames

for (i in 1:nEta) {
  UL[,i] <- EtaRep[,i+1] + 2 * EtaRep[,nEta+i+1]
}

ZERO <- matrix(nrow=nID,ncol=nEta)
ZEROnames <- character()
for (i in 1:nEta) ZEROnames <- c(ZEROnames, paste("ZERO",i,sep=""))
colnames(ZERO) <- ZEROnames

for (i in 1:nEta) {
  ZERO[,i] <- (LL[,i] * UL[,i] < 0)
}

RSE   <- matrix(nrow=nID,ncol=nEta)
RSEnames <- character()
for (i in 1:nEta) RSEnames <- c(RSEnames, paste("RSE",i,sep=""))
colnames(RSE) <- RSEnames

for (i in 1:nEta) {
  RSE[,i] <- EtaRep[,nEta+i+1] / abs(EtaRep[,i+1])
}

SHR  <- matrix(nrow=nID,ncol=nEta)
SHRnames <- character()
for (i in 1:nEta) SHRnames <- c(SHRnames, paste("SHR",i,sep=""))
colnames(SHR) <- SHRnames

for (i in 1:nEta) {
  SHR[,i] <- EtaRep[,nEta+i+1] / sqrt(OM[i,i])
}

write.csv(cbind(EtaRep,LL,UL,ZERO,RSE,SHR), file="EBEALL.CSV",row.names=F)

for (i in 1:nEta) {
  tabHeader <- c("ID",paste("ETA",i,sep=""),paste("seETA",i,sep=""),paste("LL",i,sep=""),paste("UL",i,sep=""),paste("ZERO",i,sep=""),paste("RSE",i,sep=""),paste("SHR",i,sep=""))
  tabEta <-cbind(EtaRep[,c(1,i+1,i+nEta+1)],LL[,i],UL[,i],ZERO[,i],RSE[,i],SHR[,i])
  colnames(tabEta) <- tabHeader

  PrinMTxt(capture.output(tabEta), Header1=paste("ETA",i))
}



ClosePDF()





