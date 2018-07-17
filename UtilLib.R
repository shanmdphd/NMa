# Library Functions
TrimLeading  = function(x) sub("^\\s+", "", x)
TrimTrailing = function(x) sub("\\s+$", "", x)
Trim         = function(x) gsub("^\\s+|\\s+$", "", x)

# Add Dose Number(DoNo), Time after Latest Dose(TaLD), and TIme of Latest Dose(ToLD) columns to NONMEM Dataset
AddDoNoTaLD = function(NMData, ID="ID", TIME="TIME", AMT="AMT", II="II", ADDL="ADDL", MDV="MDV")
{
  nRec = length(NMData[,1])
  DoNo = vector(length=nRec)  #DoOc: Dosing Occasion Number
  ToLD = vector(length=nRec)  #ToLD: Time of Lastest Dosing Start
  TaLD = vector(length=nRec)  #TaLD: Time after Latest Dosing Start

  LastID = -1
  for (i in 1:nRec) {
    CurID = NMData[i,ID]
    if (LastID != CurID) {
      pToLD = 0
      cDoNo = 0
    }

    if (length(intersect(names(NMData),ADDL))==1) {
      DosingHist = NMData[NMData[,ID]==CurID & NMData[,AMT] > 0 & NMData[,MDV]==1, c(TIME,AMT,II,ADDL)]
      nDoseRec = length(DosingHist[,AMT])
      for (j in 1:nDoseRec) {
        cADDL = DosingHist[j,ADDL]
        if (cADDL > 0) {
          cAMT = DosingHist[j,AMT]
          cTIME = DosingHist[j,TIME]
          cII = DosingHist[j,II]
          for (k in 1:cADDL) DosingHist = rbind(DosingHist, c(cTIME + k*cII, cAMT, NA, NA))
        }
      }
      DosingHist2 = DosingHist[order(DosingHist[,TIME]),c(TIME,AMT)]
    }

    if (NMData[i,AMT] > 0 & NMData[i,MDV] == 1) {
      cToLD = NMData[i,TIME]
      cDoNo = cDoNo + 1
    } else {
      if (length(intersect(names(NMData),ADDL))==1) {
        cTime = NMData[i,TIME]
        cToLD = max(DosingHist2[DosingHist2[,TIME] < cTime,TIME])
        cDoNo = length(DosingHist2[DosingHist2[,TIME] < cTime,TIME])
      } else {
        cToLD = pToLD
      }
    }
    DoNo[i] = cDoNo
    ToLD[i] = cToLD
    LastID = CurID
    pToLD = cToLD
  }

  TaLD = NMData[,TIME] - ToLD
  return(cbind(NMData,DoNo,ToLD,TaLD))
}


# Checking LOG DV model

LogDV <- function(CtlFileName)
{
  ModFile <- toupper(readLines(CtlFileName))

  iPRED <- grep("$PRED", ModFile, fixed=TRUE)
  iERROR <- grep("$ERROR", ModFile, fixed=TRUE)
  iLOGF <- grep("LOG(F)", ModFile, fixed=TRUE)

  if (length(iPRED)==1) {
    if (iLOGF > iPRED) return(TRUE)
  }

  if (length(iERROR)==1) {
    if (iLOGF > iERROR) return(TRUE)
  }
  return(FALSE)
}



LogDV2 <- function()
{
  ModFile <- readLines("FSUBS")

  iPRED <- grep("SUBROUTINE PRED (", ModFile, fixed=TRUE)
  iERROR <- grep("SUBROUTINE ERROR (", ModFile, fixed=TRUE)
  iLOGF <- grep("LOG(F)", ModFile, fixed=TRUE)

  if (length(iLOGF)==1) {
    if (length(iPRED)==1) {
      if (iLOGF > iPRED) return(TRUE)
    }

    if (length(iERROR)==1) {
      if (iLOGF > iERROR) return(TRUE)
    }
  }

  return(FALSE)
}


# For Pair plot
PanelHist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

PanelCor <- function(x, y, digits=2, prefix="", cex.cor)
{
# put correlations with size proportional to the correlations.
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = min(1,(cex * abs(r)+0.5)))
}

PanelChar <- function(x, y)
{
# External: ParDat[,"ID"]
  IDs <- ParDat[,"ID"]
  lines(lowess(x, y), col="red")

#  n <- length(x)
#  ordx <- order(x)
#  ordy <- order(y)
#  OutSide <- union(union(ordx[1:max(1,round(0.025*n))], ordx[round(0.975*n):n]), union(ordy[1:max(1,round(0.025*n))], ordy[round(0.975*n):n]))
#  InSide <- intersect(ordx[max(2,round(0.025*n)):min(round(0.975*n),n-1)], ordy[max(2,round(0.025*n)):min(round(0.975*n),n-1)])
#  text(x[OutSide],y[OutSide], IDs[OutSide], cex=0.6)
#  points(x[InSide],y[InSide],cex=0.5)

  points(x,y, cex=0.5)
}

EBEpair <- function(tabEta, RunNumber, each=FALSE)
{
# Requires PanelHist(), PanelCor(), PanelChar()
  ParDat <<- tabEta
  NameList <- names(ParDat)
  nVar <- length(NameList)
  nEta <- sum(substr(names(ParDat),1,3)=="ETA")
  EtaNames <- vector()
  for(i in 1:nEta) EtaNames <- append(EtaNames, paste("ETA",i,sep=""))

  # Graph

  if (each==FALSE) {
    pairs(ParDat[,-1], main=paste("Covariate vs ETA of", RunNumber), gap=0,
        lower.panel=PanelChar,
        diag.panel=PanelHist,
        upper.panel=PanelCor)
  } else {
    for(i in 1:nVar) {
      if (substr(NameList[i],1,3) != "ETA" & NameList[i] != "ID") {
        pairs(ParDat[,c(NameList[i],EtaNames)], main=paste(NameList[i],"vs ETAs of Model",RunNumber), gap=0,
            lower.panel=PanelChar,
            diag.panel=PanelHist,
            upper.panel=PanelCor)
      }
    }
  }
#  dev.off(dev.cur())
}

###############################################################
AllNA <- function(Column)
{
  return(all(is.na(Column)))
}

#############################################
AllSame <- function(Column)
{
  return(length(unique(Column))==1)
}

#############################################
FuncDep <- function(NMTable, DetNameList, DepNameList)
{
  LenDetName <- length(DetNameList)
  LenDepName <- length(DepNameList)

  UniqDet <- as.data.frame(unique(NMTable[,DetNameList]))
  UniqTot <- as.data.frame(unique(NMTable[,c(DetNameList,DepNameList)]))
  LenUniqDet <- length(UniqDet[,1])
  LenUniqTot <- length(UniqTot[,1])

  if (LenUniqDet != LenUniqTot) return(FALSE)

# identical function does work here!
  if (sum(UniqDet == UniqTot[,1:LenDetName], na.rm=T) == LenDetName*LenUniqDet) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


##############################################
NMVarStat <- function(NMTable)
{
  VarNames <- names(NMTable)
  StatNames <- c("nNA", "nZero", "nPos", "nUniq", "Min", "Max", "ALLNA", "ALLSame", "ALLInt", "ALLReal", "DepID", "DepIDTimeMDV", "DepIDTimeEVID")
  nVar <- length(VarNames)
  nStat <- length(StatNames)

  if (length(intersect("sDT",VarNames))==1) {
    TLabel <- "sDT"
  } else {
    TLabel <- "TIME"
  }

  VarStat <- matrix(nrow=nVar, ncol=nStat, dimnames=list(VarNames, StatNames))

  for (i in 1:nVar) {
    TotData <- NMTable[,paste(VarNames[i])]
    UniqData <- unique(TotData)
    nUniq <- length(UniqData)

    VarStat[i,"nNA"] <- sum(is.na(TotData))
    VarStat[i,"nZero"] <- sum(TotData==0, na.rm=T)
    VarStat[i,"nPos"] <- sum(TotData > 0, na.rm=T)
    VarStat[i,"nUniq"] <- nUniq
    VarStat[i,"Min"] <- min(UniqData, na.rm=T)
    VarStat[i,"Max"] <- max(UniqData, na.rm=T)
    VarStat[i,"ALLNA"] <- AllNA(UniqData)
    VarStat[i,"ALLSame"] <- AllSame(UniqData)
    VarStat[i,"ALLInt"] <- is.integer(UniqData)
    VarStat[i,"ALLReal"] <- is.double(UniqData)
    VarStat[i,"DepID"] <- FuncDep(NMTable, "ID", paste(VarNames[i]))
    if (length(intersect("MDV",VarNames))==1) {
      VarStat[i,"DepIDTimeMDV"] <- FuncDep(NMTable, c("ID",TLabel,"MDV"), paste(VarNames[i]))
    }
    if (length(intersect("EVID",VarNames))==1) {
      VarStat[i,"DepIDTimeEVID"] <- FuncDep(NMTable, c("ID",TLabel,"EVID"), paste(VarNames[i]))
    }
  }

  return(VarStat)
}

###############################################
IntStat <- function(NMTable, VarStat)
{
  VarNames <- rownames(VarStat[VarStat[,"ALLInt"]==T,])
  nVar <- length(VarNames)
  for (i in 1:nVar) {
    print(VarNames[i])
    print(sort(unique(NMTable[,paste(VarNames[i])])))
  }
}

####################################################
NMIDStat <- function(NMTable)
{
  sortedID <- TRUE
  sortedDT <- TRUE

  VarNames <- names(NMTable)
  if (length(intersect("sDT",VarNames))==1) {
    TLabel <- "sDT"
  } else {
    TLabel <- "TIME"
  }

  IDList <- unique(NMTable[,"ID"])
  StatNames <- c("nRec", "nDV", "nMDV", "nAMT", "nEVID1", "nEVID2", "nEVID3", "nEVID4", "FRec", "FDRec", "FRDT", "FDDT")
  nID <- length(IDList)
  nStat <- length(StatNames)

  IDStat <- as.data.frame(matrix(data=0, nrow=nID, ncol=nStat, dimnames=list(IDList, StatNames)))

  nTotRec <- length(NMTable[,1])
  pID <- ""
  pDT <- ""

  for (i in 1:nTotRec) {
    cID <- NMTable[i,"ID"]
    cDT <- NMTable[i,TLabel]

    IDStat[paste(cID),"nRec"] <- IDStat[paste(cID),"nRec"] + 1
    if (length(intersect("MDV",VarNames))==1) {
      if (NMTable[i,"MDV"] == 0) IDStat[paste(cID),"nDV"] <- IDStat[paste(cID),"nDV"] + 1
      if (NMTable[i,"MDV"] == 1) {
        IDStat[paste(cID),"nMDV"] <- IDStat[paste(cID),"nMDV"] + 1
        if (length(intersect("EVID",VarNames))==1) {
          if (NMTable[i,"EVID"] == 1) {
            IDStat[paste(cID),"nEVID1"] <- IDStat[paste(cID),"nEVID1"] + 1
            if (NMTable[i,"AMT"] > 0) IDStat[paste(cID),"nAMT"] <- IDStat[paste(cID),"nAMT"] + 1
          }
          if (NMTable[i,"EVID"] == 2) {
            IDStat[paste(cID),"nEVID2"] <- IDStat[paste(cID),"nEVID2"] + 1
          }
          if (NMTable[i,"EVID"] == 3) {
            IDStat[paste(cID),"nEVID3"] <- IDStat[paste(cID),"nEVID3"] + 1
          }
          if (NMTable[i,"EVID"] == 4) {
            IDStat[paste(cID),"nEVID4"] <- IDStat[paste(cID),"nEVID4"] + 1
            if (NMTable[i,"AMT"] > 0) IDStat[paste(cID),"nAMT"] <- IDStat[paste(cID),"nAMT"] + 1
          }
        } else {
          if (NMTable[i,"AMT"] > 0) IDStat[paste(cID),"nAMT"] <- IDStat[paste(cID),"nAMT"] + 1
        }
      }
    } else {
      IDStat[paste(cID),"nDV"] <- IDStat[paste(cID),"nDV"] + 1
    }

    if (IDStat[paste(cID),"nRec"] == 1) {
      IDStat[paste(cID),"FRec"] <- i
      IDStat[paste(cID),"FRDT"] <- cDT
    }
    if (length(intersect("AMT",VarNames))==1) {
      if (IDStat[paste(cID),"FDRec"] == 0 & NMTable[i,"AMT"] > 0) {
        IDStat[paste(cID),"FDRec"] <- i
        IDStat[paste(cID),"FDDT"] <- cDT
      }
    }

    if (pID > cID) sortedID <- FALSE
    if (pID == cID & pDT > cDT) sortedDT <- FALSE

    pID <- cID
    pDT <- cDT
  }
  Result <- list(IDStat,sortedID,sortedDT)
  names(Result) <- c("Individuals","ID Sorted","Time Sorted")
  return(Result)
}


PrepPDF <- function(FileName, Paper="letter", FontFamily="Courier")
{
  pdf(FileName, paper=Paper, width=8.5, height=11, family=FontFamily, title="Report of NONMEM 7.3 single-run")
}

PrinTxt <- function(Row, Col, Text, Cex=0.8)
{
  text(Col-1, 2*Row-1, Text, cex=Cex, offset=0)
}

AddPage <- function(Cex=0.8, Header1="", Header2="", Header3="", Footer1="", Footer2="", Footer3="", PrintRowNum=FALSE, StartRowNum=1)
{
  if (Cex == 0.8) {
    nRow=55
    nCol=90
    options(width=100)
  }
  if (Cex == 0.6) {
    nRow=80
    nCol=128
    options(width=150)
  }

  par(oma=c(0,0,0,0), mfrow=c(1,1), mar=c(0, 0, 0, 0), adj=0, cex=Cex)
  plot(0, 0, type = "n", ylim = c(2*nRow-1, 0), xlim = c(0,nCol-1), xaxt = "n", yaxt = "n", ylab = "", xlab = "", bty = "n")
  if (PrintRowNum==TRUE) for(j in 1:nRow) text(-2-1+3-floor(log10(j+StartRowNum-1)), 2*j-1, paste(j+StartRowNum-1,":",sep=""), offset=0)
  text(0,-3, Header1, pos=4)
  text(nCol/2,-3, Header2, pos=3)
  text(nCol,-3, Header3, pos=2)

  text(0,2*nRow+1,Footer1, pos=4)
  text(nCol/2,2*nRow+1, Footer2, pos=1)
  text(nCol,2*nRow+1,Footer3, pos=2)
}

ClosePDF <- function()
{
  dev.off()
}


PrinMTxt <- function(MTxt, Cex=0.8,  Header1="", Header2="", Header3="", Footer2="", Footer1="", Footer3="", PrintRowNum=FALSE)
{
  if (Cex == 0.8) {
    nRow <- 55
  }
  if (Cex == 0.6) {
    nRow <- 80
  }
  for (i in 1:length(MTxt)) {
  if (i%%nRow==1) AddPage(Cex=Cex, Header1=Header1, Header2=Header2, Header3=Header3, Footer2=Footer2, Footer1=Footer1, Footer3=Footer3, PrintRowNum=PrintRowNum)
  PrinTxt((i-1)%%nRow+1, 1, MTxt[i])
  }
}

DxPlotPost <- function (x, y, mat, xlbl, ylbl, smooth, xlm="", ylm="", Log="")
{
  if (Log=="y") {
    x <- x[y>0]
    y <- y[y>0]
  }

  if (ylbl=="DV" & (xlbl=="PRED" | xlbl=="IPRE")) {
    xlm <- ylm <- c(min(x,y), max(x,y))
  } else {
    if (xlm=="" ) {
      xlm <- c(min(x, na.rm=T), max(x, na.rm=T))
    }
    if (ylm=="") {
      ylm <- c(min(y, na.rm=T), max(y, na.rm=T))
    }
  }

  plot(0.001, 0.001, type="n", bty="o", xlim=xlm, ylim=ylm, xlab=xlbl, ylab=ylbl, log=Log)
  if (smooth == "T") {
    lines(lowess(x, y), lty=2)
    abline(h=0, lty=3)
  } else if (xlbl=="PRED" | xlbl=="IPRE") {
    abline(0, 1, lty=3)
  }
  SUBID <- unique(mat$ID)
  for (i in SUBID) {
    SelID <- mat$ID == i
    x1 <- x[SelID]
    y1 <- y[SelID]
    xord <- sort.list(x1)
    x1 <- x1[xord]
    y1 <- y1[xord]
    lines(x1, y1)
    for (j in 1:length(x1)) text(x1[j], y1[j], i, cex=0.75)
  }
}

run.p <- function(m, n, r)
{
# INPUT
# m : count of fewer species (minimum value = 0)
# n : count of more frequent species (minimum value = 1)
# r : count of run (minimum value = 1)
# RETURNS probability of run count to be less than or equal to r with m and n
#         P(Run count <= r | m, n)

  if (m==0 & r==1) {
    return(1)
  } else if (m > n | m < 1 | n < 1 | r < 2 | (r > min(m + n, 2 * m + 1))) {
    return(0);
  }

  sumfu <- 0
  for (u in 2:r) {
    if (u %% 2 == 0) {
      k = u / 2
      fu <- 2 * choose(m-1, k-1) * choose(n-1, k-1)
    } else {
      k = (u + 1) / 2
      fu <- choose(m-1, k-1) * choose(n-1, k-2) + choose(m-1, k-2) * choose(n-1, k-1)
    }
    sumfu <- sumfu + fu
  }
  return(sumfu / choose(m+n, m))
}

run.test.nm <- function(RES)
{
  RES <- RES[RES != 0] # Zeros are omitted.
  t <- length(RES)
  r <- RES > 0
  m <- sum(r)
  if (t > 1) {
    j <- 2:t
    run <- sum(abs(r[j] - r[j-1])) + 1
  } else {
    run <- 1
  }
  m <- min(m, t-m)
  n <- max(m, t-m)
  if (run > 1) {
    p <- run.p(m, n, run)
    if (p > 0.5) {
      p <- 1 - run.p(m, n, run-1)
    }
  } else {
    p <- 0.5^(t-1)
  }
  return(p)
}

sqrtinvcov <- function(M)
{
  evx <- eigen(as.matrix(M))
  eigvec <- evx$vectors
  eigval <- abs(evx$values)
  return(eigvec %*% diag(1/sqrt(eigval), nrow=dim(M)[1]) %*% t(eigvec))
}

OBJfpost <- function(ctlname, sdtab)
{
  # Requires sqrtinvcov(), ctlname.ETA, FCON, OM(53), SG()
  # fMETH==0: METHOD=ZERO or ZERO INTER without POSTHOC
  # fMETH==1: METHOD=ZERO or ZERO INTER with POSTHOC
  # fMETH==2: METHOD=COND with or without INTER

  FCON <- readLines('FCON')
  iESTM <- 0
  for (i in 1:length(FCON)) {
    if (substring(FCON[i],1,4) == "ESTM") iESTM <- i
  }
  ESTM <- read.fwf('FCON', rep(4,20),skip=iESTM-1,nrow=1)
  fMETH <- ESTM[1,9] + 1

  TDAT <- sdtab
  TDAT <- TDAT[TDAT[,"MDV"]==0,]
  IDs <- unique(TDAT[,"ID"])                      # Extract IDs

  OM <- as.matrix(read.table("fort.53"))
  NETA <- length(OM[1,])
  OM <- OM[1:NETA,]

  if (file.exists("fort.54")) {
    SG <- as.matrix(read.table("fort.54"))
    NEPS <- length(SG[1,])
    SG <- SG[1:NEPS,]
  } else {
    NEPS <- 1
    SG <- matrix(1)
  }

  etanames <- "ID"
  for (i in 1:NETA) {
    etanames <- c(etanames, paste("ETA",i,sep=""))
  }

  if (fMETH==0) {
    ETAr <- matrix(rep(0, NETA*length(IDs)), ncol=NETA)
    ETA <- cbind(IDs, ETAr)
    colnames(ETA) <- c(etanames)
  } else {
    ETAs <- read.table(paste(ctlname,".ETA", sep=""),header=T, skip=1)
    ETA <- as.matrix(ETAs[,etanames])
  }

  gnames <- vector()
  for(i in 1:NETA) gnames <- append(gnames, paste("G",i,"1",sep=""))
  hnames <- vector()
  for(i in 1:NEPS) hnames <- append(hnames, paste("H",i,"1",sep=""))

  CWRE <- vector()
  OFVi <- cbind(IDs, rep(NA, length(IDs)))

  DATcolnames <- colnames(TDAT)
  nDATcol <- length(DATcolnames)

  for(i in 1:length(IDs)) {
# Obsolete code does not work in case of nrow==1
#   DATi <- TDAT[TDAT[,"ID"]==IDs[i],]
    DATi <- matrix(as.matrix(TDAT[TDAT[,"ID"]==IDs[i],]), ncol=nDATcol)
    colnames(DATi) <- DATcolnames
    Yi <- DATi[,"DV"]
    F0i <- DATi[,"PRED"]
    F1i <- DATi[,"IPRE"]
    Gi <- matrix(as.matrix(DATi[,gnames]), ncol=NETA)
    Hi <- matrix(as.matrix(DATi[,hnames]), ncol=NEPS)
    RES0i <- Yi - F0i
    RES1i <- Yi - F1i
    COVi <- Gi %*% OM %*% t(Gi) + diag(diag(Hi %*% SG %*% t(Hi)), nrow=length(Yi))

    if (fMETH==0) {                          # Calculate OFVi only
      WRESi <- sqrtinvcov(COVi) %*% RES0i
    } else {                                 # Calculate CWRES also
      ETAi <- ETA[ETA[,"ID"]==IDs[i],2:(NETA+1)]
      WRESi <- sqrtinvcov(COVi) %*% (RES1i + Gi %*% ETAi)
    }
    OFVi[i,2] <- determinant(COVi,logarithm=T)$modulus[1] + t(WRESi) %*% WRESi
    CWRE <- append(CWRE, WRESi)
  }
  output <- list(sum(OFVi[,2]), OFVi, cbind(TDAT,CWRE))
  names(output) <- list("OFV", "OFVi", "CWRES")
  return(output)
}


## Test of residual counts using Binomial distribution

ResTest <- function(ResTab, TestCols=c("WRES","CWRE","IWRE"), ZVals=c(0,1,2,3), PctVals=c(0.005,0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5))
{
  nPct <- length(PctVals)
  for (i in 1:nPct) {
    ZVals <- union(ZVals, abs(qnorm(PctVals[i])))
  }
  ZVals <- sort(ZVals)
  nZVals <- length(ZVals)

  nRec <- dim(ResTab)[1]
  nTestCols <- length(TestCols)
  CountCols <- c("Z-value","Percent","Expected","LB","UB")
  for (i in 1:nTestCols) {
    CountCols <- c(CountCols, paste(TestCols[i],"Cnt"), paste(TestCols[i],"p-val"))
  }

  Count <- matrix(nrow=2*nZVals, ncol=length(CountCols))
  colnames(Count) <- CountCols

  for (i in 1:nZVals) {
    Count[i,       "Z-value"]  <- round(-1 * ZVals[nZVals+1-i], digits=3)
    Count[i+nZVals,"Z-value"]  <- round(ZVals[i],               digits=3)
    NegProb <- pnorm(-1 * ZVals[nZVals+1-i])
    PosProb <- 1 - pnorm(ZVals[i])
    Count[i,       "Percent"]  <- round(NegProb * 100,  digits=2)
    Count[i+nZVals,"Percent"]  <- round(PosProb * 100,  digits=2)
    Count[i,       "Expected"] <- round(NegProb * nRec, digits=1)
    Count[i+nZVals,"Expected"] <- round(PosProb * nRec, digits=1)
    Count[i,       "LB"]       <- qbinom(0.025, nRec, NegProb)
    Count[i,       "UB"]       <- qbinom(0.975, nRec, NegProb) - 1
    Count[i+nZVals,"LB"]       <- qbinom(0.025, nRec, PosProb)
    Count[i+nZVals,"UB"]       <- qbinom(0.975, nRec, PosProb) - 1

    for (j in 1:length(TestCols)) {
      NegCount <- length(ResTab[ResTab[,paste(TestCols[j])] < (-1 * ZVals[nZVals+1-i]), paste(TestCols[j])])
      PosCount <- length(ResTab[ResTab[,paste(TestCols[j])] > ZVals[i], paste(TestCols[j])])

      NegPVal  <- pbinom(NegCount, nRec, NegProb)
      PosPVal  <- pbinom(PosCount, nRec, PosProb)

      Count[i,        paste(TestCols[j], "Cnt")]   <- NegCount
      Count[i+nZVals, paste(TestCols[j], "Cnt")]   <- PosCount

      Count[i,        paste(TestCols[j], "p-val")] <- round(min(NegPVal,1-NegPVal), digits=3)
      Count[i+nZVals, paste(TestCols[j], "p-val")] <- round(min(PosPVal,1-PosPVal), digits=3)
    }
  }
  return(Count)
}
## NONMEM Table manipulation

RenCol <- function(ColList, OldCol, NewCol)
{
# This version uses only column name list not the table itself
  for (i in 1:nCol) if (ColList[i] == OldCol) ColList[i] <- NewCol
  return(ColList)
}


RmvCol <- function(Tab, OldCols)
{
# If there is no old column, just return the same.
# Several columns are possible.
# Different coding follows;
#  if (intersect(colnames(Tab), OldCols) != OldCols) {
#    print("There is no old column(s)")
#    return(NULL)
#  }

  return(Tab[,setdiff(colnames(Tab),OldCols)])
}


ExpandDoseHist <- function(DoseHistTab)
{
# Input Table Columns: c("TIME", "AMT", "II", "ADDL")
# Time should be in numeric format with unit="hours"
# How about "RATE", "DUR", "sDT", "DATE", "oTIME"? -> Other columns are unnaffected.

  RetTab <- DoseHistTab
  nDoseRec <- dim(RetTab)[1]
  TempRec  <- matrix(nrow=1, ncol=dim(RetTab)[2])
  colnames(TempRec) <- colnames(RetTab)

  for (j in 1:nDoseRec) {
    cADDL <- RetTab[j, "ADDL"]
    if (cADDL > 0) {
      cTIME <- RetTab[j, "TIME"]
      cAMT  <- RetTab[j, "AMT"]
      cII   <- Adm0i[j, "II"]
      for (k in 1:cADDL) {
        RetTab[j,"II"]    <- 0
        RetTab[j,"ADDL"]  <- 0
        TempRec           <- RetTab[j,]
        TempRec[1,"TIME"] <- cTIME + k * cII
        TempRec[1,"AMT"]  <- cAMT
        RetTab <- rbind(RetTab, TempRec)
      }
    }
  }

  RetTab <- RetTab[order(RetTab[,"TIME"]),]

  # Check too short interval
  IIs <- unique(DoseHistTab[DoseHistTab[,"II"] > 0, "II"])
  mII <- min(IIs)

  nRetRec <- dim(RetTab)[1]
  pTIME <- -1 * max(IIs)
  for (j in 1:nRetRec) {
    cTIME <- RetTab[j,"TIME"]
    if ((cTIME - pTIME) < 0.5*mII) {
      print("Warning: Maybe too short interval")
      print(RetTab[j,])
    }
    pTIME <- cTIME
  }
  # End of check

  return(RetTab)
}

RemoveNA = function(RawData)
{
  nRow = nrow(RawData)
  Index = vector(length=nRow)
  for (i in 1:nRow) {
    Index[i] = !any(is.na(RawData[i,]))
  }
  return(RawData[Index,])
}

mlr2 <- function(y, x.raw, standardize=0)
{
  x.raw = RemoveNA(x.raw)

  if (kappa(x.raw) > 999 & standardize==0) cat(paste("Condition Number is ", kappa(x.raw), ". Consider standardization !\n", sep=""))
  if (length(y) != length(x.raw[,1])) {
    cat("Numbers of rows of x matrix and y vector are different.\n")
    return(NULL)
  }

  n <- length(y)
  namelist <- c("Intercept", names(x.raw))

  x.avg <- matrix(rep(mean(x.raw, na.rm=T), n), nrow=n, byrow=T)
  if (standardize==3) {
    x.sd <- matrix(rep(sd(x.raw, na.rm=T), n), nrow=n, byrow=T)
    x <- (x.raw - x.avg) / x.sd
  } else if(standardize==2) {
    x <- x.raw / x.avg
  } else if(standardize==1) {
    x <- x.raw - x.avg
  } else {
    x <- x.raw
  }

  Intercept <- 1
  x <- as.matrix(cbind(Intercept, x))

  b <- ginv(t(x) %*% x) %*% t(x) %*% y
  p <- length(b)

  y.hat <- x %*% b
  e     <- y - y.hat
  SSE   <- sum(e^2)
  MSE   <- SSE/(n - p)
  b.se  <- sqrt(diag(as.numeric(MSE) * ginv(t(x) %*% x)))
  b.t   <- b/b.se
  b.p   <- pt(b.t, n - p)
  for (i in 1:p) {
    if (b.p[i] > 0.5) b.p[i] <- 1 - b.p[i]
  }
  b.p <- 2 * b.p

#  if (standardize == 2) {
#    b[-1] <- b[-1] * x.avg[1,]
#    b.se[-1] <- b.se[-1] * x.avg[1,1]
#  }

  res1 <- data.frame(namelist, cbind(b, b.se, b.t, b.p))
  names(res1) <- c("Variable", "Estimate", "SE", "T", "p-value")

  h    <- as.matrix(diag(x %*% ginv(t(x) %*% x) %*% t(x)))
  sr   <- e / sqrt(MSE * (1 - h))
  MSEi <- ( (n - p)*MSE - e^2/(1 - h) ) / (n - p - 1)
  sdr  <- e / sqrt(MSEi * (1 - h))

  DFFITS <- sqrt(h/(1 - h)) * e / sqrt(MSEi*(1 - h))

  bi <- matrix(nrow=n, ncol=p)
  for (i in 1:n) {
    z <- x[-i,]
    bi[i,] <- ginv(t(z) %*% z) %*% t(z) %*% y[-i]
  }
  bm <- matrix(rep(t(b),n), byrow=T, ncol=p)
  DFBETAS = (bm - bi)/sqrt(MSEi %*% diag(ginv(t(x) %*% x)))

  COVRATIO <- matrix(nrow=n)
  for (i in 1:n) {
    COVRATIO[i] <- det(MSEi[i] * ginv(t(x[-i,]) %*% x[-i,])) / det(MSE*ginv(t(x) %*% x))
  }

  D <- e^2 / (1-h)^2 * h / (p * MSE)

# Changed from mlr()
  res2 <- data.frame(cbind(y.hat, e, sdr, h, D, COVRATIO, DFFITS, DFBETAS))
  names(res2) <- c("Yhat", "Residual", "R-Student", "hat", "Cook's D", "COV-Ratio", "DFFITS", namelist)

  result <- list(res1, res2, n, p, n-p, SSE, MSE)
  if (standardize == 1 | standardize == 2 | standardize == 3) {
    names(result) <- c("Model Estimates with Standardization", "Influence Diagnostics with DFBETAs", "n", "Parameter Count", "Degree of Freedom", "SSE", "MSE")
  } else {
    names(result) <- c("Model Estimates", "Influence Diagnostics with DFBETAs", "n", "Parameter Count", "Degree of Freedom", "SSE", "MSE")
  }

  return(result)
}

## 2013-01-04

RmvFixed = function(Table)
{
  ColNames = colnames(Table)
  NotFixed = rep(FALSE, length(ColNames))
  for (i in 1:dim(Table)[1]) {
    for (j in 1:length(ColNames)) {
      if (Table[i,ColNames[j]] != Table[1,ColNames[j]]) NotFixed[j] = TRUE
    }
  }
  return(Table[,ColNames[NotFixed]])
}

RmvZero = function(SymmMat)
{
  nRow = dim(SymmMat)[1]
  NonZero = rep(FALSE, nRow)

  for (i in 1:nRow) {
    for (j in 1:i) {
      if (SymmMat[i,j] != 0) NonZero[i] = TRUE
    }
  }
  return(as.matrix(SymmMat[NonZero,NonZero]))
}

ParseOut = function(TagString, RawRead)
{
  Inter = grep(TagString, RawRead, value=TRUE)
  if (length(Inter) > 0) {
    return(strsplit(Inter,":")[[1]][2])
  } else {
    return(0)
  }
}

ParseItemName = function(RawRead)
{
  Tag1 = "0LABELS FOR DATA ITEMS:"
  Tag2 = "LABELS FOR PRED-DEFINED ITEMS:"

  Line1 = grep(Tag1, RawRead)
  Line2 = grep(Tag2, RawRead)

  Ans = vector()
  for (i in (Line1+1):(Line2-1)) {
     Ans = c(Ans, strsplit(RawRead[i]," ")[[1]][-1])
  }
  return(Ans)
}

ParseItemWidth = function(RawRead)
{
  Tag1 = "0FORMAT FOR DATA:"
  Line1 = grep(Tag1, RawRead)

  Str1 = sub("(\\()", "", RawRead[Line1+1])
  Str2 = sub("(\\))", "", Str1)
  Str3 = sub("( )", "", Str2)
  Str4 = sub("(\\/)", ",", Str3)
  Str5 = sub("(\\F)", "E", Str4)

  Fmts = strsplit(Str5,",")[[1]]
  n = length(Fmts)
  Widths = vector()
  for (i in 1:n) {
    x = strsplit(Fmts[i], c("E", "F"))[[1]]
    if (x[1] == "")
      { r = 1
    } else {
      r = as.integer(x[1])
    }
    for (j in 1:r) {
      Widths = c(Widths, as.integer(x[2]))
    }
  }
  return(Widths)
}

BtwTagVal = function(Tag1, Tag2, RawRead)
{
   return(sub(Tag2,"",sub(Tag1,"", grep(paste(Tag1,"(.*?)",Tag2,sep=""), RawRead, value=TRUE))))
}

BtwTagLines = function(Tag1, Tag2, RawRead)
{
  Line1 = grep(Tag1, RawRead)
  Line2a = grep(Tag2, RawRead)

  if (length(Line1) > 0) {
    if (length(Line2a) > 1) {
      i = 1
      while (Line2a[i] < Line1) i = i + 1
      Line2 = Line2a[i]
    } else {
      Line2 = Line2a
    }
    return(RawRead[(Line1+1):(Line2-1)])
  } else { return(0) }
}

BtwTagVals = function(Tag, RawRead)
{
  Tag1 = paste("<",Tag,">",sep="")
  Tag2 = paste("</",Tag,">",sep="")
  Line1 = grep(Tag1, RawRead)
  Line2 = grep(Tag2, RawRead)
  if (length(Line1) != 0 && length(Line2) != 0) {
    TmpRaw = RawRead[(Line1 + 1):(Line2 - 1)]
    nVal = length(TmpRaw)
    Vals = rep(0, nVal)
    for (i in 1:nVal) {
      Vals[i] = as.numeric(BtwTagVal(paste("<nm:val nm:name='",i,"'>",sep=""), "</nm:val>", TmpRaw[i]))
    }
    return(Vals)
  } else { return(0) }
}

BtwTagMat = function(Tag, RawRead, nRow)
{
  Tag1 = paste("<nm:",Tag,">",sep="")
  Tag2 = paste("</nm:",Tag,">",sep="")
  MatLines = BtwTagLines(Tag1, Tag2, RawRead)
  RetMat = matrix(rep(1e+10, nRow*nRow), nrow=nRow, ncol=nRow)

  if(MatLines != 0) {
    for (i in 1:nRow) {
      MatRow = BtwTagLines(paste("<nm:row nm:rname='",i,"'>",sep=""),"</nm:row>",MatLines)
      for (j in 1:i) {
        RetMat[i,j] = RetMat[j,i] =  as.double(BtwTagVal(paste("<nm:col nm:cname='",j,"'>",sep=""),"</nm:col>",MatRow))
      }
    }
  }
  return(RetMat)
}

####################### Util functions for Summarize

GetCurModelName = function()
{
  vPath = strsplit(getwd(), "/")[[1]]
  n = length(vPath)
  return(strsplit(vPath[n],"(\\.)")[[1]][1])
}

GetOFV = function()
{
  EXTName = paste0(GetCurModelName(),".ext")
  if (length(intersect(toupper(list.files()), toupper(EXTName))) == 1) {
    EXT = read.table(EXTName, skip=1, header=TRUE)
    return(tail(EXT[EXT[,"ITERATION"] > 0,"OBJ"],1))
  } else {
    return(NA)
  }
}

GetEstMethod = function()
{
  EXTName = paste0(GetCurModelName(),".ext")
  if (length(intersect(toupper(list.files()), toupper(EXTName))) == 1) {
    EstString = strsplit(readLines(EXTName, n=1),":")[[1]][2]
    EstMethod = c("FO", "FOI", "FOCE", "FOCEI", "L", "LI")
    names(EstMethod) = c(" First Order",
                " First Order with Interaction",
                " First Order Conditional Estimation",
                " First Order Conditional Estimation with Interaction",
                " Laplacian Conditional Estimation",
                " Laplacian Conditional Estimation with Interaction")
    return(EstMethod[[EstString]])
  } else {
    return(NA)
  }
}

GetProbVal = function(Tag)
{
  if (length(intersect(toupper(list.files()),"FCON")) == 1) {
    FCON = readLines("FCON")
    PROB = strsplit(Trim(substr(FCON[grep("PROB", FCON)], 9, 80))," ")[[1]]
    Loc = grep(Tag, PROB)
    if (Loc > 0) {
      return(substr(PROB[Loc], 3, nchar(PROB[Loc])))
    } else {
      return("")
    }
  } else {
    return(NA)
  }
}

FileTag = function(FileName, Tag)
{
  if (length(intersect(toupper(list.files()),toupper(FileName))) == 1) {
    if (!is.na(pmatch(Tag, readLines(FileName)))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(NA)
  }
}

MinSuccess = function()
{
  return(FileTag("PRINT.OUT", "MINIMIZATION SUCCESSFUL"))
}

SESuccess = function()
{
  return(FileTag("PRINT.OUT", "********************                            STANDARD ERROR OF ESTIMATE"))
}

GetCountPara = function()
{
  GRDName = paste0(GetCurModelName(),".grd")
  if (length(intersect(toupper(list.files()),toupper(GRDName))) == 1) {
    GRD = read.table(GRDName, skip=1, header=TRUE)
    return(ncol(GRD) - 1)
  } else {
    return(NA)
  }
}

GetCountAllTheta = function()
{
  EXTName = paste0(GetCurModelName(),".ext")
  if (length(intersect(toupper(list.files()),toupper(EXTName))) == 1) {
    EXT = read.table(EXTName, skip=1, header=TRUE)
    return(length(grep("THETA", colnames(EXT))))
  } else {
    return(NA)
  }
}

GetCountUnfixedTheta = function()
{
  return(GetCountPara() - GetCountOmega() - GetCountUnfixedEps())
}

GetCountFixedTheta = function()
{
  return(GetCountAllTheta() - GetCountUnfixedTheta())
}

GetCountEta = function()
{
  PHIName = paste0(GetCurModelName(),".phi")
  if (length(intersect(toupper(list.files()),toupper(PHIName))) == 1) {
    PHI = read.table(PHIName, skip=1, header=TRUE)
    return(length(grep("ETA", colnames(PHI))))
  } else {
    return(NA)
  }
}

GetCountOmega = function()
{
  EXTName = paste0(GetCurModelName(),".ext")
  if (length(intersect(toupper(list.files()),toupper(EXTName))) == 1) {
    EXT = read.table(EXTName, skip=1, header=TRUE)
    return(length(grep("OMEGA", colnames(EXT))))
  } else {
    return(NA)
  }
}

GetOffDiagOmega = function()
{
  nEta = GetCountEta()
  return(GetCountOmega() - nEta)
}

GetCountEps = function()
{
  EXTName = paste0(GetCurModelName(),".ext")
  if (length(intersect(toupper(list.files()),toupper(EXTName))) == 1) {
    EXT = read.table(EXTName, skip=1, header=TRUE)
    return(length(grep("SIGMA", colnames(EXT))))
  } else {
    return(NA)
  }
}

GetCountUnfixedEps = function()
{
  EXTName = paste0(GetCurModelName(),".ext")
  if (length(intersect(toupper(list.files()),toupper(EXTName))) == 1) {
    EXT = read.table(EXTName, skip=1, header=TRUE)
    EXT = EXT[EXT[,"ITERATION"] > 0,]
    nRow = nrow(EXT)
    ColNo = grep("SIGMA", colnames(EXT))
    nCol = length(ColNo)

    nUnfixed = 0
    for (i in 1:nCol) {
      for (j in 2:nRow) {
        if (EXT[j,ColNo[i]] != EXT[j-1,ColNo[i]]) {
          nUnfixed = nUnfixed + 1
          break
        }
      }
    }
    return(nUnfixed)
  } else {
    return(NA)
  }
}

GetCountObs = function()
{
  if (length(intersect(list.files(),"PRINT.OUT")) == 1) {
    PRINTOUT = readLines("PRINT.OUT")
    Tag = "TOT. NO. OF OBS RECS:"
    LineNo = grep(Tag, PRINTOUT)
    return(as.integer(substr(PRINTOUT[LineNo], nchar(Tag)+1, 80)))
  } else {
    return(NA)
  }
}

GetAICc = function()
{
  n = GetCountObs()
  p = GetCountPara()
  ofv = GetOFV()
  return(ofv + 2 * p + 2 * p * (p + 1) / (n - p - 1))
}

MatchEnd = function(vName0, End0, IgnoreCase=TRUE)
{
  if (IgnoreCase == TRUE) {
    vName = toupper(vName0)
    End = toupper(End0)
  } else {
    vName = vName0
    End = End0
  }

  n = length(vName)
  vRet = rep(FALSE, n)
  nc1 = nchar(End)

  for (i in 1:n) {
    nc2 = nchar(vName[i])
    if (substr(vName[i], nc2 - nc1 + 1, nc2) == End) vRet[i] = TRUE
  }

  return(vName0[vRet])
}

GetModelNames = function(vName)
{
  ModelNames = character()
  n = length(vName)

  for (i in 1:n) {
    vStr = strsplit(vName[i],"(\\.)")[[1]]
    if (length(vStr) != 2) stop("Folder Name should not have more than one period!")
    ModelNames = c(ModelNames, vStr[1])
  }
  return(ModelNames)
}

SumOut1 = function(ModelName)
{
  OutName = ModelName
  Parent = GetProbVal("P:")
  EstMethod = GetEstMethod()
  Formula = GetProbVal("F:")
  Minimize = MinSuccess()
  OFV = GetOFV()
  SE = SESuccess()
  Parameter = GetCountPara()
  Theta = GetCountAllTheta()
  FixedTheta = GetCountFixedTheta()
  Eta = GetCountEta()
  Eps = GetCountEps()
  OffDiagOmega = paste(GetOffDiagOmega(),Eta*(Eta - 1)/2,sep="/")
  nDV = GetCountObs()
  AICc = GetAICc()
  return(data.frame(OutName,Parent,EstMethod,Formula,Minimize,OFV,SE,Parameter,Theta,FixedTheta,Eta,Eps,OffDiagOmega,nDV,AICc,stringsAsFactors=FALSE))
}

SumOut = function(FileExt=".MDL", RunExt=".R74", OutExt=".OUT")
{
  WorkDir = getwd()
  Folders = list.dirs(path=WorkDir, full.names=FALSE, recursive=FALSE)
  RunFolders = MatchEnd(Folders, RunExt)
  nRun = length(RunFolders)
  ModelNames = GetModelNames(RunFolders)

  MDL = data.frame(OutName=character(), Parent=character(), EstMethod=character(),
                   Formula=character(), Minimize=logical(), OFV=double(),
                   SE=logical(), Parameter=integer(), Theta=integer(),
                   FixedTheta=integer(), Eta=integer(), Eps=integer(),
                   OffDiagOmega=character(), nDV=integer(), AICc=double())

  for (i in 1:nRun) {
    setwd(paste(WorkDir,RunFolders[i],sep="/"))
    MDL = rbind(MDL, SumOut1(ModelNames[i]))
  }

  nCtl = nrow(MDL)

  MDL = cbind(MDL, data.frame(NextSib=character(length=nCtl),
                              FirstKid=character(length=nCtl),
                              LastKid=character(length=nCtl),
                              LOrder=integer(length=nCtl),
                              Depth=integer(length=nCtl),
                              Pos=double(length=nCtl), stringsAsFactors=FALSE))

  for (i in 1:nCtl) {
    if (is.na(MDL[i,"Parent"])) MDL[i,"Parent"] = ""
    if (toupper(MDL[i,"Parent"]) == "ROOT") MDL[i,"Parent"] = ""
    if (length(intersect(MDL[i,"Parent"], MDL[,"OutName"])) == 0) MDL[i,"Parent"] = ""
  }

  for (i in 1:nCtl) {
    cCtl = MDL[i,"OutName"]
    Kids = MDL[MDL[,"Parent"]==cCtl,]
    if (nrow(Kids) > 0) {
      MDL[i,"FirstKid"] = Kids[1,"OutName"]
      MDL[i,"LastKid"] = tail(Kids[,"OutName"],1)
    }

    cPar = MDL[i,"Parent"]
    Sibs = MDL[MDL[,"Parent"]==cPar,]
    if (nrow(Sibs) > 1) {
      Loc = which(Sibs[,"OutName"]==MDL[i,"OutName"]) + 1
      if (Loc <= nrow(Sibs)) MDL[i,"NextSib"] = Sibs[Loc,"OutName"]
    }
  }

  Roots = MDL[MDL[,"Parent"]=="",]
  NextName = ""
  nRoot = nrow(Roots)
  if (nRoot > 0) NextName = Roots[1, "OutName"]

  cDepth = 0
  cPos = 0
  cLOrder = 0
  for (cLOrder in 1:nCtl) {
    cOutName = MDL[MDL[,"OutName"]==NextName,"OutName"]
    cIndex = which(MDL[,"OutName"]==cOutName)
    MDL[cIndex,"LOrder"] = cLOrder - 1
    MDL[cIndex,"Depth"] = cDepth
    MDL[cIndex,"Pos"] = cPos

    cFirstKid = MDL[cIndex,"FirstKid"]
    cNextSib = MDL[cIndex,"NextSib"]
    if (cFirstKid != "") {
      NextName = cFirstKid
      cDepth = cDepth + 1
    } else if (cNextSib != "") {
      NextName = cNextSib
      cPos = cPos + 1
    } else {
      ParName = MDL[cIndex, "Parent"]
      SibName = ""
      fFound = FALSE
      while (fFound == FALSE & ParName != "") {
        cDepth = cDepth - 1
        cIndex = which(MDL[,"OutName"] == ParName)
        SibName = MDL[cIndex, "NextSib"]
        if (SibName != "") {
          NextName = SibName
          fFound = TRUE
          cPos = cPos + 1
        } else {
          ParName = MDL[cIndex, "Parent"]
        }
      }
    }
  }

  for (i in nCtl:1) {
    if (MDL[i,"FirstKid"] != "") {
      cFirstKid = MDL[i,"FirstKid"]
      cLastKid = MDL[i,"LastKid"]
      FirstKidPos = MDL[which(MDL[,"OutName"]==cFirstKid),"Pos"]
      LastKidPos = MDL[which(MDL[,"OutName"]==cLastKid),"Pos"]
      MDL[i,"Pos"] = (FirstKidPos + LastKidPos) / 2
    }
  }

  setwd(WorkDir)
  write.csv(MDL, paste0(GetCurModelName(),FileExt), row.names=FALSE, na="")
  return(MDL)
}

ConnPoint = function(pt1, pt2)
{
  x1 <- pt1[1]
  y1 <- pt1[2]
  x2 <- pt2[1]
  y2 <- pt2[2]

  if (x1 == x2) {
    lines(c(x1,x2), c(y1,y2))
  } else {
    yi <- (y1 + y2)/2
    lines(c(x1, x1, x2, x2), c(y1, yi, yi, y2))
  }
}

Outline = function(MDL, MDLName, out.start="0", out.end="zzzzzzzz", Target="PDF")
{
  ttab <- MDL
  ttab <- ttab[ttab[,"OutName"] >= out.start & ttab[,"OutName"] <= out.end,]
  MinAICc <- min(ttab$AICc, na.rm=TRUE)

  start.x <- min(ttab$Pos)
  start.y <- min(ttab$Depth)

  maxdepth <- max(ttab$Depth)
  maxwidth <- max(ttab$Pos)
  ttab$Pos = ttab$Pos * 0.9

  if (maxdepth == 0) maxdepth = 1;

  if (Target!="PDF") {
    windows(width=max(maxwidth*4,10), height=max(maxdepth*3,10))
  }
  par(oma=c(1, 0.5, 1, 0.5), mar=c(0.5, 0.2, 1.5, 0.2))

  plot(ttab$Pos, ttab$Depth, xlim=c(start.x-maxwidth*0.05, maxwidth*1.1), ylim=c(maxdepth*1.1, start.y-maxdepth*0.1), type="n", xlab="", ylab="", axes=FALSE, main=paste("Model Development Flow of", toupper(MDLName)))
  mtext("*: Minimization Successful,  #: Std Error Available,  Theta - Eta - Off Diag - Total Parameters", side=1, cex=0.8)

  for (i in (1:length(ttab$OutName))) {
    if (ttab$Minimize[i] == TRUE) {
      sym1 <- "*"
      col1 <- "red"
    } else {
      sym1 <- " "
      col1 <- "black"
    }
    if (ttab$SE[i] == TRUE) {
      sym2 <- "#"
      col2 <- "blue"
    } else {
      sym2 <- " "
      col2 <- "black"
    }

    AICc <- ttab$AICc[i]

    if (AICc == MinAICc) {
      sym3 <- "v"
      col3 <- "brown"
    } else {
      sym3 <- " "
      col3 <- "black"
    }

    text(ttab$Pos[i], ttab$Depth[i]-0.02*maxdepth, paste(ttab$OutName[i], sym1), cex=0.7, col=col1)
    text(ttab$Pos[i], ttab$Depth[i], paste(format(ttab$OFV[i],digits=6), sym2), cex=0.65, col=col2)
    text(ttab$Pos[i], ttab$Depth[i]+0.02*maxdepth, paste(ttab$Theta[i], ttab$Eta[i], ttab$OffDiagOmega[i], ttab$Parameter[i], sep="-"), cex=0.7)
    text(ttab$Pos[i], ttab$Depth[i]+0.04*maxdepth, paste(format(AICc,digits=6), sym3), cex=0.7, col=col3)

    fs <- ttab[i,"Formula"]
    if (fs != "") {
      fs <- strsplit(fs, ",")[[1]]
      for (j in 1:length(fs)) {
        text(ttab$Pos[i], ttab$Depth[i]+((j-1)*0.016-0.02)*maxdepth, paste("          ",fs[j]), cex=0.65, adj=0)
      }
    }

    pid <- ttab$Parent[i]
    if ((!is.na(pid)) & pid != "" & sum(ttab[,"OutName"]==pid, na.rm=TRUE)==1) {
      ppt = c(ttab[ttab$OutName==paste(pid),]$Pos, ttab[ttab$OutName==paste(pid),]$Depth + max(ttab$Depth)*0.05)
      ConnPoint(c(ttab$Pos[i], ttab$Depth[i] - max(ttab$Depth)*0.03), ppt)
    }
  }
}

Decomp = function(Data)
{
  VarStat = NMVarStat(Data)
  AllVar = colnames(Data)

  DemogVar = rownames(VarStat[VarStat[,"DepID"]==1,])
  AdminVar = intersect(c("ID", "TIME", "AMT", "RATE", "CMT", "II", "SS"), AllVar)
  DVVar = intersect(c("ID", "TIME", "CMT", "DV"), AllVar)
  KnownVar = union(union(union(DemogVar, AdminVar), DVVar), c("MDV", "EVID"))
  TimeVar = setdiff(AllVar, KnownVar)
  AdminVar = union(AdminVar, TimeVar)

  Demog = unique(Data[,DemogVar])
  Admin = unique(Data[!is.na(Data[,"AMT"]),AdminVar])
  DV = unique(Data[!is.na(Data[,"DV"]),DVVar])
  Res = list(Demog, Admin, DV)
  names(Res) = c("Demog", "Admin", "DV")
  return(Res)
}

Combine = function(Demog, Admin, DV)
{
  IDs = unique(Demog[,"ID"])
  nID = length(IDs)

  nAdmin = nrow(Admin)
  nDV = nrow(DV)

  Admin = cbind(Admin, MDV = rep(1, nAdmin))
  DV = cbind(DV, MDV = rep(0, nDV))

  ColDemog = colnames(Demog)
  ColAdmin = colnames(Admin)
  ColDV = colnames(DV)
  ColAll = union(union(ColAdmin, ColDV), ColDemog)

  ToAddAdmin = setdiff(ColAll, ColAdmin)
  ToAddDV = setdiff(ColAll, ColDV)

  ToAdmin = matrix(nrow=nAdmin, ncol=length(ToAddAdmin))
  colnames(ToAdmin) = ToAddAdmin

  ToDV = matrix(nrow=nDV, ncol=length(ToAddDV))
  colnames(ToDV) = ToAddDV

  Admin = cbind(Admin, ToAdmin)
  DV = cbind(DV, ToDV)

  Final = rbind(Admin,DV) # Admin first for MDV descending order
  Final = Final[order(Final[,"ID"],Final[,"TIME"]),]
  for (i in 1:nID) {
    cID = Demog[i,"ID"]
    Final[Final[,"ID"]==cID,setdiff(ColDemog, "ID")] = Demog[i,setdiff(ColDemog, "ID")]
  }

  return(Final)
}

Permute = function(Demog, VarName)
{
  nID = length(unique(Demog[,"ID"]))
  if (nID != nrow(Demog)) stop("Demog table should have one row for each ID")

  Demog[,VarName] = sample(Demog[,VarName], nID, replace=FALSE)
  return(Demog)
}

SetRPT = function(ModelName, Var, nPerm=30, Seed, ModelExt=".CTL", NMFE="C:\\NMa\\nm73gb.bat", Divide=1, StartNum=1)
{
  if (missing(Seed)) Seed = StartNum
  set.seed(Seed + StartNum)
  BAT = character(length=nPerm)

  CtlName = paste0(ModelName,ModelExt)
  CTL = readLines(CtlName)
  Loc1 = pmatch("$INPUT", CTL)
  nLine = length(CTL)
  for (i in (Loc1+1):nLine) {
    if (substr(CTL[i],1,1) == "$") {
      break
    } else {
      CTL[i] = ""
    }
  }
  Loc2 = pmatch("$DATA", CTL)

  Loc3 = pmatch("$COV", CTL)
  CTL[Loc3] = ""

  for (i in 1:nLine) {
    if (substr(CTL[i],1,4) == "$TAB") {
      Loc4 = i
      break
    }
  }
  for (i in Loc4:nLine) CTL[i] = ""

  FDATA = read.csv(paste0(ModelName,".R74/FDATA.CSV"))
  Res = Decomp(FDATA)
  Demog = Res$Demog
  Admin = Res$Admin
  DV = Res$DV

  WorkDir = getwd()
  NewFolder = paste0(ModelName, "_", Var)

  Folders = list.dirs()
  if (length(intersect(Folders, paste0("./",NewFolder)))==0) {
    system2("mkdir", NewFolder)
  }
  setwd(paste0(WorkDir,"/",NewFolder))

  for (i in 1:nPerm) {
    DemogR = Permute(Demog, Var)
    Final = Combine(DemogR, Admin, DV)
    BaseName = paste0(substr(ModelName,1,4),sprintf("%04d",i+StartNum-1))
    CTLName = paste0(BaseName, ".CTL")
    CSVName = paste0(BaseName, ".CSV")
    write.csv(Final, CSVName, row.names=FALSE, quote=FALSE, na="")

    sINPUT = paste(colnames(Final), collapse=" ")
    CTL[Loc1] = paste("$INPUT", sINPUT)
    CTL[Loc2] = paste0("$DATA ..\\", CSVName, " IGNORE=@")
    writeLines(CTL, CTLName)

    BAT[i] = paste0("CALL ", NMFE, " ", CTLName, " ", BaseName, ".OUT -rundir=", BaseName, ".R74")
  }

  if (Divide > 1) {
    for(i in 1:(Divide-1)) {
      L = ceiling(quantile(1:nPerm,seq(0,1,1/Divide)))
      writeLines(BAT[L[i]:(L[i+1]-1)], paste0("RUN", L[i],"_",L[i+1]-1,".BAT"))
    }
    writeLines(BAT[L[Divide]:L[Divide+1]], paste0("RUN", L[Divide],"_",L[Divide+1],".BAT"))
  }
  writeLines(BAT, paste0("RUN",StartNum,"-ALL.BAT"))

  setwd(WorkDir)
}

HistDens = function(Data, xLabel, Target="Screen", OrgOFV)
{
  if (Target=="Screen") {
    windows()
  }
  h.res = hist(Data, plot=FALSE)
  h.rat = max(h.res$counts)/max(h.res$density)
  d.res = density(Data, na.rm=TRUE)
  xrange = range(d.res$x)
  yrange = c(0, max(h.res$counts, max(d.res$y)*h.rat))
  plot(h.res, xlim=xrange, ylim=yrange, xlab=xLabel, main=paste("Original set OFV=", format(OrgOFV,digits=7)," 5%=",format(quantile(Data,c(0.05)),digits=7)))
  lines(d.res$x, h.rat*d.res$y)
}

