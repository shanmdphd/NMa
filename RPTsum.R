source("c:/nma/UtilLib.R")
args = commandArgs(trailingOnly = TRUE)
WorkDir=getwd()
ModelName = args[1]
VarName = args[2]
setwd(paste0(WorkDir,"/",ModelName,"_",VarName))
MDL2 = SumOut()
PrepPDF(paste(toupper(ModelName),toupper(VarName),"RPT.PDF",sep="_"))

setwd(paste0(WorkDir, "/",ModelName, ".R73"))
OrgOFV = GetOFV()

#scrnmat = matrix(0, 2, 4)
#scrnmat[1,] = c(0.1, 0.9, 0.1, 0.6)
#scrnmat[2,] = c(0.1, 0.9, 0.6, 0.9)
#split.screen(scrnmat)

#screen(1)
HistDens(MDL2$OFV, xLabel=paste0(ModelName, " ", VarName, " Random Permuation Test OFV (n=",length(MDL2$OFV),")"), Target="PDF", OrgOFV)
#screen(2)
#Res = t.test(MDL2[,"OFV"], mu=OrgOFV)
#PrinMTxt(capture.output(Res))
ClosePDF()

