source("C:/NMa/UtilLib.R")

vPath = strsplit(getwd(), "/")[[1]]
n = length(vPath)
cFolder = strsplit(vPath[n],"(\\.)")[[1]][1]

PrepPDF(paste0(toupper(cFolder),"_Model_Flow.PDF"))
MDL = SumOut()
Outline(MDL, cFolder, Target="PDF")
ClosePDF()




