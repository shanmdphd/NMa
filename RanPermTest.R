# Randomization Test
source("c:/nma/UtilLib.R")

setwd("d:/nm08/busulfan")
Outline(SumOut(), "Busulfan")

SetRPT(112, "HYP1", nPerm=30, Seed=1, Divide=4)

setwd("D:/NM08/Busulfan/112-HYP1")
MDL2 = SumOut()

HistDens(MDL2$OFV, xLabel="112 HYP1 RPT OFV")
setwd("D:/NM08/Busulfan/112.R73")
OrgOFV = GetOFV()
t.test(MDL2[,"OFV"], mu=OrgOFV)

### 
setwd("d:/nm08/busulfan")
SetRPT(108, "SEX", nPerm=30, Seed=1, Divide=4)

setwd("D:/NM08/Busulfan/108-SEX")
MDL2 = SumOut()
HistDens(MDL2$OFV, xLabel="108 SEX RPT OFV")
setwd("D:/NM08/Busulfan/108.R73")
OrgOFV = GetOFV()
t.test(MDL2[,"OFV"], mu=OrgOFV)

###
source("c:/nma/UtilLib.R")
ModelName = "103"
VarName = "SEX"

WorkDir = "C:/busulfan" 
setwd(WorkDir)
SetRPT(ModelName, VarName, nPerm=32, Seed=1, Divide=4)

setwd(paste0(WorkDir,"/",ModelName,"-",VarName))
MDL2 = SumOut()
HistDens(MDL2$OFV, xLabel=paste(ModelName, VarName, "Random Permuation Test OFV"))
setwd(paste0(WorkDir, "/",ModelName, ".R73"))
OrgOFV = GetOFV()
t.test(MDL2[,"OFV"], mu=OrgOFV)


###
source("c:/nma/UtilLib.R")
ModelName = "103"
VarName = "BWT"

WorkDir = "c:/busulfan" 
setwd(WorkDir)
SetRPT(ModelName, VarName, nPerm=32, Seed=1, Divide=4, StartNum=31)

setwd(paste0(WorkDir,"/",ModelName,"-",VarName))
MDL2 = SumOut()
HistDens(MDL2$OFV, xLabel=paste(ModelName, VarName, "Random Permuation Test OFV"))
setwd(paste0(WorkDir, "/",ModelName, ".R73"))
OrgOFV = GetOFV()
t.test(MDL2[,"OFV"], mu=OrgOFV)

