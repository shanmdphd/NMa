# RENAME
CurDir = getwd()
ModelPath = strsplit(CurDir,"(\\.)")[[1]][1]
SplittedPath = strsplit(ModelPath,"(\\/)")[[1]]
nSplitted = length(SplittedPath)
ModelName = SplittedPath[nSplitted]
system(paste("C:\\NMa\\Rename.BAT", ModelName))
