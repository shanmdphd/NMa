# PRINT.R
setwd("d:/g/desk/t/")
O1 = readLines("OUTPUT") ; O1
nLine = length(O1) ; nLine
MaxW = 1023

L1 = pmatch("1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) ", O1) ; L1
L2 = pmatch("Stop Time:", O1) ; L2
nEnd = ifelse(is.na(L2), nLine, L2)

Res = substr(O1[L1], 2, MaxW)
cLine = 2
for (i in (L1 + 1):nEnd) {
  Ch1 = substr(O1[i], 1, 1)
  if (Ch1 == "1" | Ch1 == "0") {
    Res[cLine] = ""
    Res[cLine + 1] = substr(O1[i], 2, MaxW)
    cLine = cLine + 2
  } else if (Ch1 == "+") {
    Res[cLine - 1] = paste0(Res[cLine - 1], substr(O1[i], nchar(O1[i - 1]) + 1, MaxW))
  } else {
    Res[cLine] = substr(O1[i], 2, MaxW)
    cLine = cLine + 1
  }
} ; Res
writeLines(Res, "PRINT.OUT")
