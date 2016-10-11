source("c:/nma/UtilLib.R")
args = commandArgs(trailingOnly = TRUE)
SetRPT(ModelName=args[1], Var=toupper(args[2]), nPerm=32, Seed=1, Divide=4, StartNum=31)

