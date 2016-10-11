source("c:/nma/UtilLib.R")
args = commandArgs(trailingOnly = TRUE)
SetRPT(ModelName=args[1], Var=toupper(args[2]), nPerm=as.integer(args[3]), Seed=1, Divide=4, StartNum=as.integer(args[4]))

