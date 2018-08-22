if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Coen van Hasselt, 2011
###
### Required: - Ext file from NONMEM run
###           - Lattice library
###
### Description: This R-script create a plot of run information in the ext-file,

library(lattice)

models <- list ( 
  "C02-1" = list ( 
    modelfile       = "C02-1.ctl",
    description     = "THEOPHYLLINE ORAL",
    reference_model = "",
    working_dir     = "C:/nma/examples/THEO-pirana",
    data_file       = "THEO",
    output_file     = "C02-1.lst",
    tables          = c("sdtabC02-1","patabC02-1"),
    estim           = list(
      th          = c(3.21E+01, 8.77E-02, 1.59E+00),
      th_se       = c(1.50E+00, 4.28E-03, 3.09E-01),
      om          = c(0.019, 0.0186, 0.43),
      om_se       = c(0.0071,0.0191,0.217),
      om_block    = matrix(c( 0.019, 0.0136, 0.0565,
                              0.0136, 0.0186, -0.0078,
                              0.0565, -0.0078, 0.43), ncol=3),
      om_se       = c(0.0071,0.0191,0.217),
      si          = c( 0.477),
      si_se       = c(0.124),
      si_block    = matrix(c(0.477), ncol=1),
      si_se_block = matrix(c(0.124), ncol=1)
    )
  )
)
run_from <- list(software = "pirana", version = "2.9.7")
open_res <- 1

setwd('C:/nma/examples/THEO-pirana')
model_names <- names(models)
if (!file.exists("pirana_temp")) {dir.create ("pirana_temp")}
if (file.exists (paste("pirana_temp/plot_ext_",names(models)[1],".pdf", sep=""))){
    file.remove (paste("pirana_temp/plot_ext_",names(models)[1],".pdf", sep=""))
}
filename=paste("pirana_temp/plot_ext_",names(models)[1],".pdf", sep="")
pdf (file = filename)
for (j in model_names) {
  if (!file.exists(paste(j, ".ext", sep=""))) {
    cat (paste ("Expected file (", paste(j,".ext",sep=""), ") not found.\nStopping R execution.\n", sep=""))
    quit()
  }
  con <- readLines(file(paste(j,".ext",sep="")))
  methodLines <- grep("TABLE",con)
  for(i in 1:length(methodLines)){
    start <- methodLines[i]; stop <- c(methodLines[i+1]-1)
    method <- strsplit(substr(con[methodLines[i]],start=15, stop=200),":")[[1]][1]
    if(i == length(methodLines)){stop <- length(con)}
    txt <- readLines(file(paste(j,".ext",sep="")))[start:stop]
    dat <- read.table(textConnection(txt), skip=1, header=T)
    dat <- dat[dat$ITERATION>c(-10000000),]
    dat <- cbind(rep(dat$ITERATION, ncol(dat)-1), stack(dat[,2:ncol(dat)]))
    names(dat)<-c("iteration","value","parameter")
    pl <- xyplot(value ~ iteration|as.factor(parameter), data=dat, type="l", scales="free",
               main=method, cex.main=.5, par.settings = list(par.main.text = list(cex = 0.7)))
    print(pl)
    closeAllConnections()
  }
}
dev.off()

# open created file
cat (paste("OUTPUT: ", fname, sep=""))
if (file.exists(filename) && open_res) {
    if (Sys.info()['sysname'] == 'Windows') { system(filename) }  # windows
  else if (Sys.info()['sysname'] == 'Darwin') { system(paste ("open ",filename, sep="")) } # mac
  else { system(paste("xdg-open ", fname, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE) } # linux
}

quit()
