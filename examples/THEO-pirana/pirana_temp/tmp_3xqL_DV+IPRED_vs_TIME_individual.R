if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Ron Keizer, 2010-2012
###
### Required: - Monolix output folder
###
### Description: This R-script create a plot of 'concentration' and 'indPred_mean' versus 'time',
### for one or multiple selected models
###

library(lattice)

req.fields <- c("DV", "PRED")
models <- list ( 
  "C02-2" = list ( 
    modelfile       = "C02-2.ctl",
    description     = "THEOPHYLLINE ORAL",
    reference_model = "",
    working_dir     = "C:/nma/examples/THEO-pirana",
    data_file       = "THEO",
    output_file     = "C02-2.lst",
    tables          = c("sdtabC02-2","patabC02-2"),
    estim           = list(
      th          = c(3.24E+01, 8.73E-02, 1.49E+00),
      th_se       = c(1.68E+00, 4.24E-03, 3.02E-01),
      om          = c(0.0194, 0.0202, 0.435),
      om_se       = c(0.00849,0.0207,0.223),
      om_block    = matrix(c( 0.0194, 0.0121, 0.0569,
                              0.0121, 0.0202, -0.00647,
                              0.0569, -0.00647, 0.435), ncol=3),
      om_se       = c(0.00849,0.0207,0.223),
      si          = c( 0.017,  0.0829),
      si_se       = c(0.0121,0.118),
      si_block    = matrix(c(0.017, 0,
                             0, 0.0829), ncol=2),
      si_se_block = matrix(c(0.0121, 0,
                             0, 0.118), ncol=2)
    )
  )
)
run_from <- list(software = "pirana", version = "2.9.7")
open_res <- 1

setwd('C:/nma/examples/THEO-pirana')
names(models) <- gsub("_", "-", names(models))
model_names <- names(models)
if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
for (i in 1:length(model_names)) {
    fname <- paste("pirana_reports/",names(models)[i],"_DV_IPRED_vs_TIME_ind.pdf", sep="")
    if (file.exists (fname)){
        file.remove (fname)
    }
    mod      <- models[[model_names[i]]]
    tab_file <- paste(models[[model_names[i]]]$output_folder, "/predictions.txt", sep="")
    if (file.exists (tab_file)) {
        tab  <- read.table (tab_file, header=T, sep="\t") # NONMEM table with ONEHEADER option
        colnames(tab) <- gsub("\\.", "", colnames(tab))
#######################################################################################################
# R code for plotting starts here
        pl <- xyplot (concentration+indPred_mean~time | factor(ID), data=tab, type="l",
                      main = paste (model_names[i],": ", mod$description, sep=""),
                      auto.key=T, as.table=T, pch=19,
                      xlab="Time", ylab="Dependent variable / Individual prediction")
########################################################################################################
    } else {
        cat (paste("The table file ",tab_file," was not found. Please check your output tables.\nStopping R execution."))
        quit()
    }
    pdf (file = fname)
    print (pl)
    dev.off()

    # open created file
    cat (paste("OUTPUT: ", fname, sep=""))
    if (file.exists(fname) && open_res) {
        if (Sys.info()['sysname'] == 'Windows') { shell.exec(paste(getwd(),"/",fname,sep="")) }  # windows
	else if (Sys.info()['sysname'] == 'Darwin') { system(paste ("open ",fname, sep="")) } # mac
    else { system(paste("xdg-open ", fname, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE) } # linux
    }

}

quit()
