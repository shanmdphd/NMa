if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Ron Keizer, 2010
###
### Required: - NM table file with DV and IPRED on first $TABLE record
###
### Description: This R-script create a plot of DV versus IPRED, for
### multiple selected models
###

library(lattice)

req.fields <- c("DV", "TIME", "PRED")
models <- list ( 
  "C07-1" = list ( 
    modelfile       = "C07-1.ctl",
    description     = "THEOPHYLLINE ORAL",
    reference_model = "",
    working_dir     = "C:/nma/examples/THEO-pirana",
    data_file       = "THEO",
    output_file     = "C07-1.lst",
    tables          = c("sdtabC07-1","patabC07-1","cotabC07-1","catabC07-1"),
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
model_names <- names(models)
if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
for (i in 1:length(model_names)) {
    fname <- paste("pirana_reports/", names(models)[i],"_DV_vs_IPRED.pdf", sep="")
    if (file.exists (fname)){
        file.remove (fname)
    }
    mod      <- models[[model_names[i]]]
    tab_file <- c(mod$tables[grep("sdtab", mod$tables)])[1]
    if (is.na(tab_file)) { # then assume the first table has the gof variables
        tab_file <- mod$tables[1]
    }
    if (file.exists (tab_file)) {
        tab      <- read.table (tab_file, skip=1, header=T) # NONMEM table with ONEHEADER option
        m <- match (names(models[[model_names[i]]]$input_trans), colnames(tab))        # tranlation of $INPUT variables
        colnames(tab)[m] <- unlist (models[[model_names[i]]]$input_trans, use.names=F) # tranlation of $INPUT variables
        colnames(tab)[match ("IPRE", colnames(tab))] <- "IPRED"
        if ("MDV" %in% names(tab)) { tab <- tab[tab$MDV==0,] }
        if ("EVID" %in% names(tab)) { tab <- tab[tab$EVID==0,] }
        colnames(tab)[match ("IPRE", names(tab))] <- "IPRED"
        not.found <- req.fields[is.na(match(req.fields, colnames(tab)))]
        if ( length (not.found) > 0) {
          cat (paste("The variable(s)",not.found,"were not found. Please check your output tables.\nStopping R execution."))
          quit()
        }
########################################################################################################
# R code for plotting starts here

        pl <- xyplot (DV~IPRED, data=tab,
                      panel = function(x, y, ...) {
                           panel.xyplot(x, y, ...)
                           panel.abline(a=0, b=1, col="#888888", lwd=2, lty=3)
                           panel.loess(x, y, lwd=3, lty=2, col.line="red", ...)
                       },
                      main = paste (model_names[i],": ", mod$description, sep=""),
                      pch=19, xlab="Individual prediction", ylab="Dependent variable")
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
