if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Ron Keizer, 2010
###
### Required: - NM table file with WRES and PRED on first $TABLE record
###
### Description: This R-script create a plot of WRES versus PRED, for
### multiple selected models
###

if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }
library(lattice)

req.fields <- c("CWRES", "PRED")
models <- list ( 
  "C07-4" = list ( 
    modelfile       = "C07-4.ctl",
    description     = "THEOPHYLLINE ORAL P:ROOT F:BASE",
    reference_model = "",
    working_dir     = "C:/nma/examples/THEO-pirana",
    data_file       = "THEOPP",
    output_file     = "C07-4.out",
    tables          = c("sdtabC07-4","patabC07-4","cotabC07-4","catabC07-4"),
    estim           = list(
      th          = c(4.05E-02, 4.66E-01, 1.48E+00, 1.00E-01, 1.53E-01),
      th_se       = c(3.15E-03, 1.94E-02, 3.11E-01, NA, 1.78E-02),
      om          = c(0.0625, 0.0143, 0.434),
      om_se       = c(0.0366,0.00618,0.232),
      om_block    = matrix(c( 0.0625, 0.0298, -0.0144,
                              0.0298, 0.0143, -0.0137,
                             -0.0144, -0.0137, 0.434), ncol=3),
      om_se       = c(0.0366,0.00618,0.232),
      si          = c( 1),
      si_se       = c(0),
      si_block    = matrix(c(1), ncol=1),
      si_se_block = matrix(c(0), ncol=1)
    )
  )
)
run_from <- list(software = "pirana", version = "2.9.8")
open_res <- 1

setwd('C:/nma/examples/THEO-pirana')
model_names <- names(models)
if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
for (i in 1:length(model_names)) {
    fname <- paste("pirana_reports/",names(models)[i],"_CWRES_vs_PRED.pdf", sep="")
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
        if ("MDV" %in% names(tab)) { tab <- tab[tab$MDV==0,] }
        if ("EVID" %in% names(tab)) { tab <- tab[tab$EVID==0,] }
        not.found <- req.fields[is.na(match(req.fields, colnames(tab)))]
        if ( length (not.found) > 0) {
          cat (paste("The variable(s)",not.found,"were not found. Please check your output tables.\nStopping R execution."))
          quit()
        }

########################################################################################################
# R code for plotting starts here
        pl <- xyplot (CWRES~PRED, data=tab,
                       main = paste (model_names[i],": ", mod$description, sep=""),
                       panel = function(x, y, ...) {
                           panel.xyplot(x, y, ...)
                           panel.abline(a=0, b=0, col="#888888", lwd=2, lty=3)
                           panel.loess(x, y, lwd=3, lty=2, col.line="red", ...)
                       },
                      pch=19, xlab="Population predictions", ylab="Conditional Weighted residuals")
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
