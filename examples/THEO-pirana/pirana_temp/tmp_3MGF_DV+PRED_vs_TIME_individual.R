if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Ron Keizer, 2010
###
### Required: - NM table file with DV and TIME on first $TABLE record
###           - Lattice library
###
### Description: This R-script create a plot of DV+PRED versus TIME,
### split by individuals, for multiple selected models
###

if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }
library(lattice)

req.fields <- c("DV", "TIME", "PRED")
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
    fname <- paste("pirana_reports/",names(models)[i],"_DV_PRED_vs_TIME_ind.pdf", sep="")
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
        colours <- c("#3377ff", "#000000")
        pl <- xyplot (DV+PRED~TIME | factor(ID), data=tab,
                      type = c("p","l"), distribute.type= TRUE,
                      cex=0.6, col=colours,
                      key = list(text = list(c("DV", "IPRED")),
                          lines=c(0,1), col = colours, pch = c(1), type = c("p", "l")),
                      as.table=T,
                      main = paste (model_names[i],": ", mod$description, sep=""),
                      xlab="Time", ylab="Dependent variable / Ind. prediction"
                      )
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
