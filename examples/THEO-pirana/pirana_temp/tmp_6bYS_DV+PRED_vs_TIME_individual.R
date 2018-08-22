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

library(lattice)

req.fields <- c("DV", "TIME", "PRED")
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
