if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Ron Keizer, 2010-2012
###
### Required: - NM table file with a $TABLE record
###           - ggplot2 library
###
### Description: This R-script shows how to use arguments in R scrfipts
###

args <- list ()

library(ggplot2)

req.fields <- c(arg$x_var, arg$y_val)
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
    fname <- paste("pirana_reports/",names(models)[i],"_",arg$y_var,"_vs_",arg$x_var,".pdf", sep="")
    if (file.exists (fname)){
        file.remove (fname)
    }
    mod      <- models[[model_names[i]]]
    tab_file <- mod$tables[1]
    if (file.exists (tab_file)) {
        tab  <- read.table (tab_file, skip=1, header=T) # NONMEM table with ONEHEADER option
        if (nchar(arg$subset) > 2) { # subset
            tab <- subset(tab, eval(parse(text=arg$subset)))
        }
        if ("MDV" %in% names(tab)) { tab <- tab[tab$MDV==0,] }                       # check if MDV present
        if ("EVID" %in% names(tab)) { tab <- tab[tab$EVID==0,] }                     # check if EVID present
        not.found <- req.fields[is.na(match(req.fields, colnames(tab)))]
        if ( length (not.found) > 0) {
          cat (paste("The variable(s)",not.found,"were not found. Please check your output tables.\nStopping R execution."))
          quit()
        }
########################################################################################################
# R code for plotting starts here
        pl <- ggplot (data = tab, aes (x=get(arg$x_var), y=get(arg$y_var))) +
                 geom_point() +
                 xlab (arg$xlab) +
                 ylab (arg$ylab) 
        if (arg$split_id == 1) {
            pl <- pl + facet_wrap(~ID)
        }
    }
########################################################################################################
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
