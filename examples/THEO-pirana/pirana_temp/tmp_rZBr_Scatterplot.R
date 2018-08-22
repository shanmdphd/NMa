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
