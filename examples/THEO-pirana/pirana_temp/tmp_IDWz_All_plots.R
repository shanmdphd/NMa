if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Ron Keizer/Coen van Hasselt, 2010-2012
###
### Required: - Output tables: runX.tab, runXpar.tab
###	      - ID column in input CSV
###           - metrumrg package installed
### Description: This R-script creates a range of plots based on files available using the
###              metrumrg's PLOTR function. It is a template that can be extended.

library(metrumrg)

req.fields <- c("DV", "PRED")
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
    fname <- paste("pirana_reports/",names(models)[i],"_metrumrg_all_plots.pdf", sep="")
    if (file.exists (fname)){
        file.remove (fname)
    }
    mod      <- models[[model_names[i]]]
    tab_file <- mod$tables[1]

    if (file.exists (tab_file)) {
        tab  <- read.table (tab_file, skip=1, header=T) # NONMEM table with ONEHEADER option
        m <- match (names(models[[model_names[i]]]$input_trans), colnames(tab))        # tranlation of $INPUT variables
        colnames(tab)[m] <- unlist (models[[model_names[i]]]$input_trans, use.names=F) # tranlation of $INPUT variables
        not.found <- req.fields[is.na(match(req.fields, colnames(tab)))]
        if ( length (not.found) > 0) {
          cat (paste("The variable(s)",not.found,"were not found. Please check your output tables.\nStopping R execution."))
        }
        par(ask=FALSE)
        pl <- PLOTR(run=model_names[i],
                    ctlfile=mod$modelfile,
                    project=getwd(),
                    rundir=getwd(),
                    grp=NULL, # group by covariate value
                    onefile=TRUE, # one or multiple output files
                    plotfile=fname, # name of PDF file
                    logtrans=FALSE, # log transform DV, PRED, ..
                    dvname='DV', # axis label name to use for DV
                    grpnames=NULL, # specify group names for grp argument
                    cont.cov=NULL, # vector specifying continuous covariate names
                    cat.cov=NULL, # vector specifying categorical covariate names
                    par.list=NULL, # vector specifying parameters from *par.tab
                    eta.list=NULL, # vector specifying ETAs from either *tab or *par.tab
                    estimated = NULL,
                    superset = FALSE) # instead of dataSynthesis
    }

    # open created file
    cat (paste("OUTPUT: ", fname, sep=""))
    if (file.exists(fname) && open_res) {
        if (Sys.info()['sysname'] == 'Windows') { shell.exec(paste(getwd(),"/",fname,sep="")) }  # windows
	else if (Sys.info()['sysname'] == 'Darwin') { system(paste ("open ",fname, sep="")) } # mac
    else { system(paste("xdg-open ", fname, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE) } # linux
    }
}

quit()

