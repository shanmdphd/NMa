if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R-Script supplied with Pirana
### by Ron Keizer, Jan 2010
###
### Required: - Eta values outputted by NONMEME as .ETA file
###           - ggplot2 module
###
### Description: This R-script looks for a table file named ".ETA" or patab among
### the files generated using $TABLE and creates histograms of the eta
### distributions
###
### Example $TABLE record specification:
###    $TABLE ID ETA(1) ETA(2) ETA(3) FIRSTONLY NOAPPEND NOPRINT FILE=001.ETA
###
library(ggplot2)

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
names(models) <- gsub("_", "-", names(models))
model_names <- names(models)

for (i in 1:length(model_names)) {
    fname <- paste("pirana_reports/", model_names[i],"_eta_distr.pdf", sep="")
    if (file.exists (fname)){
        file.remove (fname)
    }
    mod      <- models[[model_names[i]]]
    tabfiles <- mod$tables
    eta_tab <- c(tabfiles[grep("\\.ETA", tabfiles)], tabfiles[grep("\\.eta", tabfiles)], tabfiles[grep("patab", tabfiles)])[1]
    if (!file.exists(eta_tab)) {
        cat (paste ("Expected file with eta's not found.\nStopping R execution.\n", sep=""))
        quit()
    }
    etas <- read.table (file = unlist(eta_tab)[1], header=T, skip=1)
    etas <- etas[!duplicated(etas$ID),]
    n_etas <- length(etas[1,])-1
    etas.l <- reshape(etas, idvar="ID", direction="long", varying=list(2:(n_etas+1)))
    colnames(etas.l) <- c("ID", "NETA", "ETA")

########################################################################################################
# R code for plotting starts here
    pdf (file = fname)
    pl1 <- ggplot (etas.l, aes(x=ETA, group=NETA)) +
        geom_density(aes(y= ..scaled..)) +
        geom_point(aes(y=0)) +
        facet_wrap(~NETA, scales="free_x") + xlab ("Eta") + ylab ("Relative density")
    pl2 <- plotmatrix(etas[,-1])
    print (pl1)
    print (pl2)
    dev.off()
########################################################################################################

    # open created file
    cat (paste("OUTPUT: ", fname, sep=""))
    if (file.exists(fname) && open_res) {
        if (Sys.info()['sysname'] == 'Windows') { shell.exec(paste(getwd(),"/",fname,sep="")) }  # windows
	else if (Sys.info()['sysname'] == 'Darwin') { system(paste ("open ",fname, sep="")) } # mac
	else { system(paste("xdg-open ", fname, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE) } # linux
    }

}

quit()
