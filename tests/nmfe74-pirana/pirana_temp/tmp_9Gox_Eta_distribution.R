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
  "CONTROL5" = list ( 
    modelfile       = "CONTROL5.ctl",
    description     = "THEOPHYLLINE POPULATION DATA",
    reference_model = "",
    working_dir     = "C:/nma/tests/nmfe74-pirana",
    data_file       = "THEOPP",
    output_file     = "CONTROL5.out",
    tables          = c(""),
    input_trans     = list('DOSE' = 'AMT', 'CP' = 'DV'),
    estim           = list(
      th          = c(2.77E+00, 7.81E-02, 3.63E-02),
      th_se       = c(7.08E-01, 7.26E-03, 4.53E-03),
      om          = c(5.55, 0.00024, 0.515),
      om_se       = c(4.83,0.000122,0.212),
      om_block    = matrix(c( 5.55, 0.00524, -0.128,
                              0.00524, 0.00024, 0.00911,
                             -0.128, 0.00911, 0.515), ncol=3),
      om_se       = c(4.83,0.000122,0.212),
      si          = c( 0.388),
      si_se       = c(0.105),
      si_block    = matrix(c(0.388), ncol=1),
      si_se_block = matrix(c(0.105), ncol=1)
    )
  )
)
run_from <- list(software = "pirana", version = "2.9.7")
open_res <- 1

setwd('C:/nma/tests/nmfe74-pirana')
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
