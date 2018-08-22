if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R-Script supplied with Pirana
### Description: Creates cwres.vs.pred() using Xpose

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
library(xpose4)

if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
model_names <- names(models)
for (i in seq(model_names)) {
    fname <- paste("pirana_reports/",names(models)[i],"_CWRES_vs_PRED.pdf", sep="")
    model     <- names(models)[i]
    new.runno <- gsub("run", "", model)
    xpdb      <- xpose.data(new.runno)
    newnam    <- paste("xpdb", new.runno, sep = "")
    assign (pos = 1, newnam, xpdb)
    assign (pos = 1, ".cur.db", xpdb)
    pl <- cwres.vs.pred(xpdb)
    pdf(fname, width=5, height=5)
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
