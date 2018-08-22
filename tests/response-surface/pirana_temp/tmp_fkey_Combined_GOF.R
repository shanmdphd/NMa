if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana
### by Ron Keizer/Rick Admiraal, 2010-2012
###
### Required: - NM table file with DV, PRED, IPRED, TIME, CWRES on first $TABLE record
###
### Description: This R-script create four basic GOF plots
###

library(lattice)
library(grid)

req.fields <- c("DV", "PRED", "IPRED", "TIME", "CWRES")
models <- list ( 
  "3201B101" = list ( 
    modelfile       = "3201B101.ctl",
    description     = "INDOBUFEN PKPD Simulation (R and S form PK and Platelet Aggregation)",
    reference_model = "NointerconversionbetweenS-andR-form,Thereisareferenceforthis.",
    working_dir     = "C:/nma/tests/response-surface",
    data_file       = "Indobufen_PKPD_nm.csv",
    output_file     = "3201B101.out",
    tables          = c("3201B101.FIT","3201B101.PAR","3201B101.IPK"),
    estim           = list(
      th          = c(2.54E+00, 1.98E-01, 1.08E+00, 7.37E+00, 4.01E-01, 2.25E-01, 7.89E-01, 2.90E-01, 9.51E-02, 8.44E+00, 3.32E-01, 2.01E+00, 4.94E-01, 3.81E-01, 1.44E+00, 4.09E-01, 2.89E-01, 7.26E-01, 6.00E-01, 5.89E-02, 2.59E+00, 4.39E-01, 6.75E+00, 4.87E+01, -1.00E-01, 2.54E+00, 6.38E+00, 1.21E+01, 0.00E+00),
      th_se       = c(),
      om          = c(0.13, 0.237, 0.0525, 0.134, 0.0903, 0.302, 0.176, 0.154, 0.0195, 0.454, 0.0277, 0.412, 1.61, 0.677, 0.188, 0.00709),
      om_se       = NULL,
      om_block    = matrix(c( 0.13, 0.0857, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0.0857, 0.237, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0.0525, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0.134, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0.0903, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0.302, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0.176, 0.135, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0.135, 0.154, 0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0.0195, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0.454, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0277, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.412, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.61, -0.736, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.736, 0.677, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.188, 0,
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.00709), ncol=16),
      om_se       = NULL,
      si          = c( 1,  1,  1),
      si_se       = NULL,
      si_block    = matrix(c(1, 0, 0,
                             0, 1, 0,
                             0, 0, 1), ncol=3),
      si_se_block = NULL
    )
  )
)
run_from <- list(software = "pirana", version = "2.9.7")
open_res <- 1

setwd('C:/nma/tests/response-surface')
model_names <- names(models)
if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
for (i in 1:length(model_names)) {
    fname <- paste("pirana_reports/",names(models)[i],"_GOF.pdf", sep="")
    header<- paste("Goodness of fit model",names(models)[i], sep=" ")
    if (file.exists (fname)){
        file.remove (fname)
    }
    mod      <- models[[model_names[i]]]
    tab_file <- c(mod$tables[grep("sdtab", mod$tables)])[1]
    if (is.na(tab_file)) { # then assume the first table has the gof variables
        tab_file <- mod$tables[1]
    }
    if (file.exists (tab_file)) {
        tab <- read.table (tab_file, skip=1, header=T) # NONMEM table with ONEHEADER option
        m <- match (names(models[[model_names[i]]]$input_trans), colnames(tab))        # tranlation of $INPUT variables
        colnames(tab)[m] <- unlist (models[[model_names[i]]]$input_trans, use.names=F) # tranlation of $INPUT variables
        if ("MDV" %in% names(tab)) { tab <- tab[tab$MDV==0,] }                       # check if MDV present
        if ("EVID" %in% names(tab)) { tab <- tab[tab$EVID==0,] }                     # check if EVID present
        not.found <- req.fields[is.na(match(req.fields, colnames(tab)))]
        if ( length (not.found) > 0) {
          cat (paste("The variable(s)",not.found,"were not found. Please check your output tables.\nStopping R execution."))
          quit()
        }
########################################################################################################
# R code for plotting starts here
        pan2 <- function(x,y,...)    {
            panel.xyplot(x,y,...)
            panel.abline(0,1,col="black")
            panel.loess(x,y,col="red", lwd=1)
            }

        pan3 <- function(x,y,...)    {
            panel.xyplot(x,y,...)
            panel.abline(0,0,col="black")
            panel.abline(1.96,0,col="black",lty=2)
            panel.abline(-1.96,0,col="black",lty=2)
            panel.lmline(x,y,col="red",lwt=2)
        }

        ipredlim     <- range(tab$IPRED) * c(0.95, 1.05)
        dvlim        <- range(tab$DV) * c(0.95, 1.05)
        lim1         <- range(c(ipredlim, dvlim))
        reslim       <- range(tab$CWRES) * 1.05
        tick <- c(-5,-2,0,2,5)

        pl1 <- 	xyplot(DV~IPRED, data=tab,
                       panel=pan2,
                       type="p", cex=0.5,
                       ylab=list("Observed concentration",cex=0.7, just=c(0.5,2.5)),
                       xlab=list("Individual predicated concentration", cex=0.7, just=c(0.5,-1)),
                       scales=list (tck=c(1, 0),x=list(cex=0.5),y= list(cex=0.5)),
                       ylim=lim1,
                       xlim=lim1,
                       main=list("Individual predictions versus concentrations",cex=0.55,just=c(0.43,4))
                       )

        pl2 <- 	xyplot(DV~PRED, data=tab,
                       panel=pan2,
                       type="p", cex=0.5,
                       ylab=list("Observed concentration",cex=0.7, just=c(0.5,2.5)),
                       xlab=list("Population predicated concentration",cex=0.7,just=c(0.5,-1)),
                       ylim=lim1,
                       xlim=lim1,
                       scales=list (tck=c(1, 0),x=list(cex=0.5),y= list(cex=0.5)),
                       main=list("Population predictions versus concentrations",cex=0.55,just=c(0.43,4))
                       )

        pl3 <- 	xyplot(CWRES~PRED, data=tab,
                       ylim=reslim,
                       panel=pan3,
                       type="p", cex=0.5,
                       ylab=list("CWRES",cex=0.5, just=c(0.7,3)),
                       xlab=list(" Population predicted concentration", cex=0.7,just=c(0.5,-1)),
                       scales=list (tck=c(1, 0),x=list(cex=0.5),y= list(cex=0.5, at=tick)),
                       main=list("Conditional weighted residuals versus population predictions",cex=0.55,just=c(0.43,4))
                       )

        pl4 <- 	xyplot(CWRES~TIME, data=tab,
                       ylim=reslim,
                       panel=pan3,
                       type="p", cex=0.5,
                       ylab=list("CWRES",cex=0.5, just=c(0.7,3)),
                       xlab=list(" Time after dose", cex=0.7,just=c(0.5,-1)),
                       scales=list (tck=c(1, 0),x=list(cex=0.5),y= list(cex=0.5, at=tick)),
                       main=list("Conditional weighted residuals versus time",cex=0.55,just=c(0.43,4))
                       )

########################################################################################################
    } else {
        cat (paste("The table file ",tab_file," was not found. Please check your output tables.\nStopping R execution."))
        quit()
    }
    pdf (file = fname)
    print(pl3, position=c(0,-0.01,    0.52,0.51)  , more=TRUE)
    print(pl4, position=c(0.48,-0.01, 1,0.51)     , more=TRUE)
    print(pl1, position=c(0,0.45,     0.52,0.99)  , more=TRUE)
    print(pl2, position=c(0.48,0.45,  1,0.99)     , more=TRUE)
    grid.text(header,y = 0.98)
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
