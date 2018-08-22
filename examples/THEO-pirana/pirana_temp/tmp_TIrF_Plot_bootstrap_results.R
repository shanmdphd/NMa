if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }


### R script supplied with Pirana
###
### Ron Keizer (2011-12)
### Adapted from "PsN R script for plotting results of bootstrap"
### Justin Wilkins (October 2005)
### Lars Lindbom (August 2006, April 2007)
###
### Required: - folder with results from PsN bootstrap
###	- assumes "ofv" is the last column before we get to parameter estimates
###
### Description: Generate histrograms displaying bootstrap estimates

library(lattice)
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
folder <-  "nmfe_C02-1_003" 

if (length(dir(folder, pattern="raw_results_")) == 0) {
  cat ("Bootstrap results not available. Are you sure the bootstrap is finished?\nStopping R execution.")
  quit()
}

# get the run name from the command.txt
cmd <- strsplit (tail(readLines(paste(folder, "/command.txt", sep="")),1), "\\s")[[1]][-1]
for (i in seq(cmd)) {
    if (substr(cmd[i],1,1) != "-") {
        mod <- cmd[i]
    }
}
mod <- strsplit(mod, "\\.")[[1]][1]

filename <- paste("pirana_reports/",mod,"_bootstrap_",folder,".pdf", sep="")
if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
if (file.exists (paste("pirana_reports/",mod,"_bootstrap_",folder,".pdf", sep=""))){
    file.remove (paste("pirana_reports/",mod,"_bootstrap_",folder,".pdf", sep=""))
}

min.failed    <- FALSE      # do we want to omit minimization failed runs?
cov.failed    <- FALSE      # do we want to omit covariance failed runs?
cov.warnings  <- TRUE       # do we want to omit covariance failed runs?
boundary      <- TRUE       # do we want to omit boundary runs?
showoriginal  <- TRUE       # show line for original estimate
showmean      <- TRUE       # show line for mean
showmedian    <- FALSE      # show line for median
show95CI      <- TRUE       # show line for 95 % confidence interval (percentile)
showquart     <- FALSE      # show line for quartiles
cols          <- c("#6666CC", "darkred", "#9999FF", "#888888")

excl.id <- c()              # exclude samples that have this individual

## read files
bootstrap.data <- read.csv(paste(folder, "/", dir(folder, pattern="raw_results_")[1],sep=""), header=T)
# incl.ids       <- read.csv(paste(folder, "/", dir(folder, pattern="included_individuals")[1], sep=""), header=F)

## replace underscores
for (i in 1:length(names(bootstrap.data))) {
  names(bootstrap.data)[i] <- gsub("\\_", "\\.", names(bootstrap.data)[i])
}

## find ofv column index
index <- 0
# names(bootstrap.data)
index <- grep("ofv", names(bootstrap.data))
par1 <- names(bootstrap.data)[index+1]
par1 <- gsub("^X[\\.]*","",par1)
names(bootstrap.data) <- gsub("^se[\\.]*", "se", names(bootstrap.data))
index2 <- grep(paste("^se", par1, sep=""), names(bootstrap.data))
par.mat <- bootstrap.data[,c((index+1):(index2-1))]
cor.mat <- cor(par.mat)
cor.mat <- cor.mat[,length(cor.mat[1,]):1]

## get number of parameters
n       <- length(colnames(bootstrap.data)) - index
nparams <- length(colnames(bootstrap.data))

## separate out original model fit
p1 <- subset(bootstrap.data, bootstrap.data$model != 0)
o1 <- subset(bootstrap.data, bootstrap.data$model == 0)

incl.flag <- rep(0,length(rownames(p1)))
for( i in excl.id ) {
  incl.flag <- incl.flag + rowSums( incl.ids == i )
}

p1 <- p1[(incl.flag==0),]
if (min.failed) {
  p1 <- subset(p1, minimization.successful == 1)
}
if (cov.failed) {
  p1 <- subset(p1, covariance.step.successful == 1)
}
if (cov.warnings) {
  p1 <- subset(p1, covariance.step.warnings == 0)
}
if (boundary) {
  p1 <- subset(p1, estimate.near.boundary == 0)
}

## stats and plots for each - 6 per sheet
layout (mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE))
par (mar=c(5.5, 3, 2, 1))
pdf (file=filename,
     paper="special", title="Bootstrap results",
     width=7, height=5)
for (i in index:nparams) {
  if (mode(p1[[i]]) == "numeric" && sum(p1[[i]],na.rm=T)) {
    sp <- summary(p1[[i]])
    # IQR <- diff(summary(p1[[i]])[c(5,2)])
    dp <- density(p1[[i]], na.rm=T)
    parmlabel <- names(p1)[i]
    qu <- quantile(p1[[i]], c(0.025, 0.975), na.rm=T)

    legend=paste("n = ", nrow(p1), sep="")
    if (showmean) {
      legend=paste(legend, "; Mean = ", sp[4], sep="")
    }
    if (showmedian) {
      legend=paste(legend, "; Median = ", sp[3], sep="")
    }
    if (showoriginal) {
      legend=paste(legend, "; Orig = ", o1[[i]], sep="")
    }

    hist(p1[[i]],
          main = paste("Bootstrap results - ", parmlabel, sep=""),
          xlab = parmlabel,
          xlim = c(min(dp$x), max(dp$x)),
          ylab = "",
          breaks = 20,
          probability = T,
          col=cols[3], border="white",
          sub=legend )
    lines(dp, lwd=2, lty=1, col=cols[2])

    if (showquart) {
      abline(v=sp[2], lwd= 1, lty=3, col=cols[4]) ## 1st quartile
      abline(v=sp[5], lwd= 1, lty=3, col=cols[4]) ## 3rd quartile
    }
    if (showmean) {
      abline(v=sp[4], lty=1, lwd=1, col=cols[4]) ## mean
    }
    if (showmedian) {
      abline(v=sp[3], lty=1, lwd=2, col=cols[4]) ## median
    }
    if (showoriginal) {
      abline(v=o1[[i]], lty=2, lwd=1, col=cols[4]) ## original
    }
    if (show95CI) {
      abline(v=qu[1], lty=3, lwd=1, col=cols[4]) ## 2.5% CL
      abline(v=qu[2], lty=3, lwd=1, col=cols[4]) ## 97.5% CL
      text(qu[1], max(dp$y), labels=signif(qu[1], digits = 3), col=cols[4], cex = .8, adj = c(0,0), pos='2')
      text(qu[2], max(dp$y), labels=signif(qu[2], digits = 3), col=cols[4], cex = .8, adj = c(0,0), pos='4')
    }
  }
}

if (!is.na(index2)) { ## correlation plot
  rgb.palette <- colorRampPalette(c("red", "white", "blue"), space="rgb")
  pl.cor <- levelplot (cor.mat, col.regions=rgb.palette(200),at=seq(-1, 1, 0.01))
  print (pl.cor)
}

dev.off()

## open created file
cat (paste("OUTPUT: ", filename, sep=""))
if (file.exists(filename) && open_res) {
    if (Sys.info()['sysname'] == 'Windows') { shell.exec(paste(getwd(),"/",filename,sep="")) }  # windows
    else if (Sys.info()['sysname'] == 'Darwin') { system(paste ("open ",filename, sep="")) } # mac
    else { system(paste("xdg-open ", filename, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE) } # linux
}

quit()
