if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana for plotting sse results
###
### Ron Keizer (2012)
### Required: - folder with results from PsN vpc
###           - libraries xpose4, ggplot2, reshpae
### Description: Generates a VPC from a PsN created vpc folder
## Needs libraries xpose4, ggplot and reshape
## modified by Andreas Lindauer, November 2016

if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }
library(xpose4)
library(ggplot2)
library(reshape)

models <- list ( 
  "C01-1" = list ( 
    modelfile       = "C01-1.ctl",
    description     = "THEOPHYLLINE ORAL",
    reference_model = "",
    working_dir     = "C:/nma/examples/THEO-pirana",
    data_file       = "THEO",
    output_file     = "C01-1.out",
    tables          = c("sdtabC01-1"),
    estim           = list(
      th          = c(3.17E+00, 1.05E-01, 3.83E+01),
      th_se       = c(6.41E-01, 2.31E-02, 1.68E+00),
      om          = c(1.2, 0.251, 0.0313),
      om_se       = c(0.42,0.289,0.0198),
      om_block    = matrix(c( 1.2, 0.37, 0.137,
                              0.37, 0.251, 0.0434,
                              0.137, 0.0434, 0.0313), ncol=3),
      om_se       = c(0.42,0.289,0.0198),
      si          = c( 0.0121,  0.0543),
      si_se       = c(0.00358,0.0321),
      si_block    = matrix(c(0.0121, 0,
                             0, 0.0543), ncol=2),
      si_se_block = matrix(c(0.00358, 0,
                             0, 0.0321), ncol=2)
    )
  )
)
run_from <- list(software = "pirana", version = "2.9.8")
open_res <- 1

setwd('C:/nma/examples/THEO-pirana')
folder <-  "vpc_C01-1" 

# ------------------------------ some plot settings -------------------
idv    ='TIME'
idv.lab="Independent variable"
dv.lab="Dependent variable"
strat.name=NULL # vector of names of the stratification levels in the same order as the original levels, set NULL to use the orginal levels
ci    =95 #confidence level
up.pi =95 # upper percentile of prediction interval
lo.pi = 5 # lower percentile of prediction interval
logy  = FALSE # log transform y-axis
time.range = NULL # vector of range of idv eg. c(0,10)
drop.level = NULL # vector of excluded stratification levels

# color of CI areas
ci.out.col = "#002288"
ci.med.col = "#882222"
# line color of observed percentiles
obs.med.col="#882222"
obs.per.col="#222288"
col.obs="#444444"
line.size.per = 0.5
line.size.med = 1
#  ------------------------------------------------------------------------


command <- readLines(paste(folder, "/command.txt", sep=""))
args <- strsplit(command, " ")[[1]][-1]
for (i in seq(args)) { if(substr(args[i],0,1) != "-") { modfile <- args[i] } }
runno <- strsplit(modfile, "\\.")[[1]][1]

csv_file <- dir(folder, pattern="raw_result")[1]
fname <- paste("pirana_reports/vpc_", folder, "_bin.pdf", sep="")
if (runno != "") {
  fname <- paste("pirana_reports/",runno,"_vpc_bin.pdf", sep="")
}
if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
if (file.exists (paste("pirana_reports/vpc_",folder,"_bin.pdf", sep=""))){
    file.remove (paste("pirana_reports/vpc_",folder,"_bin.pdf", sep=""))
}

vpc.info <- paste(folder,"/vpc_results.csv", sep="")
vpctab <- paste(folder, "/", dir(folder, pattern = "^vpctab")[1], sep="")
if (!file.exists(vpctab)|!(file.exists(vpc.info))) {
  cat ("VPC output not found. The vpc in PsN probably failed.")
  quit()
}
tab <- read.vpctab(vpctab)
vpc <- read.npc.vpc.results(vpc.info)
vpc_tmp <- vpc$result.tables#[-length(vpc$result.tables)])
tab_dat <- tab@Data

## handle stratification (if present)
n_strata <- length(vpc$strata.names)

if (n_strata > 1) {

  vpc_dat <- c()
  for(i in 1:n_strata){
    tmp <- cbind(vpc_tmp[[i]],strata_no =i)
    vpc_dat <- rbind(vpc_dat, tmp)
  }
  if(is.null(strat.name)){
    vpc_dat$strata_name <- factor(vpc$strata.names[vpc_dat$strata_no])
    tab_dat$strata_name <- factor(vpc$strata.names[tab_dat$strata_no])
  } else{
    vpc_dat$strata_name <- factor(strat.name[vpc_dat$strata_no])
    tab_dat$strata_name <- factor(strat.name[tab_dat$strata_no])
  }
  # drop stratification level
  if(!is.null(drop.level)){
    vpc_dat <- subset(vpc_dat,!strata_name%in%drop.level)
    tab_dat <- subset(tab_dat,!strata_name%in%drop.level)
  }
  pl <- ggplot(tab_dat, aes(group=strata_name) )+ facet_wrap ( ~ strata_name)
} else {
  vpc_dat <- vpc_tmp
  # drop stratification level
  if(!is.null(drop.level)){
    vpc_dat <- subset(vpc_dat,!strata_name%in%drop.level)
    tab_dat <- subset(tab_dat,!strata_name%in%drop.level)
  }


  pl <- ggplot (tab_dat)
}

# enlarge limits to allow plotting of confidence intervals
xlim <- c(min(c(vpc_dat$lower, tab_dat[[idv]]), na.rm=TRUE), max(c(vpc_dat$upper, tab_dat[[idv]]), na.rm=TRUE))
ylim <- c(min(c(vpc_dat[[paste0(ci,'.CI.for.',lo.pi,'.from')]], tab_dat$DV), na.rm=TRUE), max(c(vpc_dat[[paste0(ci,'.CI.for.',up.pi,'.to')]], tab_dat$DV), na.rm=TRUE))

# for unbinned VPCs
if (is.na(vpc_dat$lower[1])) {
  vpc_dat$lower <- vpc_dat$upper - min(diff(vpc_dat$upper))/2
  vpc_dat$upper <- vpc_dat$upper + min(diff(vpc_dat$upper))/2
  xlim[2] <- xlim[2] *1.02
}

# plot all layers
pl <- pl +
    geom_rect(data=vpc_dat,aes_string(xmin='lower', xmax='upper',ymin=paste0('`',ci,'.CI.for.',lo.pi,'.from`'),
                                                     ymax=paste0('`',ci,'.CI.for.',lo.pi,'.to`')), linetype=0, alpha=0.2, fill=ci.out.col) +
    geom_rect(data=vpc_dat, aes_string(xmin='lower', xmax='upper', ymin=paste0('`',ci,'.CI.for.',up.pi,'.from`'),
                                                     ymax=paste0('`',ci,'.CI.for.',up.pi,'.to`')), linetype=0, alpha=0.2, fill=ci.out.col) +
    geom_rect(data=vpc_dat, aes_string(xmin='lower', xmax='upper',ymin=paste0('`',ci,'.CI.for.50.from`'),
                                                     ymax=paste0('`',ci,'.CI.for.50.to`')), linetype=0, alpha=0.3, fill=ci.med.col) +
    geom_point(data=tab_dat, aes_string(idv, 'DV'), colour=col.obs,shape=1) +
    geom_line(data=vpc_dat, aes_string( x='(upper+lower)/2', y="`50.real`"), size=line.size.med, colour=obs.med.col) +
    geom_line(data=vpc_dat, aes_string(x='(upper+lower)/2', y=paste0("`",lo.pi,".real`")), size=line.size.per, linetype="solid", colour=obs.per.col) +
    geom_line(data=vpc_dat, aes_string(x='(upper+lower)/2', y=paste0("`",up.pi,".real`")), size=line.size.per, linetype="solid", colour=obs.per.col) +
    xlab (idv.lab) +
    ylab (dv.lab) +
    xlim (xlim) +
    ylim (ylim) +
    theme_bw()

if(!is.null(time.range)){
  pl <- pl +coord_cartesian(xlim=time.range)
}

if(logy==TRUE){
  pl <- pl +   scale_y_log10()
}



pdf(file=fname, height=6, width=8,useDingbats=F)
print (pl)
dev.off()

## open created file
cat (paste("OUTPUT: ", fname, sep=""))
if (file.exists(fname) && open_res) {
    if (Sys.info()['sysname'] == 'Windows') { shell.exec(paste(getwd(),"/",fname,sep="")) }  # windows
    else if (Sys.info()['sysname'] == 'Darwin') { system(paste ("open ",fname, sep="")) } # mac
    else { system(paste("xdg-open ", fname, sep=""), ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE) } # linux
}

quit()
