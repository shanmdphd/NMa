if(Sys.getenv("R_LIB") != "") { .libPaths(c(Sys.getenv("R_LIB"), .libPaths())) }

### R script supplied with Pirana for plotting sse results
###
### Ron Keizer (2012)
### Required: - folder with results from PsN vpc
###           - libraries xpose4, ggplot2, reshpae
### Description: Generates a VPC from a PsN created vpc folder
## Needs libraries xpose4, ggplot and reshape

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

command <- readLines(paste(folder, "/command.txt", sep=""))
args <- strsplit(command, " ")[[1]][-1]
for (i in seq(args)) { if(substr(args[i],0,1) != "-") { modfile <- args[i] } }
runno <- strsplit(modfile, "\\.")[[1]][1]

csv_file <- dir(folder, pattern="raw_result")[1]
fname <- paste("pirana_reports/vpc_", folder, ".pdf", sep="")
if (runno != "") {
  fname <- paste("pirana_reports/",runno,"_vpc_", folder, ".pdf", sep="")
}
if (!file.exists("pirana_reports")) {dir.create ("pirana_reports")}
if (file.exists (paste("pirana_reports/vpc_",folder,".pdf", sep=""))){
    file.remove (paste("pirana_reports/vpc_",folder,".pdf", sep=""))
}

vpc.info <- paste(folder,"/vpc_results.csv", sep="")
vpctab <- paste(folder, "/", dir(folder, pattern = "^vpctab")[1], sep="")
if (!file.exists(vpctab)|!(file.exists(vpc.info))) {
  cat ("VPC output not found. The vpc in PsN probably failed.")
  quit()
}
tab <- read.vpctab(vpctab)
vpc <- read.npc.vpc.results(vpc.info)
vpc_tmp <- data.frame(vpc$result.tables[-length(vpc$result.tables)])
tab_dat <- tab@Data

## handle stratification (if present)
strat_id <- grep("lower", names(vpc_tmp))
if (length(strat_id) > 1) {
  strat_id <- c(grep("lower", names(vpc_tmp)), (length(vpc_tmp[1,])+1))
  vpc_dat <- c()
  for (i in 1:(length(strat_id)-1)) {
    tmp <- cbind(vpc_tmp[,c(strat_id[i]:(strat_id[i+1]-1))], strata_no = i)
    if (i>1) { colnames(tmp) <- colnames(vpc_dat) }
    vpc_dat <- rbind(vpc_dat, tmp)
  }
  vpc_dat$strata_name <- vpc$strata.names[vpc_dat$strata_no]
  tab_dat$strata_name <- vpc$strata.names[tab_dat$strata]
  pl <- ggplot (tab_dat, aes(group=factor(as.character(strata_name)))) + facet_wrap ( ~ strata_name)
} else {
  vpc_dat <- vpc_tmp
  pl <- ggplot (tab_dat)
}

# enlarge limits to allow plotting of confidence intervals
xlim <- c(min(c(vpc_dat$lower, tab_dat$TIME), na.rm=TRUE), max(c(vpc_dat$upper, tab_dat$TIME), na.rm=TRUE))
ylim <- c(min(c(vpc_dat[["X95.CI.for.5.from"]], tab_dat$DV), na.rm=TRUE), max(c(vpc_dat[["X95.CI.for.95.to"]], tab_dat$DV), na.rm=TRUE))

# for unbinned VPCs
if (is.na(vpc_dat$lower[1])) {
  vpc_dat$lower <- vpc_dat$upper - min(diff(vpc_dat$upper))/2
  vpc_dat$upper <- vpc_dat$upper + min(diff(vpc_dat$upper))/2
  xlim[2] <- xlim[2] *1.02
}

# plot all layers
pl <- pl +
    geom_ribbon(data=vpc_dat, aes(x=(upper+lower)/2, ymin=vpc_dat[["X95.CI.for.5.from"]], ymax=vpc_dat[["X95.CI.for.5.to"]]), linetype=0, alpha=0.2, fill="#002288") +
    geom_ribbon(data=vpc_dat, aes(x=(upper+lower)/2, ymin=vpc_dat[["X95.CI.for.95.from"]], ymax=vpc_dat[["X95.CI.for.95.to"]]), linetype=0, alpha=0.2, fill="#002288") +
    geom_ribbon(data=vpc_dat, aes(x=(upper+lower)/2, ymin=vpc_dat[["X95.CI.for.50.from"]], ymax=vpc_dat[["X95.CI.for.50.to"]]), linetype=0, alpha=0.3, fill="#882222") +
    geom_point(data=tab_dat, aes(TIME, DV), colour="#444444") +
    geom_line(data=vpc_dat, aes( (vpc_dat$upper+vpc_dat$lower)/2, vpc_dat[["X50.real"]]), size=1.5, colour="#882222") +
    geom_line(data=vpc_dat, aes( (vpc_dat$upper+vpc_dat$lower)/2, vpc_dat[["X5.real"]]), size=.5, linetype="solid", colour="#222288") +
    geom_line(data=vpc_dat, aes( (vpc_dat$upper+vpc_dat$lower)/2, vpc_dat[["X95.real"]]), size=.5, linetype="solid", colour="#222288") +
    xlab ("Independent variable") +
    ylab ("Dependent variable") +
    xlim (xlim) +
    ylim (ylim)

pdf(file=fname, height=6, width=8)
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
