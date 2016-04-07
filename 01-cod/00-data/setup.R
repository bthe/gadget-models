library(mfdb)
library(Rgadget)
## Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("../02-mfdbcod")

mdb<-mfdb('Iceland')

reitmapping <- read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)


defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_quarterly,
    year = 1982:2015,
    species = 'COD')
## Write out areafile and update mainfile with areafile location
gadget_dir_write(gd, gadget_areafile(
    size = mfdb_area_size(mdb, defaults)[[1]],
    temperature = mfdb_temperature(mdb, defaults)[[1]]))
## Write a penalty component to the likelihood file
gadget_dir_write(gd, gadget_likelihood_component("penalty",
                                                 name = "bounds",
                                                 weight = "0.5",
                                                 data = data.frame(
                                                     switch = c("default"),
                                                     power = c(2),
                                                     upperW=10000,
                                                     lowerW=10000,
                                                     stringsAsFactors = FALSE)))

gadget_dir_write(gd, gadget_likelihood_component("understocking",
                                                 name = "understocking",
                                                 weight = "100"
                                                 ))




source('setup-fleets.R')
source('setup-model.R')
source('setup-catchdistribution.R')
source('setup-indices.R')
source('setup-catchstatistics.R')

file.copy('itterfitter.sh',gd$dir)
file.copy('run.R',gd$dir)
file.copy('optinfofile',gd$dir)
