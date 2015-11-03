library(mfdb)
## Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("09-wolf/02-mfdbwolf")
setup.d <- '09-wolf/00-data'
mdb<-mfdb('Iceland')

reitmapping <- read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)


defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_quarterly,
    year = 1982:2015,
    species = 'CAA')
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




source(sprintf('%s/setup-fleets.R',setup.d))
source(sprintf('%s/setup-model.R',setup.d))
source(sprintf('%s/setup-catchdistribution.R',setup.d))
source(sprintf('%s/setup-indices.R',setup.d))
#source(sprintf('%s/setup-catchstatistics.R',setup.d))

file.copy(sprintf('%s/itterfitter.sh',setup.d),gd$dir)
file.copy(sprintf('%s/run.R',setup.d),gd$dir)
file.copy(sprintf('%s/optinfofile',setup.d),gd$dir)
