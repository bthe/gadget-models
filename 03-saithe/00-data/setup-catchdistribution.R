minage <- Rgadget:::getMinage(gm)
maxage <- Rgadget:::getMaxage(gm)


## Query length data to create IGFS catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'IGFS',
    species = 'HAD',
    length = mfdb_interval("len", seq(0, 150, by = 2))),
                                                        defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("hadimm", "hadmat")))
rm(aggdata)
## Age IGFS
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'IGFS',
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             species='HAD',
                             length = mfdb_interval("len", seq(0, 150, by = 4))),
                        defaults))

#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("hadimm", "hadmat")))
rm(aggdata)

## Maturity @3 from IGFS
aggdata <- mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                             append(defaults,
                                    list(sampling_type='IGFS',
                                         age=mfdb_group(age3=3:12),
                                         length = mfdb_step_interval('len', by = 2, to = 150),              
                                         maturity_stage = mfdb_group(hadimm = 1, hadmat = 2:5))))

gadget_dir_write(gd,
                 gadget_likelihood_component("stockdistribution",
                                             name = "matp.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("hadimm", "hadmat")))



## Query length data to create AUT catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'AUT',
    species = 'HAD',
    length = mfdb_interval("len", seq(0, 150, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.aut",
                                                 weight = 1,
                                                 data = aggdata[[1]],,
                                                 fleetnames = c("aut"),
                                                 stocknames = c("hadimm", "hadmat")))
rm(aggdata)
## Age AUT
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'AUT',
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, 150, by = 4))),
                        defaults))

#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])


gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.aut",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("aut"),
                                             stocknames = c("hadimm", "hadmat")))
rm(aggdata)


## Query length data to create lln catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    species = 'HAD',
    gear = c('LLN','HLN'),
    length = mfdb_interval("len", seq(0, 150, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.lln",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("lln"),
                                                 stocknames = c("hadimm", "hadmat")))
rm(aggdata)
## Age lln
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
                             gear = c('LLN','HLN'),
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, 150, by = 4))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.lln",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("lln"),
                                             stocknames = c("hadimm", "hadmat")))
rm(aggdata)



## Query length data to create bmt catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    species = 'HAD',
    gear = c('BMT', 'NPT'),
    length = mfdb_interval("len", seq(0, 150, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.bmt",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("bmt"),
                                                 stocknames = c("hadimm", "hadmat")))
rm(aggdata)
## Age bmt
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
                             gear = c('BMT','NPT'),
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, 150, by = 4))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.bmt",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("bmt"),
                                             stocknames = c("hadimm", "hadmat")))
rm(aggdata)

## Query length data to create gil catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    species = 'HAD',
    gear = c('GIL'),
    length = mfdb_interval("len", seq(0, 150, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.gil",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("gil"),
                                                 stocknames = c("hadimm", "hadmat")))
rm(aggdata)
## Age gil
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
                             gear = c('GIL'),
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, 150, by = 4))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.gil",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("gil"),
                                             stocknames = c("hadimm", "hadmat")))
rm(aggdata)

