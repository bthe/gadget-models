minage <- Rgadget:::getMinage(gm)
maxage <- Rgadget:::getMaxage(gm)
maxlength <- 180 #max(Rgadget:::getLengthgroups(gm))

## Query length data to create IGFS catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", seq(0, maxlength, by = 2))),
                                                        defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("lingimm", "lingmat")))
rm(aggdata)
## Age IGFS
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'IGFS',
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, maxlength, by = 4))),
                        defaults))

#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("lingimm", "lingmat")))
rm(aggdata)

## Maturity @3 from IGFS
aggdata <- mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                             append(defaults,
                                    list(sampling_type='IGFS',
                                         age=mfdb_group(age3=3:20),
                                         length = mfdb_step_interval('len', by = 2, to = maxlength),              
                                         maturity_stage = mfdb_group(lingimm = 1, lingmat = 2:5))))

gadget_dir_write(gd,
                 gadget_likelihood_component("stockdistribution",
                                             name = "matp.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("lingimm", "lingmat")))





## Query length data to create lln catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    gear = c('LLN','HLN'),
    length = mfdb_interval("len", seq(0, maxlength, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.lln",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("lln"),
                                                 stocknames = c("lingimm", "lingmat")))
rm(aggdata)
## Age lln
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
                             gear = c('LLN','HLN'),
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, maxlength, by = 4))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.lln",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("lln"),
                                             stocknames = c("lingimm", "lingmat")))
rm(aggdata)



## Query length data to create bmt catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    species = 'LIN',
    gear = c('BMT', 'NPT'),
    length = mfdb_interval("len", seq(0, maxlength, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.bmt",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("bmt"),
                                                 stocknames = c("lingimm", "lingmat")))
rm(aggdata)
## Age bmt
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
                             gear = c('BMT','NPT'),
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, maxlength, by = 4))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.bmt",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("bmt"),
                                             stocknames = c("lingimm", "lingmat")))
rm(aggdata)

## Query length data to create gil catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    species = 'LIN',
    gear = c('GIL'),
    length = mfdb_interval("len", seq(0, maxlength, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.gil",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("gil"),
                                                 stocknames = c("lingimm", "lingmat")))
rm(aggdata)
## Age gil
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
                             gear = c('GIL'),
                             age = mfdb_step_interval('age',by=1,from=1,to=12),
                             length = mfdb_interval("len", seq(0, maxlength, by = 4))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.gil",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("gil"),
                                             stocknames = c("lingimm", "lingmat")))
rm(aggdata)

