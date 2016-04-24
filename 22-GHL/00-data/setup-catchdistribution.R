minage <- Rgadget:::getMinage(gm)
maxage <- Rgadget:::getMaxage(gm)
maxlength <- Rgadget:::getMaxlength(gm)
minlength <- Rgadget:::getMinlength(gm)

## Query length data to create IGFS catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
  sampling_type = 'AUT',
  sex = 'M',
  length = mfdb_interval("len", seq(minlength, maxlength, by = 2))),
  defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.m.survey",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = "aut",
                                             stocknames = 'ghlmale'))
rm(aggdata)


aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
  sampling_type = 'AUT',
  sex = 'F',
  length = mfdb_interval("len", seq(minlength, maxlength, by = 2))),
  defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.f.survey",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = "aut",
                                             stocknames = 'ghlfemale'))
rm(aggdata)

## Maturity @3 from IGFS
aggdata <- mfdb_sample_count(mdb, c('sex','age','length'),
                             append(defaults,
                                    list(sampling_type='SEA',
                                         sex=mfdb_group(ghlmale='M',ghlfemale='F'),
                                         length = mfdb_step_interval('len', by = 2, to = maxlength))))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("stockdistribution",
                                             name = "comm.sex.ratio",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = "comm",
                                             stocknames = c("ghlmale", "ghlfemale")))





## Query length data to create commercial catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = c('SEA','FAER'),
    length = mfdb_interval("len", seq(minlength, maxlength, by = 2))),
                                                        defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.comm",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = 'comm',
                                                 stocknames = c("ghlmale", "ghlfemale")))
rm(aggdata)
## Age from the faeroes
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'FAER',
                             sex = 'F',
                             age = mfdb_step_interval('age',by=1,from=1,to=15),
                             length = mfdb_interval("len", seq(minlength, maxlength, by = 4))),
                        defaults))


gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.f.faer",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = 'comm',
                                             stocknames = c("ghlfemale")))
rm(aggdata)


aggdata <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'FAER',
                           sex = 'M',
                           age = mfdb_step_interval('age',by=1,from=1,to=15),
                           length = mfdb_interval("len", seq(minlength, maxlength, by = 4))),
                      defaults))


gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.m.faer",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = 'comm',
                                             stocknames = c("ghlmale")))
rm(aggdata)
