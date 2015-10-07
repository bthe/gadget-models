

catage <-
    mfdb_sample_count(mdb,c('age','length'),
                      c(list(age = mfdb_step_interval('age',by=1,from=1,to=12),
                             sampling_type = 'CAA',
                             area = mfdb_group("1" =
                                 unique(reitmapping$SUBDIVISION)),
                             timestep = mfdb_timestep_quarterly,
                             year = 1960:1980,
                             species = 'COD'
                             )))
attributes(catage$'0.0.0.0.0')$length$all <-
    c(Rgadget:::getMinlength(gm),Rgadget:::getMaxlength(gm))
catw <-
    mfdb_sample_meanweight(mdb,c('age'),
                           c(list(age = mfdb_step_interval('age',by=1,from=1,to=12),
                                  sampling_type = 'CAA',
                                  area = mfdb_group("1" =
                                      unique(reitmapping$SUBDIVISION)),
                                  timestep = mfdb_timestep_quarterly,
                                  year = 1960:1980,
                                  species = 'COD'
                                  )))


gadget_dir_write(gd,
                 gadget_likelihood_component('catchdistribution',
                                             name = 'cod.catage',
                                             weight = 1,
                                             data = catage[[1]],
                                             fleetnames = 'oldfleet',
                                             stocknames = c('codimm','codmat')))

gadget_dir_write(gd,
                 gadget_likelihood_component('catchstatistics',
                                             name = 'cod.catw',
                                             weight = 1,
                                             data_function = 'weightnostddev',
                                             data = catw[[1]],
                                             fleetnames = 'oldfleet',
                                             stocknames = c('codimm','codmat')))
