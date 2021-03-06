## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(20,50))),
  defaults))

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(50,60,70))),
  defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(70,80,90,100,140))),
  defaults))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp1",
                                                 weight = 1,
                                                 data = igfs.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("pokimm","pokmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp2",
                                                 weight = 1,
                                                 data = igfs.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("pokimm","pokmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp3",
                                                 weight = 1,
                                                 data = igfs.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("pokimm","pokmat")))




