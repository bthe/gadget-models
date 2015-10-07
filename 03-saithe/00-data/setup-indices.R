## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(4,21))),
    defaults))

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(21,33))),
    defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(33,50))),
    defaults))

igfs.SI4 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(50,140))),
    defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp1",
                                                 weight = 1,
                                                 data = igfs.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("hadimm")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp2",
                                                 weight = 1,
                                                 data = igfs.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("hadimm","hadmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp3",
                                                 weight = 1,
                                                 data = igfs.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("hadimm","hadmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp4",
                                                 weight = 1,
                                                 data = igfs.SI4[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("hadimm","hadmat")))

## AUT survey indices

aut.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(4,18))),
    defaults))

aut.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(18,30))),
    defaults))

aut.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(30,140))),
    defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp1a",
                                                 weight = 1,
                                                 data = aut.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("hadimm")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp2a",
                                                 weight = 1,
                                                 data = aut.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("hadimm","hadmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp3a",
                                                 weight = 1,
                                                 data = aut.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("hadimm","hadmat")))
