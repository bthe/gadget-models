## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(4,17))),
    defaults))

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(17,33))),
    defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(33,140))),
    defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp1",
                                                 weight = 1,
                                                 data = igfs.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp2",
                                                 weight = 1,
                                                 data = igfs.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp3",
                                                 weight = 1,
                                                 data = igfs.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))
## AUT survey indices

aut.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(16,27))),
    defaults))

aut.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(27,39))),
    defaults))

aut.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(39,140))),
    defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp1a",
                                                 weight = 1,
                                                 data = aut.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp2a",
                                                 weight = 1,
                                                 data = aut.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.gp3a",
                                                 weight = 1,
                                                 data = aut.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("codimm","codmat")))
