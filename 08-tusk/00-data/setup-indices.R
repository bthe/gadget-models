## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(10,20,30,40))),
    defaults))

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(40,50,60,70))),
    defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(70,80,90,110))),
    defaults))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.10-40",
                                                 weight = 1,
                                                 data = igfs.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("tuskimm","tuskmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.40-70",
                                                 weight = 1,
                                                 data = igfs.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("tuskimm","tuskmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.70-110",
                                                 weight = 1,
                                                 data = igfs.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("tuskimm","tuskmat")))
