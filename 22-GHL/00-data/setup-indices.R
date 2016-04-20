## AUT and GRE survey indices

fix.year <- function(x){
  x %>%
    
    mutate(number = ifelse(year<1997,
                                number*number[year==1997&length==length]/number[year==1996&length==length],
                                ifelse(year %in% c(2001,2011), (lead(number)+lag(number))/2,
                                       number)))
               
}

surv.defaults <- defaults
surv.defaults$year <- year_range[year_range>1996 & !(year_range %in% c(2001,2011)) ]

comb.F.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = c('AUT','GRE'),
    sex = 'F',
    length = mfdb_interval("len", c(10,50))),
    surv.defaults))

comb.M.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'M',
  length = mfdb_interval("len", c(10,50))),
  surv.defaults))

comb.F.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'F',
  length = mfdb_interval("len", c(50,60,70))),
  surv.defaults))

comb.M.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'M',
  length = mfdb_interval("len", c(50,60,70))),
  surv.defaults))

comb.F.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'F',
  length = mfdb_interval("len", c(70,80,120))),
  surv.defaults))

comb.M.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'M',
  length = mfdb_interval("len", c(70,120))),
  surv.defaults))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.M.20-50",
                                                 weight = 1,
                                                 data = comb.M.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlmale'))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.M.50-70",
                                                 weight = 1,
                                                 data = comb.M.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlmale'))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.M.70-120",
                                                 weight = 1,
                                                 data = comb.M.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlmale'))



gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.F.20-50",
                                                 weight = 1,
                                                 data = comb.F.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlfemale'))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.F.50-70",
                                                 weight = 1,
                                                 data = comb.F.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlfemale'))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.F.70-120",
                                                 weight = 1,
                                                 data = comb.F.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlfemale'))
