## AUT and GRE survey indices

fix.year <- function(x){
  x %>%
    group_by(length) %>% 
    mutate(number = ifelse(year<1997,
                                number*number[year==1997&length==length]/number[year==1996&length==length],
                                ifelse(year %in% c(2001,2011), (lead(number)+lag(number))/2,
                                       number)))
               
}
comb.F.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = c('AUT','GRE'),
    sex = 'F',
    length = mfdb_interval("len", c(10,50))),
    defaults))

comb.M.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = c('AUT','GRE'),
  timestep = mfdb_timestep_yearly,
  sex = 'M',
  length = mfdb_interval("len", c(10,50))),
  defaults))

comb.F.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'F',
  length = mfdb_interval("len", c(50,60,70))),
  defaults))

comb.M.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'M',
  length = mfdb_interval("len", c(50,60,70))),
  defaults))

comb.F.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'F',
  length = mfdb_interval("len", c(70,80,90,100,120))),
  defaults))

comb.M.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = c('AUT','GRE'),
  sex = 'M',
  length = mfdb_interval("len", c(70,80,90,100,120))),
  defaults))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.M.20-50",
                                                 weight = 1,
                                                 data = fix.year(comb.M.SI1[[1]]),
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlmale'))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.M.50-70",
                                                 weight = 1,
                                                 data = fix.year(comb.M.SI2[[1]]),
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlmale'))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.M.70-120",
                                                 weight = 1,
                                                 data = fix.year(comb.M.SI3[[1]]),
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlmale'))



gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.F.20-50",
                                                 weight = 1,
                                                 data = fix.year(comb.M.SI1[[1]]),
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlfemale'))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.F.50-70",
                                                 weight = 1,
                                                 data = fix.year(comb.M.SI2[[1]]),
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlfemale'))


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.F.70-120",
                                                 weight = 1,
                                                 data = fix.year(comb.M.SI3[[1]]),
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = 'ghlfemale'))
