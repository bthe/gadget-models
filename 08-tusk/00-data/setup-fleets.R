## Collect catches by fleet:
lln.landings <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    gear=c('HLN','LLN'),
    sampling_type = 'LND',
    species = defaults$species),
                                                        defaults))


bmt.landings <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    gear=c('BMT','NPT'),
    sampling_type = 'LND',
    species = defaults$species),
                                                        defaults))


gil.landings <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    gear='GIL',
    sampling_type = 'LND',
    species = defaults$species),
                                                        defaults))

foreign.landings <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(
                          sampling_type = 'FLND',
                          species = defaults$species),
                        defaults))


tmp <- defaults
tmp$year <- 1960:1982

old.landings <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(
                          sampling_type = 'OLND',
                          species = defaults$species),
                        tmp))


## make fleets
lln.fleet <-
    Rgadget:::make.gadget.fleet(name='lln',suitability='exponentiall50',
                                fleet.data=lln.landings[[1]],
                                stocknames=c('tuskimm','tuskmat'))

#Rgadget:::gadget_dir_write(gd,lln.fleet)


bmt.fleet <-
    Rgadget:::make.gadget.fleet(name='bmt',suitability='exponentiall50',
                                fleet.data=bmt.landings[[1]],
                                stocknames=c('tuskimm','tuskmat'))

#Rgadget:::gadget_dir_write(gd,bmt.fleet)

gil.fleet <-
    Rgadget:::make.gadget.fleet(name='gil',suitability='exponentiall50',
                                fleet.data=gil.landings[[1]],
                                stocknames=c('tuskimm','tuskmat'))

#Rgadget:::gadget_dir_write(gd,gil.fleet)


## nominal survey fleet catches

igfs.landings <- data.frame(year=defaults$year,step=1,number=1,area=1)
igfs.fleet <-
    Rgadget:::make.gadget.fleet(name='igfs',suitability='exponentiall50',
                                fleet.data=igfs.landings,
                                stocknames=c('tuskimm','tuskmat'))

#Rgadget:::gadget_dir_write(gd,igfs.fleet)

#aut.landings <- data.frame(year=defaults$year,step=4,number=1,area=1)
#aut.fleet <-
#    Rgadget:::make.gadget.fleet(name='aut',suitability='exponentiall50',
#                                fleet.data=aut.landings,
#                                stocknames=c('tuskimm','tuskmat'))

#Rgadget:::gadget_dir_write(gd,aut.fleet)

## old fleet
old.fleet <-
    Rgadget:::make.gadget.fleet(name='oldfleet',suitability='exponentiall50',
                                fleet.data=old.landings[[1]],
                                stocknames=c('tuskimm','tuskmat'))

#Rgadget:::gadget_dir_write(gd,old.fleet)


## foreign fleet
foreign.fleet <- 
    Rgadget:::make.gadget.fleet(name='foreign',suitability='exponentiall50',
                                fleet.data=foreign.landings[[1]],
                                stocknames=c('tuskimm','tuskmat'))

## foreign fishing vessels are longliners
foreign.fleet@suitability <-  lln.fleet@suitability
old.fleet@suitability <-  lln.fleet@suitability
#Rgadget:::gadget_dir_write(gd,foreign.fleet)
