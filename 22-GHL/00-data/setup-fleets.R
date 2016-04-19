## Collect catches by fleet:

ghl.landings <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    #gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
    sampling_type = 'LND',
    species = defaults$species),
                                                        defaults))


foreign.landings <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(
                          sampling_type = 'FLND',
                          species = defaults$species),
                        defaults))


ghl.fleet <-
    Rgadget:::make.gadget.fleet(name='ghl',suitability='exponentiall50',
                                fleet.data=bmt.landings[[1]],
                                stocknames=stock.names)


## nominal survey fleet catches

aut.landings <- data.frame(year=defaults$year,step=4,number=1,area=1)
aut.fleet <-
    Rgadget:::make.gadget.fleet(name='aut',suitability='exponentiall50',
                                fleet.data=aut.landings,
                                stocknames=stock.names)

## foreign fleet
foreign.fleet <- 
    Rgadget:::make.gadget.fleet(name='foreign',suitability='exponentiall50',
                                fleet.data=foreign.landings[[1]],
                                stocknames=stock.names)

## foreign fishing vessels use the same type of gear
foreign.fleet@suitability <-  ghl.fleet@suitability
