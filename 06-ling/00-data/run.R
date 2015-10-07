library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si2049','si5069','si70180'),
                          survey=c('ldist.igfs','alkeys.igfs'),
                          longline=c('ldist.lln','alkeys.lln'),
                          comm=c('ldist.gil','ldist.bmt',
                            'alkeys.gil','alkeys.bmt'))
                        wgts='WGTS')


