library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si.20-50','si.50-70','si.70-180'),
                          survey=c('ldist.igfs','aldist.igfs'),
                          longline=c('ldist.lln','aldist.lln'),
                          comm=c('ldist.gil','ldist.bmt',
                            'aldist.gil','aldist.bmt')),
                        wgts='WGTS')


