library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si.10-40','si.40-70','si.70-110'),
                          survey=c('ldist.igfs','aldist.igfs'),
                          longline=c('ldist.lln','aldist.lln'),
                          comm=c('ldist.gil','ldist.bmt',
                            'aldist.bmt')),
                        wgts='WGTS')


