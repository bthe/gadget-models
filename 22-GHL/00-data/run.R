library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind.2050=c('si.M.20-50','si.F.20-50'),
                            sind.5070=c('si.M.50-70','si.F.50-70'),
                            sind.70120=c('si.M.70-120','si.F.70-120'),
                          survey=c('ldist.f.survey','ldist.m.survey'),
                          comm=c('ldist.comm','aldist.m.faer',
                              'aldist.f.faer','comm.sex.ratio')),
                        wgts='WGTS')


