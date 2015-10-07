library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(gp1=c('si.gp1','si.gp1a'),
                          gp2=c('si.gp2','si.gp2a'),
                          gp3=c('si.gp3','si.gp3a')),
                        wgts='WGTS')


