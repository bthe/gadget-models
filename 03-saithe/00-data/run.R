library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(gp1=c('si.gp1','si.gp2','si.gp3')),
                        wgts='WGTS')


