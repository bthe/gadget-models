library(Rgadget)
library(gridExtra)

fleet.predict <- data.frame(fleet=c('lln','bmt','gil'),
                            ratio = c(0.669,0.30,0.031))


fit <- gadget.fit(mat.par = c(-7.1854,0.0945),
                  fleet.predict=fleet.predict)

plotter <- function(obj,file='foo.jpg',height=3000,width=6000){

    postscript("tmp.eps")

    
    print(obj)
    
    dev.off()


    system(paste("convert -verbose -density 300 tmp.eps -quality 92 -rotate 90 tmp.jpg", sep=""))
    system(paste("cp tmp.jpg ",file ,sep=""))

}


## igfs
plotter(plot(fit),file='sifit.jpg')

## summary
plotter(plot(fit,data='summary'),file='summary.jpg')

## ldists
tmp <- plot(fit, data='catchdist.fleets')

plotter(tmp$ldist.igfs,file='ldistigfs.jpg')
plotter(tmp$alkeys.igfs,file='aldistigfs.jpg')

plotter(tmp$ldist.lln,file='ldistlln.jpg')
plotter(tmp$alkeys.lln,file='aldistlln.jpg')

plotter(tmp$ldist.bmt+ ylim(c(0,0.05)),file='ldistbmt.jpg')
plotter(tmp$alkeys.bmt,file='aldistbmt.jpg')

plotter(tmp$ldist.gil+ ylim(c(0,0.05)),file='ldistgil.jpg')
plotter(tmp$alkeys.gil,file='aldistgil.jpg')

## ices std. plots

postscript("tmp.eps")

## hack res.by.year into the correct format
fit$res.by.year <- 
  fit$res.by.year %>% 
  group_by(year) %>% 
  summarise(stock='Ling',area=1,catch=sum(catch),num.catch = sum(num.catch), F=max(F),
            total.number = sum(total.number),total.biomass=sum(total.biomass),ssb = sum(ssb),
            recruitment = exp(-0.15)*max(recruitment,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(recruitment = lag(recruitment))

grid.arrange(plot(fit,data='res.by.year',type='ssb')+
             theme(legend.position='none') +
             xlim(c(1982,2015))+
             ylim(c(0,62)),
             plot(fit,data='res.by.year',type='F')+
             theme(legend.position='none')+
             xlim(c(1982,2015)) +
             ylim(c(0,0.9)) +
             geom_hline(yintercept=c(0.2,0.24,0.28),lty=c(2,1,2)),
             plot(fit,data='res.by.year',type='rec')+
             theme(legend.position='none')+
             xlim(c(1982,2015)) +
             ylim(c(0,27)),
             plot(fit,data='res.by.year',type='catch')+
             theme(legend.position='none')+
             xlim(c(1982,2015))+ ylim(c(0,15)))
dev.off()


system(paste("convert -verbose -density 300 tmp.eps -quality 92 -rotate 90 tmp.jpg", sep=""))
system(paste("cp tmp.jpg ",'icesplot.jpg' ,sep=""))


## retro
load('~/Dropbox/gadget-models/06-ling/01b-DEEPhack/WGTS/WGTS.Rdata')
old.fit <- out

load('~/Dropbox/gadget-models/06-ling/01-initial/WGTS/WGTS.Rdata')
older.fit <- out

old.fit$res.by.year$stock <- 'Ling 2015'
older.fit$res.by.year$stock <- 'Ling 2014'


old.fit$res.by.year <- rbind.fill(older.fit$res.by.year,old.fit$res.by.year,fit$res.by.year)

postscript("tmp.eps")

grid.arrange(plot(old.fit,data='res.by.year',type='ssb')+
             theme(legend.position='none') +
             xlim(c(1982,2015))+
             ylim(c(0,80)),
             plot(old.fit,data='res.by.year',type='F')+
             theme(legend.position='none')+
             xlim(c(1982,2015)) +
             ylim(c(0,0.9)) +
             geom_hline(yintercept=c(0.2,0.24,0.28),lty=c(2,1,2)),
#             geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=0.20,ymax=0.28),
#                       col='white',fill='green',alpha=0.01,size=0.01),
             plot(old.fit,data='res.by.year',type='rec')+
             theme(legend.position='none')+
             xlim(c(1982,2015)) +
             ylim(c(0,28)), ncol=2)
dev.off()


system(paste("convert -verbose -density 300 tmp.eps -quality 92 -rotate 90 tmp.jpg", sep=""))
system(paste("cp tmp.jpg ",'retroplot.jpg' ,sep=""))


## res.by.year table

write.table(fit$res.by.year[c('year','total.biomass','ssb',
                              'F','catch','recruitment')],
            file='lingResByYear.txt',sep='\t',col.names=TRUE,
            quote=FALSE,row.names=FALSE)

## prognosis

progn <- gadget.forward(years=5,params.file='WGTS/params.final',
                        stochastic=FALSE,rec.window=2003,
                        num.trials=2,
                        effort=c(0,0.13,0.24,0.28),
                        fleets=fleet.predict)

#Rgadget:::plot.gadget.forward(progn,type='catch')

progn.catch <- ddply(subset(progn$catch,year %in% 2016:2020 & trial == 1,
                            select=c(year,effort,biomass.consumed)),
                     ~year+effort,summarise,catch=sum(biomass.consumed)/1e6)

progn.ssb <- ddply(subset(progn$lw,year %in% 2014:2019 & trial == 1,
                            select=c(year,effort,ssb)),
                     ~year+effort,summarise,ssb=sum(ssb)/1e6)

tmp <-
    dcast(mutate(melt(merge(progn.catch,progn.ssb),id.vars=c('year','effort')),
                 tab.var = paste0(variable,effort),
                 effort=NULL,
                 variable=NULL),year~tab.var)

tmp[c('year','ssb0','catch0.13','ssb0.13','catch0.24','ssb0.24','catch0.28','ssb0.28')]
