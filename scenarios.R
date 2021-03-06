  library(Rgadget)
  library(doMC)
  registerDoMC(cores = detectCores(logical = TRUE))
  
  runner <- function(id=1,bmt=1,lln=1,gil=1,folder='01-cod/02-mfdbcod'){
    print(folder)
    curr.dir <- getwd()
    setwd(folder)
    print(getwd())#
    load('WGTS/WGTS.Rdata')
    #fit <- gadget.fit()
    fit <- out
    ## predict into the future
    pre.fleet <- filter(fit$fleet.info,year==2014) %>% 
      select(fleet, ratio = harv.rate) %>%
      mutate(ratio = ifelse(fleet=='bmt',bmt*ratio,
                            ifelse(fleet=='lln',lln*ratio,
                                   gil*ratio)))
    
    if(grepl('ling',fit$res.by.year$stock[1])){
      rec.window <- c(1980,2000)
    } else {
      rec.window <- c(1980,2010)
    }
    
    progn <- gadget.forward(params.file='WGTS/params.final',
                            effort=seq(0.5,1.5,by = 0.05),
                            fleets=pre.fleet,mat.par=c(0,0),
                            rec.window = rec.window,
                            years = 100,
                            num.trials = 10,
                            gd = list(dir = ".", 
                                      rel.dir = sprintf("PRE-%s",id)))
    
    progn.ssb <- filter(progn$lw,grepl('mat',stock))
    res <-
      left_join(progn$catch %>%
                  group_by(year,trial,effort) %>%
                  summarise(catch=sum(biomass.consumed)),
                progn.ssb) %>%
      filter(year > 2050) %>%
      ungroup() %>%
      group_by(stock,effort,trial) %>%
      summarise(catch=mean(catch),
                total.bio = mean(total.bio)) %>%
      ungroup() %>% 
      group_by(stock,effort) %>%
      summarise(catch.m=mean(catch),
                catch.u=quantile(catch,0.975),
                catch.l=quantile(catch,0.025),
                ssb.m=mean(total.bio),
                ssb.u=quantile(total.bio,0.975),
                ssb.l=quantile(total.bio,0.025)) %>%
      mutate(bmt=bmt,lln=lln,gil=gil)
    setwd(curr.dir)
    return(res)  
  }
  
  dirs <- c('01-cod/02-mfdbcod','02-haddock/02-mfdbhad','03-saithe/02-mfdbpok',
            '06-ling/02-mfdbling','08-tusk/02-mfdbtusk','09-wolf/02-mfdbwolf')
  
  lln.inc <- c(1,1.1,1.2,1.3)
  
  run.dat <- data.frame(run.id = 1:(length(dirs)*length(lln.inc)), 
                        dirs = rep(dirs,length(lln.inc)),
                        lln = rep(lln.inc,each=length(dirs)),
                        bmt = rep(2-lln.inc,each=length(dirs)),
                        gil = 1,
                        stringsAsFactors = FALSE)

res <- ddply(run.dat,~run.id,
             function(x) runner(id=x$run.id[1],
                                bmt=x$bmt[1], lln = x$lln[1],
                                gil=x$gil[1], folder=x$dirs[1]),
             .parallel = TRUE)

save(res,file='res.RData')



