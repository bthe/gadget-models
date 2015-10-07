library(Rgadget)
## find sane starting values for recl and stddev
mla <- mfdb_sample_meanlength_stddev(mdb,c('age'),
                                     c(list(sampling_type='IGFS',age=1:12),
                                       defaults))
init.sigma <- mla[[1]] %>% group_by(age) %>%
    summarise(ml=mean(mean), ms = mean(stddev,na.rm=TRUE))
## doesn't work currently, need to upload data
lw <- mfdb_sample_meanweight(mdb,c('length'),
                             c(list(sampling_type='IGFS',
                                    length=mfdb_interval("", seq(0, 150, by = 1)))))
if(FALSE){
    tmp <- stodvar %>% filter(synaflokkur == 30)
    had <- all.kv %>% filter(synis.id %in% tmp$synis.id, tegund == 2)
    lw <- had %>% group_by(ar,aldur) %>%
        summarise(ml = mean(lengd,na.rm=TRUE),
                  sl = sd(lengd,na.rm=TRUE))
    lw.m <- lw %>% filter(age<12) %>% group_by(aldur) %>%
        summarise(sl = mean(sl,na.rm=TRUE))
    
}

## populate the model with sane defaults
opt <- gadget.options(type='simple2stock')

## adapt to Haddock
weight.alpha <- 7.297e-6
weight.beta <- 3.064

opt$area$numofareas <- 1
opt$time$firstyear <- 1960
opt$time$lastyear <- 2015

## immature stock
opt$stocks$imm <-
  within(opt$stocks$imm,{
    name <- 'hadimm'
    minage <- 1
    maxage <- 10
    minlength <- 4
    maxlength <- 130
    dl <- 2
    growth <- c(linf='#had.Linf',k='( * 0.001 #had.k)',
                beta='(* 10 #had.bbin)', binn=15,recl='#had.recl')
    weight <- c(a=weight.alpha, b=weight.beta)
    init.abund <- sprintf('(* %s %s)',c(0,0.1,0.1,0.08,0.06,0.04,0.02,0.01,0,0),
                          c(0,sprintf('#had.age%s',2:8),0,0))
    n <- sprintf('(* 400 #had.rec%s)',1960:2015)
    doesmature <- 1
    maturityfunction <- 'continuous'
    maturestocksandratios <- 'hadmat 1'
    maturity.coefficients <- '( * 0.001 #had.mat1) #had.mat2 0 0'
    sigma <- init.sigma$ms
      #list(alpha='( * 0.001 #mat1)',
                            #      l50='#mat2', beta=0,
                            #      a50=0)

    ## using the same M-formulation as cod (is that a good idea, Muggur?)
    M <- c(0.5,  0.35, 0.2, 0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.3)
    
    maturitysteps <- '0'
    doesmove <- 1
    transitionstep <- 4
    transitionstocksandratios <- "hadmat 1"
    doesmigrate <- 0
    doesrenew <- 1
    renewal <- list(minlength=4, maxlength=28)
  })


## mature stock
opt$stocks$mat <-
  within(opt$stocks$mat,{
    name <- 'hadmat'
    minage <- 3
    maxage <- 12
    minlength <- 20
    maxlength <- 140
    dl <- 2
    M <- c(0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.5, 0.7)
    growth <- c(linf='#had.Linf',k='( * 0.001 #had.k)',
                beta='(* 10 #had.bbin)', binn=15,recl='#had.recl')
    weight <- c(a=3.5e-6, b=3.22)
    init.abund <- sprintf('(* %s %s)',
                          c(0,0.02,0.04,0.06,0.08,0.09,0.01,0.001,0.0001,0),
                          c(0,sprintf('#had.age%s',4:11),0))
    sigma <- init.sigma$ms
    doesmature <- 0
    doesmigrate <- 0
  })


## create the gadget skeleton
gm <- gadget.skeleton(time=opt$time, area=opt$area,
                      stocks=opt$stocks,opt$fleets)

gm@stocks$imm@initialdata$area.factor <- '( * 100 #had.mult)'
gm@stocks$mat@initialdata$area.factor <- '( * 100 #had.mult)'

gm@fleets <- list(old.fleet,igfs.fleet,aut.fleet,
                  lln.fleet,gil.fleet,bmt.fleet,foreign.fleet)
gd.list <- list(dir=gd$dir)
Rgadget:::gadget_dir_write(gd.list,gm)

curr.dir <- getwd()
setwd(gd$dir)
callGadget(s=1,ignore.stderr = FALSE)

init.params <- read.gadget.parameters('params.out')

init.params[c('had.Linf','had.k','had.bbin','had.mult',
              grep('age',rownames(init.params),value=TRUE),
              'had.mat1','had.mat2'),] <- 
  read.table(text='switch	value		lower	upper	optimise
had.Linf	         110	      80     2000        0
had.k	          90	       1      200        1
had.bbin	         6	   1e-08    100        1
had.mult	         100	     0.1      100        1
had.age2	         35	    0.01     150        1
had.age3	         25	    0.01     120        1
had.age4	         15	   0.001     100        1
had.age5	          7	  0.0001     100        1
had.age6	          7	   1e-05     100        1
had.age7	          5	   1e-08     100        1
had.age8	          5	   1e-10     100        1
had.age9	         25	   1e-12     100        1
had.age10	         10	   1e-15     100        1
had.age11	         10	   1e-15    1e+05        1
had.mat1	          70	      10      200        1
had.mat2	          70	      30      100        1',header=TRUE)

init.params$switch <- rownames(init.params)

init.params[grepl('rec[0-9]',init.params$switch),'value'] <- 30
init.params[grepl('rec[0-9]',init.params$switch),'upper'] <- 200
init.params[grepl('rec[0-9]',init.params$switch),'lower'] <- 0.01
init.params[grepl('rec[0-9]',init.params$switch),'optimise'] <- 1

init.params['had.recl',-1] <- c(12, 4, 20,1)

init.params[grepl('alpha',init.params$switch),'value'] <- 0.5
init.params[grepl('alpha',init.params$switch),'upper'] <- 3
init.params[grepl('alpha',init.params$switch),'lower'] <- 0.01
init.params[grepl('alpha',init.params$switch),'optimise'] <- 1

init.params[grepl('l50',init.params$switch),'value'] <- 50
init.params[grepl('l50',init.params$switch),'upper'] <- 100
init.params[grepl('l50',init.params$switch),'lower'] <- 10
init.params[grepl('l50',init.params$switch),'optimise'] <- 1

write.gadget.parameters(init.params,file='params.in')
setwd(curr.dir)
