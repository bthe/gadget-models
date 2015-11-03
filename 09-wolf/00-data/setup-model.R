library(Rgadget)
## find sane starting values for recl and stddev
mla <- mfdb_sample_meanlength_stddev(mdb,c('age'),
                                     c(list(sampling_type='IGFS',age=1:20),
                                       defaults))
init.sigma <- mla[[1]] %>% na.omit() %>% group_by(age) %>%
    summarise(ml=mean(mean), ms = mean(stddev,na.rm=TRUE))

## doesn't work currently, need to upload data
lw <- mfdb_sample_meanweight(mdb,c('length'),
                             c(list(sampling_type='IGFS', species = defaults$species,
                                    length=mfdb_interval("", seq(0, 150, by = 1)))))

lw.tmp <- 
  lw[[1]] %>%
   mutate(length=as.numeric(as.character(length)), 
          weight=mean/1e3) %>% 
  na.omit() %>% 
  nls(weight ~ a*length^b,.,start=list(a=1e-5,b=3)) %>%
  coefficients() %>%
  as.numeric()


## populate the model with sane defaults
opt <- gadget.options(type='simple2stock')

## adapt to wolf
weight.alpha <-  lw.tmp[1] #0.00000495
weight.beta <- lw.tmp[2] #3.01793

opt$area$numofareas <- 1
opt$time$firstyear <- 1960
opt$time$lastyear <- 2015

## immature stock
opt$stocks$imm <-
  within(opt$stocks$imm,{
    name <- 'wolfimm'
    minage <- 1
    maxage <- 10
    minlength <- 6
    maxlength <- 150
    dl <- 2
    growth <- c(linf='#wolf.Linf',k='( * 0.001 #wolf.k)',
                beta='(* 10 #wolf.bbin)', binn=15,recl='#wolf.recl')
    weight <- c(a=weight.alpha, b=weight.beta)
    init.abund <- sprintf('(* %s %s)',c(0,0.1,0.1,0.08,0.06,0.04,0.02,0.01,0,0),
                          c(0,sprintf('#wolf.age%s',2:7),0,0,0))
    n <- sprintf('(* 1000 #wolf.rec%s)',1960:2015)
    doesmature <- 1
    maturityfunction <- 'continuous'
    maturestocksandratios <- 'wolfmat 1'
    maturity.coefficients <- '( * 0.001 #wolf.mat1) #wolf.mat2 0 0'
    sigma <- c(init.sigma$ms[1],head(init.sigma$ms,14),rep(init.sigma$ms[14],5))
      #list(alpha='( * 0.001 #mat1)',
                            #      l50='#mat2', beta=0,
                            #      a50=0)

    ## using the same M-formulation as cod (is that a good idea, Muggur?)
    M <- rep(0.15,10) #c(0.5,  0.35, 0.2, 0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.3)
    
    maturitysteps <- '0'
    doesmove <- 1
    transitionstep <- 4
    transitionstocksandratios <- "wolfmat 1"
    doesmigrate <- 0
    doesrenew <- 1
    renewal <- list(minlength=4, maxlength=28)
  })


## mature stock
opt$stocks$mat <-
  within(opt$stocks$mat,{
    name <- 'wolfmat'
    minage <- 3
    maxage <- 20
    minlength <- 20
    maxlength <- 160
    dl <- 2
    M <- rep(0.15, 18) #c(0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.5, 0.7)
    growth <- c(linf='#wolf.Linf',k='( * 0.001 #wolf.k)',
                beta='(* 10 #wolf.bbin)', binn=15,recl='#wolf.recl')
    weight <- c(a=3.5e-6, b=3.22)
    init.abund <- sprintf('(* %s %s)',
                          c(0,0.02,0.04,0.06,0.08,0.09,0.01,0.001,0.0001,
                            0,0,0,0,0,0,0,0,0),
                          c(0,sprintf('#wolf.age%s',4:10),rep(0,10)))
    sigma <- c(init.sigma$ms[1],head(init.sigma$ms,14),rep(init.sigma$ms[14],5))
    doesmature <- 0
    doesmigrate <- 0
  })


## create the gadget skeleton
gm <- gadget.skeleton(time=opt$time, area=opt$area,
                      stocks=opt$stocks,opt$fleets)

gm@stocks$imm@initialdata$area.factor <- '( * 100 #wolf.mult)'
gm@stocks$mat@initialdata$area.factor <- '( * 100 #wolf.mult)'

gm@fleets <- list(old.fleet,igfs.fleet,#aut.fleet,
                  lln.fleet,gil.fleet,bmt.fleet,foreign.fleet)
gd.list <- list(dir=gd$dir)
Rgadget:::gadget_dir_write(gd.list,gm)

curr.dir <- getwd()
setwd(gd$dir)
callGadget(s=1,ignore.stderr = FALSE)

init.params <- read.gadget.parameters('params.out')

init.params[c('wolf.Linf','wolf.k','wolf.bbin','wolf.mult',
              grep('age',rownames(init.params),value=TRUE),
              'wolf.mat1','wolf.mat2'),] <- 
  read.table(text='switch	 value 		lower 	upper 	optimise
wolf.Linf	         160	      80     200        0
wolf.k	          90	       60      100        1
wolf.bbin	         6	   1e-08    100        1
wolf.mult	         100	     0.1      100        1
wolf.age2	         35	    0.01     150        1
wolf.age3	         25	    0.01     120        1
wolf.age4	         15	   0.001     100        1
wolf.age5	          7	  0.0001     100        1
wolf.age6	          7	   1e-05     100        1
wolf.age7	          5	   1e-08     100        1
wolf.age8	          5	   1e-10     100        1
wolf.age9	         25	   1e-12     100        1
wolf.age10	         10	   1e-15     100        1
wolf.mat1	          70	      10      200        1
wolf.mat2	          70	      30      100        1',header=TRUE) 

init.params$switch <- rownames(init.params)

init.params[grepl('rec[0-9]',init.params$switch),'value'] <- 1
init.params[grepl('rec[0-9]',init.params$switch),'upper'] <- 4
init.params[grepl('rec[0-9]',init.params$switch),'lower'] <- 0.001
init.params[grepl('rec[0-9]',init.params$switch),'optimise'] <- 1

init.params['wolf.recl',-1] <- c(12, 4, 20,1)

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
