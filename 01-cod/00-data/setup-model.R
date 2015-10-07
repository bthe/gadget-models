library(Rgadget)
## find sane starting values for recl and stddev
mla <- mfdb_sample_meanlength_stddev(mdb,c('age'),
                                     c(list(sampling_type='IGFS',age=1:12),defaults))
init.sigma <- mla[[1]] %>% group_by(age) %>%
    summarise(ml=mean(mean), ms = mean(stddev,na.rm=TRUE))
## populate the model with sane defaults
opt <- gadget.options(type='simple2stock')

## adapt to Cod
weight.alpha <- 6.5e-6
weight.beta <- 3.07

opt$area$numofareas <- 1
opt$time$firstyear <- 1960
opt$time$lastyear <- 2015

## immature stock
opt$stocks$imm <-
  within(opt$stocks$imm,{
    name <- 'codimm'
    minage <- 1
    maxage <- 10
    minlength <- 4
    maxlength <- 130
    dl <- 2
    growth <- c(linf='#Linf',k='( * 0.001 #k)',
                beta='(* 10 #bbin)', binn=15,recl='#recl')
    weight <- c(a=weight.alpha, b=weight.beta)
    init.abund <- sprintf('(* %s %s)',c(0,0.1,0.1,0.08,0.06,0.04,0.02,0.01,0,0),
                          c(0,sprintf('#age%s',2:8),0,0))
    n <- sprintf('(* 1000 #rec%s)',1960:2015)
    doesmature <- 1
    maturityfunction <- 'continuous'
    maturestocksandratios <- 'codmat 1'
    maturity.coefficients <- '( * 0.001 #mat1) #mat2 0 0'
    sigma <- init.sigma$ms
      #list(alpha='( * 0.001 #mat1)',
                            #      l50='#mat2', beta=0,
                            #      a50=0)
    M <- c(0.5,  0.35, 0.2, 0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.3)
    maturitysteps <- '0'
    doesmove <- 1
    transitionstep <- 4
    transitionstocksandratios <- "codmat 1"
    doesmigrate <- 0
    doesrenew <- 1
    renewal <- list(minlength=4, maxlength=28)
  })


## mature stock
opt$stocks$mat <-
  within(opt$stocks$mat,{
    name <- 'codmat'
    minage <- 3
    maxage <- 12
    minlength <- 20
    maxlength <- 140
    dl <- 2
    M <- c(0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.5, 0.7)
    growth <- c(linf='#Linf',k='( * 0.001 #k)',
                beta='(* 10 #bbin)', binn=15,recl='#recl')
    weight <- c(a=3.5e-6, b=3.22)
    init.abund <- sprintf('(* %s %s)',c(0,0.02,0.04,0.06,0.08,0.09,0.01,0.001,0.0001,0),
                          c(0,sprintf('#age%s',4:11),0))
    sigma <- init.sigma$ms
    doesmature <- 0
    doesmigrate <- 0
  })


## create the gadget skeleton
gm <- gadget.skeleton(time=opt$time, area=opt$area,
                      stocks=opt$stocks,opt$fleets)

gm@stocks$imm@initialdata$area.factor <- '( * 100 #mult)'
gm@stocks$mat@initialdata$area.factor <- '( * 100 #mult)'

#gm@stocks$mat@doesrenew <- 1
#gm@stocks$mat@renewal


gm@fleets <- list(old.fleet,igfs.fleet,aut.fleet,lln.fleet,gil.fleet,bmt.fleet,foreign.fleet)
gd.list <- list(dir=gd$dir)
Rgadget:::gadget_dir_write(gd.list,gm)

curr.dir <- getwd()
setwd(gd$dir)
callGadget(s=1,ignore.stderr = FALSE)

init.params <- read.gadget.parameters('params.out')

init.params[c('Linf','k','bbin','mult',
              grep('age',rownames(init.params),value=TRUE),
              'mat1','mat2'),] <- 
  read.table(text='switch	value		lower	upper	optimise
Linf	         160	      80     2000        0
k	          90	       1      200        1
bbin	         6	   1e-08    100        1
mult	         100	     0.1      100        1
age2	         35	    0.01     150        1
age3	         25	    0.01     120        1
age4	         15	   0.001     100        1
age5	          7	  0.0001     100        1
age6	          7	   1e-05     100        1
age7	          5	   1e-08     100        1
age8	          5	   1e-10     100        1
age9	         25	   1e-12     100        1
age10	         10	   1e-15     100        1
age11	         10	   1e-15    1e+05        1
mat1	          70	      10      200        1
mat2	          70	      30      100        1',header=TRUE)

init.params$switch <- rownames(init.params)

init.params[grepl('rec[0-9]',init.params$switch),'value'] <- 30
init.params[grepl('rec[0-9]',init.params$switch),'upper'] <- 200
init.params[grepl('rec[0-9]',init.params$switch),'lower'] <- 0.01
init.params[grepl('rec[0-9]',init.params$switch),'optimise'] <- 1

init.params['recl',-1] <- c(12, 4, 20,1)

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
