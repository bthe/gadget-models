library(Rgadget)
library(broom)
## find sane starting values for recl and stddev
mla <- mfdb_sample_meanlength_stddev(mdb,c('age','sex'),
                                     c(list(sampling_type='FAER',age=1:20,sex=c('M','F')),
                                       defaults))
init.sigma <- 
  mla[[1]] %>% 
  dplyr::group_by(age,sex) %>%
  dplyr::summarise(ml=mean(mean), ms = mean(stddev,na.rm=TRUE))

init.male <- filter(init.sigma,sex == 'M')
init.female <- filter(init.sigma,sex == 'F')

lw <- mfdb_sample_meanweight(mdb,c('length','sex'),
                             c(list(sampling_type='AUT', species = defaults$species,
                                    sex = c('M','F'),
                                    length=mfdb_interval("", seq(0, 150, by = 1)))))

lw.tmp <- 
  lw[[1]] %>%
  dplyr::mutate(length=as.numeric(as.character(length)), 
                weight=mean/1e3) %>% 
  dplyr::filter(length<90) %>% 
  na.omit() %>% 
  nls(weight ~ a*length^b,.,start=list(a=1e-5,b=3)) %>% 
  tidy()


## populate the model with sane defaults
opt <- gadget.options(type='simple1stock')

## adapt to Ling
weight.alpha <-  as.numeric(lw.tmp$estimate[1]) #0.00000495
weight.beta <- as.numeric(lw.tmp$estimate[2]) #3.01793

opt$area$numofareas <- 1
opt$time$laststep <- 2
opt$time$firstyear <- min(year_range)
opt$time$lastyear <- max(year_range)



## immature stock
opt$stocks$imm <-
  within(opt$stocks$imm,{
    name <- 'ghlmale'
    minage <- 1
    maxage <- 25
    minlength <- 4
    maxlength <- 130
    dl <- 2
    growth <- c(linf='#ghlmale.Linf',k='( * 0.001 #ghlmale.k)',
                beta='(* 10 #ghlmale.bbin)', binn=15,recl='#ghlmale.recl')
    weight <- c(a=weight.alpha, b=weight.beta)
    init.abund <- sprintf('(* 0.01 %s %s)',c(0,rep(1000,9),rep(100,3),70,60,20,1,exp(-0.1*1:8)),
                          c(0,sprintf('#ghl.age%s',c(2:18,rep(18,7)))))
    n <- sprintf('(* #ghl.rec.mult #ghl.rec%s)',year_range)
    doesmature <- 0
    sigma <- c(rep(init.sigma$ms[2],5),tail(init.male$ms,9),rep(init.male$ms[10],15))
    M <- rep(0.1,25)
      doesmove <- 0
    doesmigrate <- 0
    doesrenew <- 1
    renewal <- list(minlength=4, maxlength=28)
  })

opt$stocks$fem <- 
  within(opt$stocks$imm,{
    name <- 'ghlfemale'
    growth <- c(linf='#ghlfemale.Linf',k='( * 0.001 #ghlfemale.k)',
                beta='(* 10 #ghlfemale.bbin)', binn=15,recl='#ghlfemale.recl')
    weight <- c(a=weight.alpha, b=weight.beta)
    init.abund <- sprintf('(* 0.01 %s %s)',c(0,rep(1000,9),rep(100,3),70,60,20,1,exp(-0.1*1:8)),
                          c(0,sprintf('(* #ghl.age%1$s (exp (* %1$s (* -1 #ghl.F))))',c(2:18,rep(18,7)))))
  })


## create the gadget skeleton
gm <- gadget.skeleton(time=opt$time, area=opt$area,
                      stocks=opt$stocks,opt$fleets)

gm@stocks$imm@initialdata$area.factor <- '( * 100 #ghl.mult)'
gm@stocks$fem@initialdata$area.factor <- '( * 100 #ghl.mult)'

gm@fleets <- list(aut.fleet,
                  comm.fleet,foreign.fleet)
gd.list <- list(dir=gd$dir)
Rgadget:::gadget_dir_write(gd.list,gm)

curr.dir <- getwd()
setwd(gd$dir)
callGadget(s=1,ignore.stderr = FALSE)

init.params <- read.gadget.parameters('params.out')

init.params[c('ghlmale.Linf','ghlmale.k','ghlmale.bbin','ghl.mult',
              'ghlfemale.Linf','ghlfemale.k','ghlfemale.bbin',
              'ghl.F','ghl.rec.mult',
              grep('age',rownames(init.params),value=TRUE)),] <- 
  read.table(text='switch	 value 		lower 	upper 	optimise
ghlmale.Linf	         75	      60     200        0
ghlmale.k	          90	       30      150        1
ghlmale.bbin	         6	   1e-08    100        1
ghl.mult	         100	     0.1      100        0
ghlfemale.Linf	         110	      80     200        0
ghlfemale.k	          90	       30      150        1
ghlfemale.bbin	         6	   1e-08    100        1
ghl.F	         0.1	     0.04      0.3        1
ghl.rec.mult     500      0.01     2000       0
ghl.age2	         35	    0.01     150        1
ghl.age3	         25	    0.01     120        1
ghl.age4	         15	   0.001     100        1
ghl.age5	          7	  0.0001     100        1
ghl.age6	          7	   1e-05     100        1
ghl.age7	          5	   1e-08     100        1
ghl.age8	          5	   1e-10     100        1
ghl.age9	         25	   1e-12     100        1
ghl.age10	         0.001	   1e-15     100        1
ghl.age11	         0.001	   1e-15     100        1
ghl.age12	         0.001	   1e-15     100        1
ghl.age13	         0.001	   1e-15     100        1
ghl.age14	         0.001	   1e-15     100        1
ghl.age15	         0.001	   1e-15     100        1
ghl.age16	         0.001	   1e-15     100        1
ghl.age17	         0.001	   1e-15     100        1
ghl.age18	         0.001	   1e-15     100        1',header=TRUE) 

init.params$switch <- rownames(init.params)

init.params[grepl('rec[0-9]',init.params$switch),'value'] <- 1
init.params[grepl('rec[0-9]',init.params$switch),'upper'] <- 4
init.params[grepl('rec[0-9]',init.params$switch),'lower'] <- 0.001
init.params[grepl('rec[0-9]',init.params$switch),'optimise'] <- 1

init.params['ghlmale.recl',-1] <- c(12, 4, 20,1)

init.params['ghlfemale.recl',-1] <- c(12, 4, 20,1)

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
