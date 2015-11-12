library(Rgadget)
library(purrr)
load('res.RData')

dirs <- c('01-cod/02-mfdbcod','02-haddock/02-mfdbhad','03-saithe/02-mfdbpok',
          '06-ling/02-mfdbling','08-tusk/02-mfdbtusk','09-wolf/02-mfdbwolf')


fit <- dirs %>% map(function(x){
  load(sprintf('%s/WGTS/WGTS.Rdata',x))
  out$fleet.info$species <- gsub('([0-9]+)-','',gsub('/([0-9]+)-([a-z]+)','',x))
  return(out)
  }) %>%
  zip_n() %>% map(function(x) x %>% map(function(y) y %>% map_if(is.factor, as.character))) %>%
  map(function(x) try(dplyr::bind_rows(x)))
  

blim <- 
  fit$res.by.year %>% filter(grepl('mat',stock)) %>% 
  mutate(species = gsub('mat','',stock)) %>%
  group_by(species) %>% 
  summarise(blim=min(total.biomass)/1e6)

Fmsy <- 
  res %>% 
  group_by(stock) %>%
  filter(catch.m==max(catch.m))

class(fit) <- c("gadget.fit", "list")

## fit to biomass indices
fit$stock.full %>% filter(year==1960) %>% 
  mutate(species = gsub('imm|mat','',.id)) %>%
  select(species,lower=length,weight=mean.weight) %>%
  distinct() %>%
  right_join(fit$sidat %>%
               mutate(species = gsub('imm|mat','',
                                     gsub('\t([a-z]*)','',stocknames)),
                      lower = ifelse(lower==min(lower,na.rm=TRUE),lower+6,lower),
                      lower = ifelse(lower %% 2 == 1,lower+1,lower))) %>%
  filter(step==1) %>%
  group_by(year,species) %>%
  summarise(obs=sum(number.x*weight,na.rm=TRUE),
            prd=sum(predict*weight,na.rm=TRUE)) %>%
  ggplot(aes(year,obs)) + geom_point() + geom_line(aes(year,prd)) + 
  facet_wrap(~species,scale='free_y') + theme_bw() + 
  xlab('Year') + ylab('Spring survey index')

## trends in biomass
plot(fit,data='res.by.year',type='total')

## trends in abundance
plot(fit,data='res.by.year',type='num.total')

## trends in landings
plot(fit,data='res.by.year',type='catch')

## fishing mortality
plot(fit,data='res.by.year',type='F') 

## fishing mortality
plot(fit,data='res.by.year',type='rec') 

## Harvest rate
fit$fleet.info %>%
  filter(fleet %in% c('bmt','gil','lln'), year >1990) %>%
  #  group_by(species,fleet) %>%
  #  mutate(norm.rate = harv.rate/max(harv.rate)) %>%
  ggplot(aes(year,harv.rate,col=species)) + geom_line() + facet_wrap(~fleet,ncol=1,scale='free_y') + 
  theme_bw() + xlab('Year') + ylab('Catch/Harv. biomass')

## Num overfished stocks
Fmsy %>%
  ungroup() %>%
  summarise(num.overfished = sum(as.character(effort) < 1),
            num.total = length(effort))

## Large fish indicator
fit$stock.full %>% 
  filter(year>1980) %>%
  group_by(year) %>% 
  summarise(total.bio = sum(number*mean.weight),
            lfi = sum((length>60) * number * mean.weight)/total.bio) %>%
  ggplot(aes(year,lfi)) + geom_line() + 
  ylab('LFI') + xlab('Year') + theme_bw()

## shannon index 
fit$res.by.year %>% 
  group_by(year) %>%
  mutate(p = catch/sum(catch)) %>% 
  summarise(shannon = -1*sum(p*log(p)),
            eveness = shannon/log(length(catch))) %>%
  ggplot(aes(year,shannon)) + geom_line() + theme_bw() + 
  ylab('Shannon index') + xlab('Year')

## Mean maximum length
fit$stock.growth %>%
  group_by(stock) %>% 
  summarise(Linf = max(length)) %>%
  right_join(fit$res.by.year) %>%
  ungroup() %>%
  filter(year>1980) %>%
  group_by(year) %>%
  summarise(mml=sum(total.biomass*Linf)/sum(total.biomass)) %>%
  ggplot(aes(year,mml)) + geom_line() + theme_bw() + 
  xlab('Year') + ylab('Mean Maximum Length')

## Revenue by year (using mean fish price from 2014-2015)
price <- data.frame(species = c('cod','had','pok','lin','tusk','wolf'),
                    price = c(275,305,187,189.7,123,156))

fit$res.by.year %>%
  mutate(species = gsub('mat|imm','',stock)) %>%
  left_join(price) %>%
  mutate(revenue = catch*price) %>%
  ggplot(aes(year,revenue/1e9,fill=species)) + geom_bar(stat='identity') + 
  xlab('Year') + ylab('Revenue (in Billion ISK)') + theme_bw()

  