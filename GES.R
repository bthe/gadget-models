library(Rgadget)
library(purrr)
library(gridExtra)
#library(wesanderson)
load('res.RData')

## extra plotting functions
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  tmp <- function(...) arrangeGrob(...,ncol=1)
  grid.arrange(
    do.call(tmp, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

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
  xlab('Year') + ylab('Spring survey index') +  
  theme (panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
         strip.background = element_blank(), strip.text.x = element_blank())


## ICES plot
ssb.plot <- 
  fit$res.by.year %>%
  mutate(species = gsub('imm|mat','',stock)) %>%
  filter(year>1980 & grepl('mat',stock)) %>%
  ggplot(aes(year,total.biomass/1e6,fill=species)) + 
  geom_bar(stat='identity') +  
  theme_bw() + xlab('') + ylab('SSB') + 
  scale_fill_brewer(palette="Paired")+
  theme(legend.position='none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0,0,0,0),'cm'),
        panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
        strip.background = element_blank(), strip.text.x = element_blank())


rec.plot <- 
  fit$res.by.year %>%
  mutate(species = gsub('imm|mat','',stock)) %>%
  filter(year>1980 & grepl('imm',stock)) %>%
  ggplot(aes(year,recruitment/1e6,fill=species)) + 
  geom_bar(stat='identity') +  
  theme_bw() + xlab('') + ylab('Recruitment') +
  scale_fill_brewer(palette="Paired")+
  theme(legend.position='none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0,0,0,0),'cm'),
        panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
        strip.background = element_blank(), strip.text.x = element_blank())


F.plot <- 
  fit$res.by.year %>%
  mutate(species = gsub('imm|mat','',stock)) %>%
  filter(year>1980 & grepl('mat',stock)) %>%
  ggplot(aes(year,F,col=species)) + 
  geom_line() +  
  theme_bw() + xlab('') + ylab('Fishing mortality')+
  scale_color_brewer(palette="Paired")+
  theme(legend.position='none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0,0,0,0),'cm'),
        panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
        strip.background = element_blank(), strip.text.x = element_blank())

catch.plot <- 
  fit$res.by.year %>%
  mutate(species = gsub('imm|mat','',stock)) %>%
  filter(year>1980 ) %>%
  group_by(year,species) %>% 
  summarise(catch=sum(catch)) %>%
  ggplot(aes(year,catch/1e6,fill=species)) + 
  geom_bar(stat='identity') +  
  theme_bw() + xlab('Year') + ylab('Total catch')+ 
  scale_fill_brewer(palette="Paired")+
  theme(legend.position='none',
        plot.margin = unit(c(0,0,0,0),'cm'),
        panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
        strip.background = element_blank(), strip.text.x = element_blank())

grid_arrange_shared_legend(ssb.plot,F.plot,rec.plot,catch.plot)


## forecasts
load('01-cod/02-mfdbcod/prognByYear.Rdata')

tmp <- 
  progn.by.year %>% ungroup() %>% group_by(year,effort) %>% 
  summarise(ssb.1 = total.bio[1],ssb.m = median(total.bio), 
            ssb.u = quantile(total.bio,0.975),ssb.l = quantile(total.bio,0.025),
            catch.1 = catch[1],catch.m = median(catch), 
            catch.u = quantile(catch,0.975),catch.l = quantile(catch,0.025)) 

prog.bio.plot <-
  ggplot(tmp,aes(year,ssb.m/1e6, col=as.factor(effort))) +
  geom_rect(aes(xmin=max(fit$res.by.year$year),
                xmax=Inf,ymin=-Inf,ymax=Inf),
            fill = 'gray90', alpha=0.1,col='white') +
  geom_ribbon(aes(year,ymax=ssb.u/1e6,ymin=ssb.l/1e6,
                  fill=as.factor(effort)),col='gray90',alpha=0.1) + 
  geom_line() + #geom_line(aes(year,ssb.1/1e6),lty=2)+
  theme_bw() + xlab('') + ylab('SSB') +
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none') + 
  ylim(c(0,900)) + xlim(c(1980,2030))


prog.catch.plot <-
  ggplot(tmp,aes(year,catch.m/1e6, col=as.factor(effort))) +
  geom_rect(aes(xmin=max(fit$res.by.year$year),
                xmax=Inf,ymin=-Inf,ymax=Inf),
            fill = 'gray90', alpha=0.1,col='white') +
  geom_ribbon(aes(year,ymax=catch.u/1e6,ymin=catch.l/1e6,
                  fill=as.factor(effort)),col='gray90',alpha=0.1) + 
  geom_line() + #geom_line(aes(year,ssb.1/1e6),lty=2)+
  theme_bw() + xlab('Year') + ylab('Catch') +
  theme(plot.margin = unit(c(0,0,0,0),'cm'),
        legend.title = element_blank(),
        legend.position = 'none') +
  xlim(c(1980,2030))


grid.arrange(prog.bio.plot,prog.catch.plot,ncol=1)

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
lfi.plot <- 
  fit$stock.full %>% 
  filter(year>1980) %>%
  group_by(year) %>% 
  summarise(total.bio = sum(number*mean.weight),
            lfi = sum((length>60) * number * mean.weight)/total.bio) %>%
  ggplot(aes(year,lfi)) + geom_line() + 
  ylab('LFI') + xlab('Year') + theme_bw()

## shannon index 
shannon.plot <- 
  fit$res.by.year %>% 
  filter(year>1980) %>%
  group_by(year) %>%
  mutate(p = catch/sum(catch)) %>% 
  summarise(shannon = -1*sum(p*log(p)),
            eveness = shannon/log(length(catch))) %>%
  ggplot(aes(year,shannon)) + geom_line() + theme_bw() + 
  ylab('Shannon index') + xlab('Year')

## Mean maximum length
mml.plot <- 
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

## GES plot
grid.arrange(lfi.plot,shannon.plot,mml.plot)

## Revenue by year (using mean fish price from 2014-2015)
price <- data.frame(species = c('cod','had','pok','ling','tusk','wolf'),
                    price = c(275,305,187,189.7,123,156))

fit$res.by.year %>%
  mutate(species = gsub('mat|imm','',stock)) %>%
  left_join(price) %>%
  mutate(revenue = catch*price) %>%
  ggplot(aes(year,revenue/1e9,fill=species)) + geom_bar(stat='identity') + 
  xlab('Year') + ylab('Revenue (in Billion ISK)') + theme_bw() +
  scale_fill_brewer(palette="Paired")

  