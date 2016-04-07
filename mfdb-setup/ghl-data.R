library(dplyr)
library(dplyrOracle)
library(mar)
library(tidyr)
library(readxl)

ghl.landings <- 
  read.table('http://data.hafro.is/assmt/2015/g_halibut/landings.csv',header=TRUE,sep=',') %>% 
  select(-Total) %>% 
  gather(who,catch,-Year)

tmp <- 
landadur_afli(mar) %>% 
  filter(fteg==22) %>% 
  group_by(veidisvaedi,ar) %>% 
  summarise(c=sum(magn_oslaegt)/1000) %>% 
  collect()

ldist <- 
  read_excel('mfdb-setup/Data/DataCall_Greenland_halibut_Faroes_final.xls',sheet = 'LengthDistribution',skip = 24)

ldist %>% 
  na.omit() %>% 
  group_by(ÁR,CM) %>%
  summarise(n=sum(TAL)) %>% 
ggplot(aes(CM,n)) + geom_point() + facet_wrap(~ÁR,scale='free_y')+ xlim(c(0,100))


aldist <- 
  read_excel('mfdb-setup/Data/DataCall_Greenland_halibut_Faroes_final.xls',sheet = 'Growth_2015',skip = 6)

names(aldist) <- 
  c('Recnr','st_id','t1','species_id','species','length','sex','weight','maturity','fish_num','Age')
ldist  %>% ggplot(aes(length,Age,col=ifelse(sex==1,'Female','Male'))) + geom_point() + geom_smooth()
