library(dplyr)
library(dplyrOracle)
library(mar)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)

library(mfdb)
mdb <- mfdb('Iceland')

ghl.landings <- 
  read.table('http://data.hafro.is/assmt/2015/g_halibut/landings.csv',header=TRUE,sep=',') %>% 
  select(-Total) %>% 
  gather(who,catch,-Year)


new.landings <- 
  read.csv('mfdb-setup/Data/ghlNewlandings.csv') %>% 
  mutate(Area = as.numeric(gsub('27_([0-9]+).+$','\\1',Area))) %>% 
  filter(!(Country %in% c('NO','IS')),
         Area %in% c(5,6,12,14)) %>%
  dplyr::rename(year = Year) %>% 
  left_join(expand.grid(year=2015,month=1:12)) %>% 
  mutate(sampling_type = 'FLND', 
         species = 'GLH',
         count = 1000*Catch/12,
         gear = 'BMT', ## this needs serious consideration
         areacell = 2741) %>% 
  select(-c(AphiaID:Country,Catch)) %>% 
  as.data.frame()

mfdb_import_survey(mdb,
                   data_source = 'ICES-interim-catch',
                   new.landings)
  

ghl.landings %>% 
  ggplot(aes(Year,catch,fill=who)) + 
  geom_bar(stat='identity') + 
  theme_minimal()


tmp <- 
landadur_afli(mar) %>% 
  filter(fteg==22,flokkur != -4) %>% 
  group_by(veidisvaedi,ar) %>% 
  summarise(c=sum(magn_oslaegt)/1000) %>% 
  collect()




faroe.landings <- 
  read_excel('mfdb-setup/Data/DataCall_Greenland_halibut_Faroes_final.xls',
             sheet = 'Catch',skip = 11,na='-',col_names = FALSE) 
names(faroe.landings) <- 
  c('year','man',paste(rep(c('s_trawl','p_trawl','longline','jiggers','gillnet','all_gears'),each=4),
                       rep(c('faroese','auction','foreign','total')),sep='.'))[-16]
#faroe.landings <- 
faroe.landings %>% 
  mutate(year = rep(1993:2015,each=12),
         man = gsub('.','',tolower(man),fixed=TRUE),
         date = parse_date_time(paste('01',man,
                                      year,sep='.'),
                                '%d.%b.%Y', locale = 'fo_FO.utf8')) %>% 
  gather(gear,catch,-c(year,man,date)) %>% 
  separate(gear,c('gear','buyer'),sep='\\.') -> tmp 
filter(buyer == 'total',gear != 'all_gears') 
ggplot(aes(man,catch,fill=gear)) + geom_bar(stat='identity') + 
  facet_wrap(~year,scale='free_y')




## faroese samples
fo.st <- 
  read_excel('mfdb-setup/Data/DataCall_Greenland_halibut_Faroes_final.xls',
             sheet = 'Tow_duration',skip = 24) %>% 
  rename(year=Year, tow_id = STODNR, lat = POSBRE, lon = POSLGD, depth = DYPI, tow_duration = TOVTIMAR) %>% 
  mutate(areacell = geo::d2sr(lat,lon))

fo.ldist <- 
  read_excel('mfdb-setup/Data/DataCall_Greenland_halibut_Faroes_final.xls',
             sheet = 'LengthDistribution',skip = 24) %>% 
  rename(year=ÁR, tow_id = STODNR, lat = POSB, lon = POSL, depth = DYPI, length = CM, count = TAL) %>% 
  mutate(month=6,areacell = geo::d2sr(lat,lon),species = 'GLH',
         sampling_type = 'FAER') %>% 
  filter(!is.na(year)) %>% 
  as.data.frame()

mfdb_import_survey(mdb,
                   data_source = 'faeroes-ldist',
                   fo.ldist)

fo.aldist <- 
  read_excel('mfdb-setup/Data/DataCall_Greenland_halibut_Faroes_final.xls',sheet = 'Growth_2015',skip = 6) 

names(fo.aldist) <- 
  c('Recnr','tow_id','t1','species_id','species','length','sex','weight','maturity','fish_num','age')
fo.aldist <- 
  fo.aldist  %>% 
  mutate(year = 2015,
         month = 6,
         areacell = 1041,
         sex = ifelse(sex==1,'F','M'),
         length=length/10,
         species = 'GLH',
         maturity = pmin(2,maturity),
         sampling_type = 'FAER') %>%
  rename(count=fish_num) %>% 
  select(-c(t1,Recnr)) %>% 
  as.data.frame()

mfdb_import_survey(mdb,
                   data_source = 'faeroes-aldist',
                   fo.aldist)
  
vonB.par <-
  list(nls(length~Linf*(1-exp(-K*(age-t0))),
           data=filter(fo.aldist,sex=='M'), start=list(Linf=110, K=0.1, t0=-1)) %>% 
         tidy() %>% 
         mutate(sex='M'),
       nls(length~Linf*(1-exp(-K*(age-t0))),
           data=filter(fo.aldist,sex=='F'), start=list(Linf=110, K=0.1, t0=-1)) %>% 
         tidy() %>% 
         mutate(sex='F')
       ) %>% 
  bind_rows()


## greenland autum survey
load('mfdb-setup/Data/ghlCombinedSurveyData.RData')

gre.st <- 
  comb.st %>% 
  filter(grepl('Gre',leidangur) ) %>% 
  select(sample_id = synis.id,lat,lon,year=ar,month=man,day = dags,tow_length=toglengd) %>% 
  mutate(areacell = geo::d2sr(lat,lon),
         gear = 'BMT',
         month = 10,
         sampling_type='GRE') ## compatibility with the Icelandic autumn survey

gre.le <- 
  comb.le.sex %>% 
  select(sample_id=synis.id,length=lengd,count=fjoldi,sex=kyn) %>% 
  inner_join(gre.st) %>% 
  inner_join(comb.nr %>% filter(fj.talid>0) %>% 
               mutate(r = 1 + fj.talid/fj.maelt) %>% 
               select(sample_id=synis.id,r)) %>% 
  mutate(r=ifelse(is.na(r),1,r),
         count = r*count,
         sex=c('M','F')[sex],
         species = 'GLH') %>% 
  select(-r)
  

mfdb_import_survey(mdb,
                   data_source = 'greenland-ldist',
                   gre.le %>% filter(areacell %in% reitmapping$GRIDCELL))
