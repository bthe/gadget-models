## devtools::install_github('mareframe/mfdb',ref='4.x')
library(mfdb)
library(geo)
library(dplyr)
library(dplyrOracle)
library(mar)
library(purrr)
library(tidyr)
library(stringr)


## oracle connection from https://github.com/tomasgreif/dplyrOracle
mar <- dplyrOracle::src_oracle("mar")

## Create connection to MFDB database, as the Icelandic case study
mdb <- mfdb('Iceland')

## Import area definitions
reitmapping <- read.table(
  system.file("demo-data", "reitmapping.tsv", package="mfdb"),
  header=TRUE,
  as.is=TRUE) %>% 
  tbl_df() %>% 
  mutate(id = 1:n(),
         lat = geo::sr2d(GRIDCELL)$lat,
         lon = geo::sr2d(GRIDCELL)$lon) %>% 
  by_row(safely(function(x) geo::srA(x$GRIDCELL),otherwise=NA)) %>% 
  unnest(size=.out %>% map('result')) %>% 
  select(-.out) %>% 
  na.omit()

copy_to(mar,reitmapping,'reitmapping')
        
mfdb_import_area(mdb, 
                 reitmapping %>% 
                   select(id,
                          name=GRIDCELL,
                          size)) 

mfdb_import_division(mdb,
                     plyr::dlply(reitmapping,
                           'SUBDIVISION',
                           function(x) x$GRIDCELL))

mfdb_import_temperature(mdb, 
                        expand.grid(year=1960:2016,
                                    month=1:12,
                                    areacell = reitmapping$GRIDCELL,
                                    temperature = 3))

## BEGIN hack
## species mapping
  data.frame(tegund =  c(1:11, 19, 21, 22,
                         23, 25, 27, 30, 31, 48 ),
             species = c('COD','HAD','POK','WHG','RED','LIN','BLI','USK',
                         'CAA','RNG','REB','GSS','HAL','GLH',
                         'PLE','WIT','DAB','HER','CAP','LUM')) %>% 
  copy_to(mar,.,'species_key')

## gear mapping
ice.gear <-
  read.table('veidarf.txt',header=TRUE)

mapping <- read.table(header=TRUE,
                      file='mapping.txt') 

mapping <-
  mutate(merge(mapping, gear, by.x='gear',
               by.y = 'id'),
         gear=NULL,
         description=NULL)
names(mapping)[2] <- 'gear'
mapping$gear <- as.character(mapping$gear)

copy_to(dest = mar, 
        df = mapping,
        name = 'gear_mapping')

## Set-up sampling types

mfdb_import_sampling_type(mdb, data.frame(
  id = 1:14,
  name = c('SEA', 'IGFS','AUT','SMN','LND','LOG','INS','ACU','FLND','OLND','CAA',
           'CAP','GRE','FAER'),
  description = c('Sea sampling', 'Icelandic ground fish survey',
                  'Icelandic autumn survey','Icelandic gillnet survey',
                  'Landings','Logbooks','Icelandic nephrop survey',
                  'Acoustic capelin survey','Foreign vessel landings','Old landings (pre 1981)',
                  'Old catch at age','Capelin data','Eastern Greenland autumn survey',
                  'Faeroese summer survey')))

## stations table

## Import length distribution from commercial catches
## 1 inspectors, 2 hafro, 8 on-board discard
stations <-
  lesa_stodvar(mar) %>% 
  filter(synaflokkur %in% c(1,2,8,10,12,30,34,35)) %>% 
  mutate(sampling_type = ifelse(synaflokkur %in% c(1,2,8),'SEA',
                                ifelse(synaflokkur %in% c(10,12),'CAP',
                                       ifelse(synaflokkur == 30,'IGFS',
                                              ifelse(synaflokkur==35,'AUT','SMN'))))) %>% 
  left_join( tbl(mar,'gear_mapping'),by='veidarfaeri') %>% 
  select(synis_id,ar,man,lat=kastad_n_breidd,lon=kastad_v_lengd,gear,sampling_type) %>% 
  mutate(areacell=d2sr(lat,lon),
         lat = floor(lat/10000)+(lat/10000-floor(lat/10000))/0.6,
         lon = floor(lon/10000)+(lon/10000-floor(lon/10000))/0.6) %>% 
  inner_join(tbl(mar,'reitmapping') %>% 
               select(areacell=GRIDCELL),
             by='areacell') %>% 
  rename(year = ar, month = man)


## length distributions
ldist <- 
  lesa_lengdir(mar) %>% 
  inner_join(tbl(mar,'species_key')) %>% 
  right_join(stations) %>% 
  left_join(lesa_numer(mar) %>% #ifelse(fj_maelt>0,1,fj_maelt)
              filter(fj_maelt>0) %>% 
              mutate(r = 1 + fj_talid/fj_maelt) %>%
              select(synis_id,tegund, r),
            by = c("synis_id","tegund")) %>% 
  mutate(lengd = ifelse(is.na(lengd), 0, lengd),
         fjoldi = ifelse(is.na(fjoldi), 0, fjoldi),
         r = ifelse(is.na(r), 1 , r),
         count = round(r * fjoldi,0),
         kyn = ifelse(kyn == 2,'F',ifelse(kyn ==1,'M','')),
         kynthroski = ifelse(kynthroski > 1,2,ifelse(kynthroski == 1,1,NA)),
         age = 0)%>%
  select(-c(fjoldi,r,tegund)) %>% 
  rename(sex=kyn,maturity_stage = kynthroski,
         length = lengd) %>% 
  collect(n=Inf) %>% 
  as.data.frame()

mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist',
                   ldist)
rm(ldist)

## age -- length data
aldist <-
  lesa_kvarnir(mar) %>% 
  right_join(stations) %>% 
  inner_join(tbl(mar,'species_key')) %>%
  mutate(lengd = ifelse(is.na(lengd), 0, lengd),
         count = 1,
         kyn = ifelse(kyn == 2,'F',ifelse(kyn ==1,'M',NA)),
         kynthroski = ifelse(kynthroski > 1,2,ifelse(kynthroski == 1,1,NA)))%>%
  select(synis_id, lat,lon, year,month, areacell, gear,
         sampling_type,count,species,
         age=aldur,sex=kyn,maturity_stage = kynthroski,
         length = lengd, no = nr, weight = oslaegt,
         gutted = slaegt, liver = lifur, gonad = kynfaeri) %>% 
  collect(n=Inf) %>% 
  as.data.frame()
 

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist',
                   aldist)
rm(aldist)
## landings 
port2division <- function(hofn){
  hafnir.numer <- rep(0,length(hofn))
  hafnir.numer[hofn<=15] <- 110
  hafnir.numer[hofn>=16 & hofn<=56] <- 101
  hafnir.numer[hofn>=57 & hofn<=81] <- 102
  hafnir.numer[hofn>=82 & hofn<=96] <- 104
  hafnir.numer[hofn==97] <- 103
  hafnir.numer[hofn>=98 & hofn<=115] <- 104
  hafnir.numer[hofn>=116 & hofn<=121] <- 105
  hafnir.numer[hofn>=122 & hofn<=148] <- 106
  hafnir.numer[hofn==149] <- 109
  hafnir.numer[hofn>=150] <- 111
  data.frame(hofn=hofn,division=hafnir.numer) 
}

gridcell.mapping <-
  plyr::ddply(reitmapping,~DIVISION,function(x) head(x,1)) %>% 
  rename(areacell=GRIDCELL,division=DIVISION,subdivision=SUBDIVISION)

port2division(0:999) %>% 
  left_join(gridcell.mapping) %>% 
  copy_to(mar,.,'port2sr')    

## catches after 1992, pre 92 is a hack
landadur_afli(mar) %>% 
  filter(veidisvaedi == 'I',flokkur != -4) %>% 
  left_join( tbl(mar,'gear_mapping'),by='veidarfaeri') %>%
  inner_join( tbl(mar,'species_key'),by=c('fteg'='tegund')) %>%
  left_join(tbl(mar,'port2sr'),by='hofn') %>%
  mutate(sampling_type='LND',
         gear = ifelse(is.na(gear),'LLN',gear)) %>% 
  select(count=magn_oslaegt,sampling_type,areacell, species,year=ar,month=man,
         gear) %>% 
  filter(species == 'LIN') %>% 
  group_by(year) 
  



url <- 'http://data.hafro.is/assmt/2015/'

x1 <- gsub("<([[:alpha:]][[:alnum:]]*)(.[^>]*)>([.^<]*)", "\\3",
           readLines(url))
x2<-gsub("</a>", "", x1)
sp.it <- sapply(strsplit(x2[grepl('/ ',x2)],'/'),function(x) str_trim(x[1]))[-1]
spitToDST2 <- 
  read.table(text = 
               paste('species shortname',
                     'anglerfish MON',
                     'b_ling BLI',
                     'b_whiting WHB',
                     'capelin CAP',
                     'cod COD',
                     'cucumber HTZ',
                     'dab DAB',
                     'g_halibut GLH',
                     'haddock HAD',
                     'halibut HAL',
                     'herring HER',
                     'l_sole LEM',
                     'ling LIN',
                     'lumpfish LUM',
                     'mackerel MAC',
                     'megrim MEG',
                     'nephrops NEP',
                     'plaice PLE',
                     'quahog QUA',
                     'r_dab DAB',
                     's_mentella REB',
                     's_norvegicus RED',
                     's_smelt GSS',
                     's_viviparus REV',
                     's_wolfish CAA',
                     'saithe POK',
                     'scallop CYS',
                     'tusk USK',
                     'urchin EEZ',
                     'whelk CSI',
                     'whiting WHG',
                     'witch WIT',
                     'wolffish CAA',
                     sep = '\n'),
             header = TRUE)



landingsByYear <-
  plyr::ldply(sp.it, function(x){
    #print(x)
    tmp <- tryCatch(read.csv(sprintf('%s%s/landings.csv',url,x)),
                    error=function(x) data.frame(Total=0))
    tmp$species <- x
    
    #print(names(tmp))
    return(tmp)
  })


landingsByMonth <-
  landingsByYear %>%
  filter(species!='capelin') %>% 
  left_join(spitToDST2) %>%
  inner_join(tbl(mar,'species_key') %>% collect(), by = c('shortname'='species')) %>% 
  filter(!is.na(shortname) & !is.na(Year)) %>%
  select(shortname,Year,Iceland,Total) %>%
  left_join((expand.grid(Year=1905:2015,month=1:12))) %>%
  mutate(year = Year,
         sampling_type = 'FLND', 
         species = shortname,
         Others = Total - Iceland,
         count = 1000*Others/12,
         gear = 'LLN', ## this needs serious consideration
         areacell = 2741) %>% ## just to have something
  mutate(shortname = NULL,
         Others = NULL,
         Total = NULL) %>% 
  as.data.frame()



mfdb_import_survey(mdb,
                   data_source = 'foreign.landings',
                   landingsByMonth)

oldLandingsByMonth <-
  landingsByYear %>%
  left_join(spitToDST2) %>%
  inner_join(tbl(mar,'species_key') %>% collect(), by = c('shortname'='species')) %>%
  filter(!is.na(shortname) & !is.na(Year) &  Year < 1982) %>%
  select(shortname,Year,Others,Total) %>%
  left_join(expand.grid(Year=1905:1981,month=1:12)) %>%
  mutate(year = Year,
         sampling_type = 'OLND', 
         species = shortname,
         count = 1000*(Total - Others)/12,
         gear = 'BMT', ## this needs serious consideration
         areacell = 2741) %>% ## just to have something
  mutate(shortname = NULL,
         Others = NULL,
         Total = NULL) %>% 
  as.data.frame()

mfdb_import_survey(mdb,
                   data_source = 'old.landings',
                   oldLandingsByMonth)
