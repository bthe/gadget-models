## To run this script, first install data sources: fjolst, geo
library(mfdb)
library(fjolst)
library(Logbooks)
library(geo)
library(plyr)
library(data.table)
library(dplyr)
library(stringr)
library(dplyrOracle)
library(reshape2)
library(tidyr)

## oracle connection
mar <- dplyrOracle::src_oracle("mar")

## Create connection to MFDB database, as the Icelandic case study
mdb <- mfdb('Iceland')#,db_params=list(host='hafgeimur.hafro.is'))
#species.key <-
#    mfdb:::mfdb_fetch(mdb,"select * from species where name in",
#                      "('COD','HAD','POK','WHG','WHB','USK','REB',",
#                      "'RED','PLE','DAB','GSS','CAA','LUM','WIT',",
#                      "'GLH','HAL','RNG','HER','CAP')")
#sp <- sql('select * from fiskar.ices_tegundir')
species.key <-
    data.table(tegund =  c(1:11, 19, 21, 22,
                   23, 25, 27, 30, 31, 48 ),
               species = c('COD','HAD','POK','WHG','RED','LIN','BLI','USK',
                   'CAA','RNG','REB','GSS','HAL','GLH',
                   'PLE','WIT','DAB','HER','CAP','LUM')) %>%
    group_by(tegund)

## prey names
## NOTE serious issues with the stomach data
## -800 species categories cannot be matched with the species table
## -what can be done with large group of undetermine species, e.g. pisces
p.names <-
    tbl(mar,sql('faeda.f_tegundir')) %>%
    select(FAEDUHOPUR,ICES_HEITI) %>%
    collect() %>%
    setnames(old=c("FAEDUHOPUR","ICES_HEITI"),
             new=c("faeduhopur","id")) %>%
    left_join(species) %>%
    filter(!is.na(id) & !is.na(name))


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


## Import area definitions
reitmapping <- read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

mfdb_import_area(mdb, data.frame(
    id = 1:nrow(reitmapping),
    name = reitmapping$GRIDCELL,
    ## area size is 
    size = 30*60*cos(geo::r2d(reitmapping$GRIDCELL)$lat*pi/180))) 

mfdb_import_division(mdb,
                     dlply(reitmapping,
                           'SUBDIVISION',
                           function(x) x$GRIDCELL))
mfdb_import_temperature(mdb, data.frame(
    year = rep(1960:2015,each=12),
    month = 1:12,
    areacell = reitmapping$GRIDCELL[1],
    temperature = 3))




## Set-up some sampling types

mfdb_import_sampling_type(mdb, data.frame(
    id = 1:12,
    name = c('SEA', 'IGFS','AUT','SMN','LND','LOG','INS','ACU','FLND','OLND','CAA',
             'CAP'),
    description = c('Sea sampling', 'Icelandic ground fish survey',
        'Icelandic autumn survey','Icelandic gillnet survey',
        'Landings','Logbooks','Icelandic nephrop survey',
        'Acoustic capelin survey','Foreign vessel landings','Old landings (pre 1981)',
        'Old catch at age','Capelin data')))


## Import length distribution from commercial catches
## 1 inspectors, 2 hafro, 8 on-board discard
stations <-
    data.table(subset(stodvar,
                      synaflokkur %in% c(1,2,8),
                      select = c(synis.id,ar,man,lat,lon,veidarfaeri))) %>%
    left_join(data.table(mapping)) %>%
    group_by(synis.id) %>%
    mutate(areacell = d2sr(lat,lon),
           veidarfaeri = NULL) %>%
    filter(areacell %in% reitmapping$GRIDCELL &
           !is.na(gear))
           
ldist <- data.table(all.le) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    left_join(data.table(all.nu)) %>%
    mutate(count=fjoldi,
           sex = c('M','F')[pmax(1,kyn)],
           age = 0,
           month = man,
           sampling_type = 'SEA',
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           fjoldi = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL) 
    
setnames(ldist,
         c('synis.id','ar','lengd'),
         c('sample.id','year','length'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist.comm',
                   ldist)
rm(ldist)


## Import age-length frequencies
aldist <-
    data.table(all.kv) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    mutate(count=1,           
           sex = c('M','F')[pmax(1,kyn)],
           age = aldur,
           sampling_type = 'SEA',
           month = man,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL,
           aldur=NULL)
               
setnames(aldist,
         c('synis.id','ar','lengd', 'nr',
           'oslaegt', 'slaegt', 'lifur','kynfaeri'),
         c('sample.id','year','length','no',
           'weight','gutted','liver', 'gonad'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.comm',
                   aldist)

rm(aldist)



## capelin data

stations <-
    data.table(subset(stodvar,
                      synaflokkur %in% c(10,12),
                      select = c(synis.id,ar,man,lat,lon,veidarfaeri))) %>%
    left_join(data.table(mapping)) %>%
    group_by(synis.id) %>%
    mutate(areacell = d2sr(lat,lon),
           veidarfaeri = NULL) %>%
    filter(areacell %in% reitmapping$GRIDCELL &
           !is.na(gear))
           
ldist <- data.table(all.le) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    left_join(data.table(all.nu)) %>%
    mutate(count=fjoldi,
           sex = c('M','F')[pmax(1,kyn)],
           age = 0,
           month = man,
           sampling_type = 'CAP',
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           fjoldi = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL) 
    
setnames(ldist,
         c('synis.id','ar','lengd'),
         c('sample.id','year','length'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist.cap',
                   ldist)
rm(ldist)


## Import age-length frequencies
aldist <-
    data.table(all.kv) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    mutate(count=1,           
           sex = c('M','F')[pmax(1,kyn)],
           age = aldur,
           sampling_type = 'CAP',
           month = man,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL,
           aldur=NULL)
               
setnames(aldist,
         c('synis.id','ar','lengd', 'nr',
           'oslaegt', 'slaegt', 'lifur','kynfaeri'),
         c('sample.id','year','length','no',
           'weight','gutted','liver', 'gonad'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.cap',
                   aldist)

rm(aldist)



## surveys
## TODO: gillent and nephrop surveys

## Import length distribution from spring survey
stations <-
    data.table(subset(stodvar, synaflokkur == 30, ## autumn survey 35
                      select = c(synis.id,ar,man,lat,lon,veidarfaeri))) %>%
    left_join(data.table(mapping)) %>%
    group_by(synis.id) %>%
    setnames(old="ar",new="year") %>%
    mutate(month = 3,
           areacell = d2sr(lat,lon),
           sampling_type = 'IGFS') %>%
    filter(!is.na(areacell))
           

ldist <- data.table(all.le) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    left_join(data.table(all.nu)) %>%
    mutate(count=round(fjoldi*pmax(fj.talid+fj.maelt,1,na.rm=TRUE)/
               pmax(1,fj.maelt,na.rm=TRUE)),
           sex = c('M','F')[pmax(1,kyn)],
           age = 0,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    ungroup() %>%
    mutate(man = NULL,
           fjoldi = NULL,
           kyn = NULL,
           kynthroski=NULL,
           fj.maelt = NULL,
           fj.talid = NULL,
           afli=NULL,
           vigt.synis = NULL,
           tegund = NULL,
           veidarfaeri = NULL)           
    
setnames(ldist,
         c('synis.id','lengd'),
         c('sample.id','length'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist.igfs',
                   ldist)
rm(ldist)

## Import age - length frequencies from the spring survey

aldist <- data.table(all.kv) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    mutate(count=1,
           areacell = d2sr(lat,lon),
           sex = c('M','F')[pmax(1,kyn)],
           age = aldur,
           sampling_type = 'IGFS',
           month = 3,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL,
           veidarfaeri = NULL,
           aldur=NULL)           
    
setnames(aldist,
         c('synis.id','lengd', 'nr',
           'oslaegt', 'slaegt', 'lifur','kynfaeri'),
         c('sample.id','length','no',
           'weight','gutted','liver', 'gonad'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.igfs',
                   aldist)
rm(aldist)


## stomach data
predators <-
    ffiskar %>%
        rename(stomach_name=flokk.id, tegund=ranfiskur, length=lengd) %>%
#    setnames(old=c('flokk.id','ranfiskur','lengd'),
#             new=c('stomach_name','tegund','length')) %>%
    data.table %>%
    group_by(synis.id) %>%
    filter(synis.id %in% stations$synis.id) %>%
    left_join(stations) %>%
    left_join(species.key)

preys <-
    fhopar %>%
    filter(faeduhopur %in% p.names$faeduhopur) %>%
    left_join(p.names) %>%
        rename(stomach_name = flokk.id, count=fjoldi,weight=thyngd,
               digestion_stage=meltingarstig,species=name)%>%
#        setnames(old=c('flokk.id','fjoldi','thyngd','meltingarstig','name'),
#             new=c('stomach_name','count','weight','digestion_stage','species')) %>%
    mutate(digestion_stage = digestion_stage+1) %>%
    filter(stomach_name %in% predators$stomach_name)

mfdb_import_stomach(mdb,
                    data_source = 'stomachdata.igfs',
                    predator_data = filter(predators,stomach_name %in% preys$stomach_name),
                    prey_data = preys)
               


## Now do the same for the autumn survey
stations <-
    stodvar %>%
    filter(synaflokkur == 35) %>%
    select(synis.id,ar,man,lat,lon,veidarfaeri) %>%
    left_join(data.table(mapping)) %>%
    group_by(synis.id)  %>%
    setnames(old="ar",new="year") %>%
    mutate(month = 10,
           areacell = d2sr(lat,lon),
           sampling_type = 'AUT') %>%
    filter(!is.na(areacell))

ldist <- data.table(all.le) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations,copy=TRUE) %>%
    left_join(species.key,copy=TRUE) %>%
    left_join(data.table(all.nu),copy=TRUE) %>%
    mutate(count=round(fjoldi*pmax(fj.talid+fj.maelt,1,na.rm=TRUE)/
               pmax(1,fj.maelt,na.rm=TRUE)),
           sex = c('M','F')[pmax(1,kyn)],
           age = 0,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           fjoldi = NULL,
           kyn = NULL,
           kynthroski=NULL,
           fj.maelt = NULL,
           fj.talid = NULL,
           afli=NULL,
           vigt.synis = NULL,
           tegund = NULL,
           veidarfaeri = NULL)
           
    
setnames(ldist,
         c('synis.id','ar','lengd'),
         c('sample.id','year','length'))



mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist.aut',
                   ldist)
rm(ldist)

## Import age - length frequencies from the spring survey

aldist <- data.table(all.kv) %>%
    filter(synis.id %in% stations$synis.id &
           tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations,copy=TRUE) %>%
    left_join(species.key,copy=TRUE) %>%
    mutate(count=1,
           sex = c('M','F')[pmax(1,kyn)],
           age = aldur,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL,
           veidarfaeri = NULL,
           aldur=NULL)
           
    
setnames(aldist,
         c('synis.id','lengd', 'nr',
           'oslaegt', 'slaegt', 'lifur','kynfaeri'),
         c('sample.id','length','no',
           'weight','gutted','liver', 'gonad'))



mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.aut',
                   aldist)
rm(aldist)

## stomach data
predators <-
    ffiskar %>%
    setnames(old=c('flokk.id','ranfiskur','lengd'),
             new=c('stomach_name','tegund','length')) %>%
    data.table %>%
    group_by(synis.id) %>%
    filter(synis.id %in% stations$synis.id) %>%
    left_join(stations) %>%
    left_join(species.key)

preys <-
    fhopar %>%
    filter(faeduhopur %in% p.names$faeduhopur) %>%
    left_join(p.names) %>%
    setnames(old=c('flokk.id','fjoldi','thyngd','meltingarstig','name'),
             new=c('stomach_name','count','weight','digestion_stage','species')) %>%
    mutate(digestion_stage = digestion_stage+1) %>%
    filter(stomach_name %in% predators$stomach_name)

mfdb_import_stomach(mdb,
                    data_source = 'stomachdata.aut',
                    predator_data = filter(predators,stomach_name %in% preys$stomach_name),
                    prey_data = preys)



## let's do landings (by port)
mfdb_import_cs_taxonomy(mdb,'index_type',data.frame(name='landings'))

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
    data.table(hofn=hofn,division=hafnir.numer) %>%
        group_by(hofn,division) 
}

gridcell.mapping <-
    ddply(reitmapping,~DIVISION,function(x) head(x,1))
names(gridcell.mapping) <- c('areacell','division','subdivision')

species.key <- species.key %>%
    mutate(fteg=tegund)

mapping <- mapping %>%
     mutate(veidarf=veidarfaeri)

port.mapping <-
    port2division(0:999) %>%
    left_join(data.table(gridcell.mapping))

landings <-
    data.table(landedcatch) %>%
    group_by(veidisvaedi, gerd,ar, hofn, man, dags, veidarf, fteg) %>%
    left_join(port.mapping) %>%
    left_join(species.key) %>%
    left_join(mapping,copy=TRUE) %>%
    filter(!is.na(species) & !(veidarf %in% c(0,37,45,139)) &
           (is.na(veidisvaedi) | veidisvaedi == 'I'))%>%
    mutate(#index_type='landings',
           sampling_type='LND',
           count = magn,
           gear = ifelse(is.na(gear),'LLN',gear),
           #value = magn,
           year = ar,
           month = man) %>%
    select(sampling_type,year,month,gear,areacell,count,species)

mfdb_import_survey(mdb,
                   data_source = 'commercial.landings',
                   landings)
    


## foreign fishing vessels
## begin ugly hack, remember to change years
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
             header = TRUE) %>%
    data.table



landingsByYear <-
    ldply(sp.it, function(x){
        #print(x)
        tmp <- tryCatch(read.csv(sprintf('%s%s/landings.csv',url,x)),
                        error=function(x) data.frame(Total=0))
        tmp$species <- x

        #print(names(tmp))
        return(tmp)
    }) %>%
    data.table

## catch and weight at age

catage <-
    ldply(sp.it, function(x){
        #print(x)
        tmp <- tryCatch(read.csv(sprintf('%s%s/catage.csv',url,x)),
                        error=function(x) data.frame(Total=0))
        tmp$species <- x
        if(is.null(tmp$Total))
            tmp <- melt(tmp,id.vars=c('Year','species'))
        #print(names(tmp))
        return(tmp)
    }) %>%
    data.table %>%
    filter(is.na(Total )) %>%
    left_join(spitToDST2) %>%
    filter(!is.na(value)) %>%
    mutate(age = as.numeric(gsub('[a-zA-Z]','',variable)),
           month = ifelse(species == 'capelin',
               ifelse(substr(variable,1,1)=='w',3,9),1),
           count = ifelse(species == 'capelin',
               1e9,1e6)*value,
           areacell = 2741,
           species = NULL,
           sampling_type = 'CAA') %>%
    setnames(c('Year','shortname'),c('year','species')) %>%
    select(year,age,month,count,areacell,species,sampling_type) %>%
    left_join(
        ldply(sp.it, function(x){
                                        #print(x)
            tmp <- tryCatch(read.csv(sprintf('%s%s/wcatch.csv',url,x)),
                            error=function(x) data.frame(Total=0))
            tmp$species <- x
            if(is.null(tmp$Total))
                tmp <- melt(tmp,id.vars=c('Year','species'))
                                        #print(names(tmp))
            return(tmp)
        }) %>%
        data.table %>%
        filter(is.na(Total )) %>%
        left_join(spitToDST2) %>%
        filter(!is.na(value)) %>%
        mutate(age = as.numeric(gsub('[a-zA-Z]','',variable)),
               month = ifelse(species == 'capelin',
                   ifelse(substr(variable,1,1)=='w',3,9),1),
               weight = value,
               areacell = 2741,
               species = NULL,
               sampling_type = 'CAA') %>%
        setnames(c('Year','shortname'),c('year','species')) %>%
        select(year,age,month,weight,areacell,species,sampling_type))

mfdb_import_survey(mdb,
                   data_source = 'catchatage.old',
                   catage)


capelinByMonth <-
    landingsByYear %>%
    filter(species=='capelin') %>%
    select(-c(Total,Winter,Summer)) %>%
    gather(country,catch,c(sIceland:sEU,wIceland:wGreenland),na.rm=TRUE) %>%
    select(Year,species,country,catch) %>%
    separate(country,c('season','country'),1) %>%
    left_join(spitToDST2) %>%
    filter(!is.na(shortname) & !is.na(Year) & shortname %in% species.key$species) %>%
    mutate(group = ifelse(country=='Iceland','Iceland','Other')) %>%
    group_by(Year,season,group) %>%
    summarise(catch=sum(catch)*1e6)

landingsByMonth <-
    landingsByYear %>%
    filter(species!='capelin') %>% 
    left_join(spitToDST2) %>%
    filter(!is.na(shortname) & !is.na(Year) & shortname %in% species.key$species) %>%
    select(shortname,Year,Others,Total) %>%
    group_by(shortname,Year) %>%
    left_join(data.table(expand.grid(Year=1905:2015,month=1:12))) %>%
    mutate(year = Year,
           sampling_type = 'FLND', 
           species = shortname,
           count = 1000*Others/12,
           gear = 'LLN', ## this needs serious consideration
           areacell = 2741) %>% ## just to have something
    ungroup %>%
    mutate(shortname = NULL,
           Others = NULL,
           Total = NULL)



mfdb_import_survey(mdb,
                   data_source = 'foreign.landings',
                   landingsByMonth)

oldLandingsByMonth <-
    landingsByYear %>%
    left_join(spitToDST2) %>%
    filter(!is.na(shortname) & !is.na(Year) & shortname %in% species.key$species & Year < 1982) %>%
    select(shortname,Year,Others,Total) %>%
    group_by(shortname,Year) %>%
    left_join(data.table(expand.grid(Year=1905:1981,month=1:12))) %>%
    mutate(year = Year,
           sampling_type = 'OLND', 
           species = shortname,
           count = 1000*(Total - Others)/12,
           gear = 'BMT', ## this needs serious consideration
           areacell = 2741) %>% ## just to have something
    ungroup %>%
    mutate(shortname = NULL,
           Others = NULL,
           Total = NULL)

mfdb_import_survey(mdb,
                   data_source = 'old.landings',
                   oldLandingsByMonth)



## acoustic data for capelin
tmp <- new.env()
load('capelin.RData',envir=tmp)
tmp$rep12.2[15] <- NULL
cap.dat <- ldply(tmp,function(x) x)

acoustic <-
    data.table(cap.dat) %>%
    select(.id,lon,lat,sa,date) %>%
    mutate(species = 'CAP',
           areacell = d2sr(lat,lon),
           date = as.Date(date,'%d.%m.%y'),
           month = ifelse(is.na(month(date)),1,
               month(date)),
           year = ifelse(is.na(year(date)),
               year(as.Date(str_extract(.id,'[0-9]+'),'%y')),
               year(date)),
           date = NULL,
           count = sa,
           sampling_type = 'ACU',
           .id = NULL,
           sa = NULL,
           date = NULL)

mfdb_import_survey(mdb,
                   data_source = 'acoustic.survey',
                   acoustic)


## import logbooks data (caution this is HUGE)
## trawlers

Log.key <-
    read.table(text=paste('variable species',
                     'blalanga BLI',
                     "djupkarfi REB",
                     "flatf FLX",
                     "graluda GLH",
                     "gulllax GSS",
                     "hlyri CAS",
                     "karfi RED",
                     "keila USK",
                     "kolmunni WHB",
                     "langa LIN",
                     "luda HAL",
                     "skarkoli PLE",
                     "skotus MON",
                     "skrapflura PLA",
                   "skrapfl PLA",
                   "storkj MEG",
                     "steinbitur CAA",
                     "torskur COD",
                     "tylura LEM",
                     "ufsi POK",
                     "ysa HAD",
                     "gulldepla PLS",
                     "lodna CAP",
                     "makrill MAC",
                     "norskislsild HER",
                     "sild HER",
                     "uthafskarfi REV" ,
                     "lysa WHG",
                     "tindask SYR",
                     "skata SKT",
                   "raekja PRA",
                   "sandkoli DAB",
                   
                     sep='\n'),
               header =TRUE) %>%
    data.table
                     
                     
                     
## bottomtrawl

bmt.catch <-
    botnv %>%
    melt(id.vars=names(botnv)[1:32]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 

mfdb_import_survey(mdb,
                   data_source = 'logbooks.bmt',
                   bmt.catch)


## longline

lln.catch <-
    lina %>%
    melt(id.vars=names(lina)[1:18]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.lln',
                   lln.catch)

## pelagic trawl

pel.catch <-
    flotv %>%
    melt(id.vars=names(flotv)[1:31]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.pel',
                   pel.catch)

## nephrop trawl

npt.catch <-
    humar %>%
    melt(id.vars=names(humar)[1:32]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.npt',
                   npt.catch)

## capelin fleet

pse.catch <-
    lodna %>%
    melt(id.vars=names(lodna)[1:32]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.pse',
                   pse.catch)

## gillnets

gil.catch <-
    net %>%
    melt(id.vars=names(net)[1:20]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.gil',
                   gil.catch)

## handline

hln.catch <-
    handf %>%
    melt(id.vars=names(handf)[1:18]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.hln',
                   hln.catch)

## herring

her.catch <-
    sild %>%
    melt(id.vars=names(sild)[1:32]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.her',
                   her.catch)

## shrimp

pra.catch <-
    raekja %>%
    melt(id.vars=names(raekja)[1:32]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.pra',
                   pra.catch)

## danish seine

dse.catch <-
    dragnot %>%
    melt(id.vars=names(dragnot)[1:20]) %>%
    filter(value >0) %>%
    data.table %>%
    left_join(Log.key) %>%
    filter(!is.na(species) & (10*reitur+smareitur) %in% reitmapping$GRIDCELL) %>%
    left_join(mapping,copy=TRUE) %>%
    mutate(count = 1000*value,
           areacell = 10*reitur + smareitur,
           year = ar,
           month = veman,
           sampling_type = 'LOG') %>%
    select(sampling_type,year,month,gear,areacell,count,species) 


mfdb_import_survey(mdb,
                   data_source = 'logbooks.dse',
                   dse.catch)

