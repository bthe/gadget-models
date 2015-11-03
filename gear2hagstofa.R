library(Rgadget)
library(Logbooks)
library(fjolst)
library(mfdb)
mapping <- read.table('mfdb-setup/mapping.txt',header=TRUE) %>%
  select(veidarf=veidarfaeri,id=gear)

## look at catches by gear type and vessel size
tmp <-
landedcatch %>%
  filter(ar > 2005, fteg==1) %>% 
  left_join(mapping) %>%
  left_join(mfdb::gear) %>% 
  left_join(skipaskra) %>%
  ###  Bátar <10 brl., Bátar 10-200 brl., Bátar >200, Ísfiskitogarar, Frystitogarar, Uppsjávarfrystiskip.
  mutate(ship.class=cut(brl,c(0,10,200,max(brl,na.rm=TRUE)+1)))
  
flokkun <- 
  tmp %>% 
  group_by(ar,name,description,ship.class) %>%
  summarise(landed.catch = sum(magn))

flokkun %>% write.csv('flokkun.csv')
