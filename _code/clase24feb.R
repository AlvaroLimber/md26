rm(list=ls())
#install.packages("tidyverse")
library(tidyverse)
#####################
data("USArrests")
bd<-USArrests
names(bd)
bd<-bd %>% rename(murder=Murder,
              assault=Assault)
names(bd)[c(3, 4)]<-tolower(names(bd)[c(3, 4)])
## Generar un data frame con estados 
# con menos del 50% de población urbana
bd1<-bd %>% filter( urbanpop < 50)

bd %>% head() %>% View() 

xb<-mean(bd$murder)
sigma<-sd(bd$murder)

bd %>% mutate(
  mz = (murder-xb)/sigma,
  mz2 = scale(murder)
  ) %>% 
  head()

##crear un comando para la normalización
tnorm<-function(x){
  y<-(x-min(x))/(max(x)-min(x))
  return(y)
}
#########################################
bd<-bd %>% mutate(
  mz=scale(murder),
  az=scale(assault),
  uz=scale(urbanpop),
  rz=scale(rape),
  mn=tnorm(murder),
  an=tnorm(assault),
  un=tnorm(urbanpop),
  rn=tnorm(rape)
  )
#resumen
bd %>% summarise(mean(murder), 
                 mean(assault),
                 median(urbanpop),
                 sd(rape)
                 )
bd %>% summarise_all(sd)

bd %>% mutate(grupo4=ntile(assault,4)) %>% 
 select(assault, grupo4) %>%  head()

bd<-bd %>% mutate(grupo4=ntile(assault,4))

bd %>% group_by(grupo4) %>% 
  summarise(mean(murder))

cor(bd$murder, bd$assault)
plot(bd$murder, bd$assault)
library(corrplot)
#install.packages("corrplot")
corrplot(
  bd %>% 
    select(murder:rape) %>% 
           cor()
  )



