install.packages("readxl")

setwd(dir="L:/BUT/SD/Promo 2023/mcoprak/Programmation stat/TD/TD3/dataset2")
getwd()

dfpokemon=readxl::read_excel(path="pokemon.xlsx",sheet ="pokemon")
View(dfpokemon)

dim(dfpokemon)
ncol(dfpokemon)
nrow(dfpokemon)

summary(dfpokemon)


dfpokemon$generation=as.factor(dfpokemon$generation)
dfpokemon$is_legendary=as.factor(dfpokemon$is_legendary)
dfpokemon$type=as.factor(dfpokemon$type)

summary(dfpokemon)

dfpokemon$attack_group=ifelse(test=dfpokemon$attack>=median(dfpokemon$attack), yes="attack +", no="attack -")
dfpokemon$attack_group=as.factor(dfpokemon$attack_group)

summary(dfpokemon)
  
dfpokemon$water_fire=ifelse(test= dfpokemon$type=="water" | dfpokemon$type=="fire", yes="water_fire",no="")
dfpokemon$water_fire=as.factor(dfpokemon$water_fire)

summary(dfpokemon)

dfpokemon$best=ifelse(test=dfpokemon$attack>quantile(dfpokemon$attack,probs=0.75) & dfpokemon$defense>quantile(dfpokemon$defense,probs=0.75) & dfpokemon$speed>quantile(dfpokemon$speed,probs=0.75),yes="yes",no="no")
dfpokemon$best=as.factor(dfpokemon$best)

summary(dfpokemon)

requete1=subset(dfpokemon,is.na(dfpokemon$weight_kg))
View(requete1)

requete2=subset(dfpokemon, !is.na(dfpokemon$weight_kg))
View(requete2)

dfpokemon$weight_kgNA=ifelse(test=is.na(dfpokemon$weight_kg),yes=median(dfpokemon$weight_kg,na.rm=TRUE),no=dfpokemon$weight_kg)
dfpokemon$height_mNA=ifelse(test=is.na(dfpokemon$height_m),yes=median(dfpokemon$height_m,na.rm=TRUE),no=dfpokemon$height_m)

dfpokemon$weight_group=cut(dfpokemon$weight_kg,breaks=3,labels=c("léger","moyen","lourd"))

dfpokemon$height_m_group=cut(dfpokemon$height_m,breaks=c(0,1,2,3,max(dfpokemon$height_m, na.rm=TRUE))) 

dfpokemon$defense_group=cut(dfpokemon$defense,breaks=quantile(dfpokemon$defense,na.rm=TRUE),include.lowest=TRUE)

aggregate(attack ~ type, data=dfpokemon, FUN=function(x) mean(x))
aggregate(attack ~ generation + type, data=dfpokemon, FUN=function(x) median(x))
aggregate(pokedex_number ~ type, data=dfpokemon, FUN=function(x) length(x))
aggregate(speed ~ generation + type, data=dfpokemon, FUN=function(x) c(moy=mean(x),med=median(x),eff=length(x)))


