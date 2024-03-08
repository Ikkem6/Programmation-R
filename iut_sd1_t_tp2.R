df=read.csv("fao.csv", header = TRUE, dec = ",", sep = ";")
View(donnees)

nrow(df)
summary(df)
mean(df$Dispo_alim)
sum(df$Population,na.rm=TRUE)
sd(df$Import_viande)
sd(df$Export_viande,na.rm=TRUE)
median(df$Prod_viande,na.rm=TRUE)
quantile(df$Dispo_alim,probs=c(0,0.25,0.5,0.75,1))
quantile(df$Import_viande,probs=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

rang1=order(df$Population)
order1=df[rang1,]
req1=head(order1,n=5)
View(req1)

reqq1=head(df[order(df$Population),],n=5)
View(reqq1)


rang2=order(df$Population,decreasing = TRUE)
order2=df[rang2,]
req2=head(order2,n=5)
View(req2)


rang3=order(df$Prod_viande,decreasing = TRUE)
order3=df[rang3,]
req3=head(order3,n=5)
View(req3)


rang4=order(df$Import_viande,decreasing = TRUE)
order4=df[rang4,]
req4=head(order4,n=5)
View(req4)

req5=subset(df,df$Dispo_alim>=2300)
nrow(req5)

req6=subset(df,df$Dispo_alim>3200 & df$Import_viande>=1000)
nrow(req6)

req7=subset(df,df$Nom %in% c("France","Belgique"))
nrow(req7)


df$part_export=df$Export_viande/df$Prod_viande

df$dispo_alim_pays=df$Dispo_alim*df$Population

View(df)

write.table(df, file = "Tp2Export.csv", sep = ";", row.names = FALSE)

somme1=sum(df$dispo_alim_pays,na.rm=TRUE)
print(somme1)
calcul1=somme1/2300
print(calcul1)

plot(df$Prod_viande,df$Export_viande, main="Production de viande en fonction de l'exportation")

r1=cor(df$Prod_viande,df$Export_viande,use = "complete.obs")
print(r1)

mc1=cor(df[,-1],use = "complete.obs")
mcr1=round(mc1,2)
print(mcr1)

install.packages("corrplot")
library(corrplot)

corrplot(mcr1, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45, 
         diag=FALSE )
