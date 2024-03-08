setwd(dir="L:/BUT/SD/Promo 2023/mcoprak/Programmation stat/TD/TD2/dataset")
getwd()

bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")

dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)

summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

plot(x=drivers$Weight,y=drivers$Acceleration,main="Accélération selon le poids")

cor(x=drivers$Weight,y=drivers$Acceleration)

coefco=cov(x=drivers$Weight,y=drivers$Acceleration)/(sd(drivers$Weight)*sd(drivers$Acceleration))

coefdet=(cor(x=drivers$Weight,y=drivers$Acceleration))^2


matriceCor = cor(drivers[ ,- 1])
matriceCor = round(matriceCor , 2)
View(matriceCor)
 
install.packages("corrplot")


library(corrplot)
corrplot(matriceCor,method="circle")

matriceCor1 = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=FALSE )


matriceCor2 = round(cor(gliders[ , - 1]),1)
corrplot(matriceCor2, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=FALSE )
#ecart-type nul donc ne marche pas 


matriceCor3 = round(cor(tires[ , - 1]),1)
corrplot(matriceCor3, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=FALSE )

resultat=drivers[,c("Driver","Weight")]
View(resultat)

resultat=drivers[1:10,c("Driver","Acceleration")]
View(resultat)

resultat=drivers[,-c(5,7,9)]
View(resultat)

resultat=drivers[ ,-c(0,2,1)]
View(resultat)

resultat=drivers[,c("Driver","Acceleration","Weight")]
View(resultat)


resultat=drivers[c(3,12,32),]
View(resultat)

resultat=drivers[c(32,3,12),]
View(resultat)

rang=order(drivers$Weight)
resultat=drivers[rang,c("Driver","Weight")]
View(resultat)

rang=order(drivers$Acceleration)
resultat=drivers[rang,c("Driver","Acceleration")]
View(resultat)

rang=order(drivers$Weight)
resultat=drivers[rang,c("Driver","Weight")]
View(resultat)

rang=order(drivers$Weight,drivers$Acceleration, decreasing=c(TRUE,FALSE))
resultat=drivers[rang,c("Driver","Acceleration","Weight")]
View(resultat)




