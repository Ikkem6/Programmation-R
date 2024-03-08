#Exercice 1

iris
class(iris)

nrow(iris)

ncol(iris)

colnames(iris)

summary(iris)

iris[, c("Sepal.Length","Species")] #(Toute les lignes de la colonne Sepal.Length et Species d'ou le fait on met d'abord rien dans le premier element apres la virgule)

iris[c(100,103,105),]

iris[c(50,100),]

m=mean(iris$Sepal.Length)

med=median(iris$Sepal.Width)

ec=sd(iris$Petal.Length)

q10=quantile(iris$Petal.Width,probs=(seq(0,1,0.1)))

#Exercice 2 


dfManga=read.csv("L:/BUT/SD/Promo 2023/mcoprak/Programmation/manga.csv",header=TRUE,sep=",",dec=".")
dfAnime=read.csv("L:/BUT/SD/Promo 2023/mcoprak/Programmation/anime.csv",header=TRUE,sep=",",dec=".")
class(dfManga)
class(dfAnime)

dim(dfManga)
dim(dfAnime)

m1=mean(dfManga$Score)
m2=mean(dfAnime$Score)

s1=sum(dfManga$Vote)
s2=sum(dfManga$Vote)

ec1=sd(dfManga$Score)
ec2=sd(dfAnime$Score)

d1=quantile(dfManga$Score,probs=(seq(0,1,0.1)))
d2=quantile(dfAnime$Score,probs=(seq(0,1,0.1)))

#Exercice 3

e1=subset(dfManga, Score>9)
e2=subset(dfManga, Vote>=200000)
e3=subset(dfManga, Vote>=200000 & Score>8)
e4=subset(dfManga, Vote>=7 & Vote<=8 )


t1=table(dfAnime$Rating)
print(t1)
length(t1)
tf1=prop.table(t1)

e5=subset(dfAnime, Rating=="R - 17+ (violence & profanity)")
nrow(e5)
e6=subset(dfAnime, Rating=="R - 17+ (violence & profanity)" & Score>8)
nrow(e6)
e7=subset(dfAnime, Rating !="R - 17+ (violence & profanity)")
nrow(e7)
e8=subset(dfAnime, Rating %in% c("PG - Children", "G - All Ages "))
nrow(e8)
e9=subset(dfAnime, !Rating %in% c("PG - Children", "G - All Ages "))
nrow(e9)
e10=subset(dfAnime, Score>9 | Vote>400000)
nrow(e10)

#Exercice 4

dfAnime=dfAnime[ , c("Title","Score","Vote","Ranked")]
dfManga=dfManga[ , c("Title","Score","Vote","Ranked")]

dfAnime$Type="Anime"
dfManga$Type="Manga"

dfConcat=rbind(dfManga,dfAnime)

write.table(dfConcat,file="L:/BUT/SD/Promo 2023/mcoprak/Programmation stat/ExportTp1.csv",sep=";",row.names=FALSE)
