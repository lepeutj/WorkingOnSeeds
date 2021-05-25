library(ROCR)
library(car)
library(nnet)
library(DiscriMiner)
library(class)
library(rpart.plot)
library(rpart)
library(RcmdrMisc)


seed<-read.table("seeds_dataset.txt")

colnames(seed)=c("Aire","Perimetre","Compacité","Long-Noyau","Larg-Noyau","Coef-Asymetrie","Long-Sillon","Variété")

#
seed=cbind(seed,seed[,8])

seed$Variété[seed$Variété==1]="Kama"
seed$Variété[seed$Variété==2]="Rosa"
seed$Variété[seed$Variété==3]="Canadian"

  
  
hist(seed[1:70,1])
plot(density(seed[1:70,1]))
shapiro.test(seed[1:70,1])
  
hist(seed[71:140,1])
plot(density(seed[71:140,1]))
  
hist(seed[141:210,1])
plot(density(seed[141:210,1]))
####### Tentative de simulation données (sans algo accept/rejet)
tk=NULL
testKama<-function(n,data){
  for (i in 1:7){ tk=cbind(tk,round(runif(n,min(data[1:70,i]),max(data[1:70,i])),3))}
  return(cbind(as.numeric(tk),"Kama"))
}

  testRosa<-function(n,data){
    for (i in 1:7){ tk=cbind(tk,runif(n,min(data[71:140,i]),max(data[1:70,i])))}
    return(cbind(as.numeric(tk),"Rosa"))
  }
  
  
  testCana<-function(n,data){
    for (i in 1:7){ tk=cbind(tk,runif(n,min(data[141,210,i]),max(data[1:70,i])))}
    return(cbind(as.numeric(tk),"Canadian"))
  }
  
T=testKama(1000,seed)
hist(as.numeric(T[,1])
####### Présentation des données

as.factor(seed$Variété)
  
#Boxplot variables % variétés
par(mfrow=c(2,4))
for (i in 1:7) {boxplot(seed[,i]~seed$Variété,xlab="Variété",main=names(seed)[i])}


par(mfrow=c(2,3))
for (i in 2:7) {plot(seed[,1],seed[,i],ylab="",xlab="Aire",main=names(seed)[i])}
#
summary(lm(seed[,1]~seed[,5]))

hist(seed[,1])
lines(density(seed[,1]))
  # On retire Périmetre, Longueur/Largeur noyau
colnames(seed)
new.seed=seed[,-c(2,4,5)]
  
#Création groupe apprentissage / validation
seedTR=rbind(new.seed[1:35,],new.seed[71:106,],new.seed[141:175,])
seedPR=new.seed[-as.numeric(rownames(seedTR)),]

####### Analyse Factorielle
adf<-desDA(seedTR[,c(-5,-6)],seedTR[,5])
layout(1)
plot(adf$scores[,1],adf$scores[,2],col=seedTR[,6],xlab="Axe 1",ylab="Axe 2")
abline(h=0,v=0)
legend("topleft",c("Kama","Rosa","Canadian"),pch=1,col=c(1,2,3))


#####
Abs=adf$scores[,1]
Ord=adf$scores[,2]
#Arbre sur les composantes factorielles
cart=rpart(seedTR[,5]~Abs+Ord)
rpart.plot(cart)

#Prédiction a partir de l'arbre sur échantillon d'entrainement


###Proj 
Proj= as.numeric(seedPR[1:4,])%*%adf$discrivar[2:5,]
###
dim(adf$discrivar)
pred.tree=predict(cart,seedPR)
pred=apply(pred.tree,1,which.max)
table(pred,seedPR[,5])

#####
#Régression logistique 1er axe, création de deux groupes
y=seedTR$Variété=="Rosa"
RegLog1<-glm(y~adf$scores[,1]-1,family=binomial(link="logit"))

summary(RegLog1)  
#Impossible de faire converger l'algorithme
res<-stepwise(RegLog1,direction="forward",criterion="AIC")
summary(res)

#Projection des prévisons sur l'échantillon d'apprentissage
plot(seedTR[,9]==2~adf$scores[,1])
lines(seq(-5,5,0.1),exp(coef(RegLog1)[1]+coef(RegLog1)[2]*seq(-5,5,0.1))/(1+exp(coef(RegLog1)[1]+coef(RegLog1)[2]*seq(-5,5,0.1))),col="red")
#D'ou la non convergence de l'algorithme

#Prédiction 
plot(predict(RegLog1,seedPR),col=seedPR[,9])
pred.reglog=predict(RegLog,seedPR)

table(pred.reglog>0,seedPR[,8]=="Rosa")
      
      
##### Régression Multinomiale
Reg<-multinom(seedTR[,5]~adf$scores[,1]+adf$scores[,2])
summary(Reg)

Pred=predict(Reg,seedPR)
table(Pred,seedPR[,5])

######




###### KNN 
#Choix d'un nombre "optimal" par validation-croisée
pourc=NULL
for(i in 1:104) {
  t<-knn.cv(seedTR[,c(-5,-6)],seedTR[,5],k=i,prob=FALSE)
  pourc=c(pourc,sum(diag(table(t,seedTR[,5])))/sum(table(t,seedTR[,5])))
}

#Affichage
plot(1:length(pourc),pourc,type="l",xlab="k",ylab="Pourcentage de bien classés")
abline(v=which.max(pourc),col="red",lty=2)

#Entrainement sur l'échantillon de validation
knn.rslt<-knn(seedTR[,c(-5,-6)], seedPR[,c(-5,-6)], seedTR[,5], k =13, prob=TRUE)

#Résultats
table(knn.rslt,seedPR[,5])

#####################################################
#####################################################

new.seed<-scale(new.seed[,-c(5,6)],center=TRUE,scale=TRUE)
res<-kmeans(new.seed[,c(-5,-6)],centers=3,nstart=20)

table(T,new.seed[,5])

