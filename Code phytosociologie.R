setwd(dir="/home/charlotte/Documents/L3/S6")

#penser ? retirer les lignes o? coeff=="+"

Releves=read.csv2("PhytosocioTables.csv",header=TRUE,sep=",",dec=".")

View(Releves)

#partitionner et pr?parer les donn?es

A=split.data.frame(Releves,Releves$Identifiant.releve)

for (j in 1:length(A)){
  for (i in 4:32){A[[j]][,i]=as.numeric(as.character(A[[j]][,i]))}
}

totalcoeff=NULL
for (j in 1:length(A)){
  for (i in 5:32){totalcoeff=c(totalcoeff,sum(A[[j]]$Coeff*A[[j]][,i],na.rm=T))}
}

tablecoeff=matrix(totalcoeff,nrow=length(A),ncol=length(totalcoeff)/length(A),byrow=TRUE)
colnames(tablecoeff)=c(colnames(A[[1]][,5:32]))
rownames(tablecoeff)=c(names(A))

PtableDucerf=NULL
PtableEllen=NULL
totalDucerf=NULL
totalEllen=NULL
totalCom=NULL

#visualiser les donn?es Ducerf et Ellenberg

for (i in 1:length(A)) {
  if(is.na(tablecoeff[i,])) next
  if(sum(tablecoeff[i,])==0) next
  PtableDucerf=tablecoeff[i,1:17]*50/sum(A[[i]]$Coeff)
  totalDucerf=c(totalDucerf,PtableDucerf)
  PtableEllen=tablecoeff[i,18:27]/sum(A[[i]]$Coeff)
  PtableEllen=PtableEllen-c(4.5,4.5,4.5,4.5,6,4.5,4.5,0,4.5,4.5)
  totalEllen=c(totalEllen,PtableEllen)
  totalCom=c(totalCom,tablecoeff[i,28]*100/sum(A[[i]]$Coeff))
  barplot(t(PtableDucerf),col=c("darkolivegreen3"),main=A[[i]][1,1],las=2,beside=F, ylim=c(-100,100))
  barplot(t(PtableEllen),col=c("darkolivegreen3"),main=A[[i]][1,1],las=2,beside=F, ylim=c(-6,6))}

#visualiser et comparer les donn?es comestibilit?

tableCom=matrix(totalCom,nrow=length(A),ncol=1,byrow=TRUE)
colnames(tableCom)=c(colnames(A[[1]][,32]))
rownames(tableCom)=c(names(A))
barplot(t(tableCom), ylim=c(0,100), col="darkolivegreen4")

View(tableCom)
#avoir les valeurs exactes

tableDucerf=matrix(totalDucerf,nrow=length(A),ncol=length(totalDucerf)/length(A),byrow=TRUE)
colnames(tableDucerf)=c(colnames(A[[1]][,5:21]))
rownames(tableDucerf)=c(names(A))
tableEllen=matrix(totalEllen,nrow=length(A),ncol=length(totalEllen)/length(A),byrow=TRUE)
colnames(tableEllen)=c(colnames(A[[1]][,22:31]))
rownames(tableEllen)=c(names(A))

View(tableDucerf)
View(tableEllen)
View(tableCom)

#####ACP test#####
library(ade4)
library(fpc)
library(stats)
library(rgl)
Mailles=read.csv2("TableauFinal2.csv",header=TRUE,sep=",",dec=".")
for (i in 41:50){Mailles[,i]=as.numeric(as.character(Mailles[,i]))}
for (i in 25:40){Mailles[,i]=as.numeric(as.character(Mailles[,i]))}
Y3=na.omit(Mailles[,25:50])
acp=dudi.pca(Y3, scannf=F, nf=3)
biplot(acp)
par(mfrow = c(1,2))
s.corcircle(acp$co)
s.corcircle(acp$co,2,3)
