setwd("C:/Users/Gerson/Documents")
library(ggplot2) 
library(caret)
library(e1071)
library(gridExtra)

###############FAZER SEM DAR PESO PARA TODO MUNDO
#dividir a pertinencia apenas para os que tem classificação, e dar zero para os que não tem...
#possivel formula : a^a / N

db2 = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")
db2 = db2[1:5000,]
#db = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_5000_SS_10.csv")
db = read.csv("AlgoritmosAntigos/DataSets/dadosGerados_FCM2_SS.csv")
db = db[,2:4]
nclasses = 3
N = nrow(db)
max_mic = 200

plot(db[,1:2])

teste = kmeans(db[,1:2], max_mic, iter.max = 100, nstart = 1)
teste2 = kmeans(db[,1:2], 3, iter.max = 100, nstart = 1)

plot(teste$centers)

centroides = NULL
centroides$centro = as.data.frame(teste$centers)
centroides$classe = NULL

#gambs para funcionar
centroides$classe = matrix(ncol = nclasses+1, nrow = max_mic, 0)


for(i in 1:N){
  #if(is.null(centroides$classe[[teste$cluster[i]]][(db[i,3]+1)])){
  #  centroides$classe[[teste$cluster[i]]][(db[i,3]+1)] = 0
  #}
  centroides$classe[teste$cluster[i],(db[i,3]+1)] = centroides$classe[teste$cluster[i],(db[i,3]+1)] + 1
}

pertC = matrix(ncol = nclasses, nrow = max_mic)
pertC2 = matrix(ncol = nclasses, nrow = max_mic)
#NOVO
for(i in 1:max_mic){
  for(j in 1:nclasses)
  pertC[i,j] = centroides$classe[i,(j+1)] / sum(centroides$classe[i,])
}
for(i in 1:max_mic){
  for(j in 1:nclasses)
    pertC2[i,j] = sqrt(centroides$classe[i,(j+1)]^2 / sum(centroides$classe[i,]))
}

#forcadasso
for(i in 1:max_mic){
  for(j in 1:nclasses)
    pertC2[i,j] = sqrt((((centroides$classe[i,(j+1)]+1)^3)-1) / sum(centroides$classe[i,]))
}
resultado = cbind(centroides$centro,pertC)
resultado = cbind(centroides$centro,pertC2)

#ANTIGO
#for(i in 1:max_mic){
#  for(j in 1:nclasses)
#  pertC[i,j] = centroides$classe[i,(j+1)] + (((sum(centroides$classe[i,])) - (sum(centroides$classe[i,2:(nclasses+1)])))/nclasses)
#}
#normalizando
#pertCN = matrix(ncol = nclasses, nrow = max_mic)
#for(i in 1:max_mic){
#  for(j in 1:nclasses)
#    pertCN[i,j] = pertC[i,j] / sum(pertC[i,])
#}
#Quadratico
#pertCN2 = matrix(ncol = nclasses, nrow = max_mic)
#for(i in 1:max_mic){
#  for(j in 1:nclasses)
#    pertCN2[i,j] = pertCN[i,j]^2 / sum(pertCN[i,]^2)
#}

#resultado = cbind(centroides$centro,pertCN2)

write.csv(resultado, paste("BarsGaussAN0_",max_mic,"_MiC.csv",sep = ""))









