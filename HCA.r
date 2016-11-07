
library(foreach)

#setando o local de trabalho
setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos")

#obtendo o conjunto de dados de treinamento
mydata = read.csv("dadosGerados_PCM_HCA_150.csv")

#Quantidade de atributos no dataset
tam = nrow(mydata)
N = tam
k= 2

#Quantidade de dimencoes para dataset ja classificado
dim = length(mydata) - 1

#Passando os dados para a variável input
input <- mydata[,1:dim]

for(i in 1:20){
  input[(tam+i),1] = runif(1,-40,40)
  input[(tam+i),2] = runif(1,-40,40)
}

tam = nrow(input)
N = tam

m = 1

X = input
plot(X)


#distancia euclidiana
dist = matrix(ncol = tam, nrow = tam, 0)

for(i in 1:tam){
  for(j in 1:tam){
    if(i>j){
      res = 0
      for(l in 1:dim){
        res = res + ((X[i,l] - X[j,l])*(X[i,l] - X[j,l]))
      }
      dist[i,j] = res
      dist[j,i] = res
    }
  }
}

#h = as.data.frame.list(N)
#h$x = vector(length = 100)
h = as.data.frame.list(matrix(ncol = 2, nrow = N, 0))

colnames(dist) = c(1:N)
#colnames(dist)
#paste(colnames(dist)[1],colnames(dist)[2], sep = ",")

#colnames(dist)[N] = paste(colnames(dist)[1],colnames(dist)[2], sep = ",")

while(N > k){
  
  #Retirando os 0 das matrix
  a = max(dist)
  for(i in 1:N){
    dist[i,i] = a
  }
  
  a = min(dist)
  #procurando a menor distancia entre clusters e salvando o indice
  for(i in 1:N){
    for(j in 1:N){
      if(i>j){
        if(a == dist[i,j]){
          indiceI = i
          indiceJ = j
        }
      }
    }
  }
  
  
  vetor1 = dist[indiceI,]
  vetor2 = dist[indiceJ,]
  
  N = N - 1
  
  
  novaDist = matrix(ncol = N, nrow = N, 0)
  #colnames(novaDist) = colnames(dist)
  p = 1
  for(i in 1:(N+1)){
    q = 1
    if(i == indiceI || i == indiceJ){}
    else{
      for(j in 1:(N+1)){
        if(i == j){
          q = q + 1
          #novaDist[i,j] = 0
        }
        else if(j == indiceI || j == indiceJ){}
        else{
          novaDist[p,q] = dist[i,j]
          q = q + 1
        }
      }
      p = p + 1
    }
  }
  
  
  vetorMin = vector(length = N)
  p = 1
  for(i in 1:(N+1)){
    if(i != indiceI && i != indiceJ){
      vetorMin[p] = min(vetor1[i],vetor2[i])
      p = p + 1
    }
  }
  
  
  novaDist[N,] = vetorMin
  novaDist[,N] = vetorMin
  
  
  #colnames(novaDist)[N] = paste(colnames(h[m-1],colnames(h)[m], sep = ",")

  #h[m,1] = indiceI
  #h[m,2] = indiceJ
  
  #GAMBIARRA PARA MUDAR OS NOMES DAS COLUNAS
  aux = as.data.frame.list(matrix(ncol = 2, nrow = N, 0))
  e = 1
  for(i in 1:(N+1)){
    if(i != indiceI && i != indiceJ){
      aux[e] = colnames(dist)[i]
      e = e + 1
    }
  }
  
  
  h[m] = colnames(dist)[indiceI]
  m = m + 1
  h[m] = colnames(dist)[indiceJ]
  colnames(novaDist)[N] = paste(h[m-1],h[m], sep = ",")
  
  for(i in 1:(N-1)){
    colnames(novaDist)[i] = paste(aux[i])
  }
  
  m = m + 1
  
  dist = novaDist
  
  
}

Y = cbind(X,0)

for(i in 1:k){
  a = matrix()
  a[,1] = unlist(colnames(novaDist)[i])
  a = unlist(strsplit(a, split=","))
  
  for(j in 1:length(a)){
    Y[a[j],(dim+1)] = i
  }
}



#Fazendo plotar



colnames(Y) <- c("X","Y", "V3")
col.list <- c("blue","green","black")
palette(col.list)
family <- as.factor(Y[,3])
plot(Y[,'X'], Y[,'Y'], pch = c(16, 15, 17, 3)[family], col=family)


#a = dist(X)
#hclust(d, method = "complete", members = NULL)

#install.packages("stats")
