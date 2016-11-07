#Este algoritmo é forçado um centroide inicial
library(foreach)
setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos")

#Quantidade de Clusters
k = 2
#threshold
th = 0.001
#constante m
m = 2
#constante n
n = 2
#constante a
a = 1
#constante b
b = 1


pdf("PFCM.pdf")

#obtendo o conjunto de dados de treinamento
mydata = read.csv("dadosGerados_PCM_Artigo.csv")

#Quantidade de atributos no dataset
tam = nrow(mydata)

#Quantidade de dimencoes para dataset ja classificado
dim = length(mydata) - 1

#Passando os dados para a variável input
input <- mydata[,1:dim]

#### Obtendo os maiores e menores valores nas dimenções para gerar os clusters aleatoriamente####
#Encontrando os valores mais altos da tabela
max <- foreach(n = 1:dim, .combine = cbind) %do% max(input[,n])
#Encontrando os valores mais baixos da tabela
min <- foreach(n = 1:dim, .combine = cbind) %do% min(input[,n])

X = input


#Normalizando a entrada
#normalizar = function(x){(x-min(x))/(max(x)-min(x))}
#y = unlist(X)
#y = matrix(ncol = dim, y)
#X = normalizar(y)



#Criando matriz de coeficiente para cada entrada em relacao a cada cluster
U = matrix(ncol = k, nrow = tam, runif(k*tam,0,1)) # where K = 1

V = matrix(ncol = dim, nrow = k, 0)

for(i in 1:k){
  for(j in 1:dim){
    sum1 = 0
    sum2 = 0
    for(l in 1:tam){
      sum1 = sum1 + (U[l,i] * U[l,i]) * X[l,j]
      sum2 = sum2 + (U[l,i] * U[l,i])
    }
    V[i,j] = (sum1/sum2)
  }
}

#Encontrando os valores de pesos para cada cluster
for(i in 1:tam){
  for(j in 1:k){
    sum = 0
    for(l in 1:k){
      #Calculando Distancia Euclidiana
      distE1 = 0
      distE2 = 0
      for(d in 1:dim){
        distE1 = distE1 + ((X[i,d] - V[j,d])*(X[i,d] - V[j,d]))
        distE2 = distE2 + ((X[i,d] - V[l,d])*(X[i,d] - V[l,d]))
      }
      sum = sum + ((distE1/distE2) * (distE1/distE2))
    }
    U[i,j] = 1/sum
  }
}

#Encontrando dij^2 para encontrar ni posteriormente
#Fazendo distancia euclidiana

dist = matrix(ncol = k, nrow = tam, 0)

for(i in 1:tam){
  for(j in 1:k){
    res = 0
    for(l in 1:dim){
      res = res + ((X[i,l] - V[j,l])^2)
    }
    dist[i,j] = res
  }
}

#Estimando ni
# ni = K *(sum uij^m * dij^2)/(sum uij^m)
ni = matrix(ncol = k, 0)

for(i in 1:k){
  sum = 0
  for(j in 1:tam){
    sum = sum + (U[j,i]^m * dist[j,i])
  }
  sum2 = 0
  for(j in 1:tam){
    sum2 = sum2 + U[j,i]^m
  }
  
  ni[1,i] = sum/sum2
}

#Criando matriz de coeficiente para cada entrada em relacao a cada cluster
w = matrix(ncol = k, nrow = tam, 0)

#Encontrando os valores de pesos para cada cluster
for(i in 1:tam){
  for(j in 1:k){
    sum = 0
    for(l in 1:k){
      #Calculando Distancia Euclidiana
      distE1 = 0
      distE2 = 0
      for(d in 1:dim){
        distE1 = distE1 + ((X[i,d] - V[j,d])*(X[i,d] - V[j,d]))
        distE2 = distE2 + ((X[i,d] - V[l,d])*(X[i,d] - V[l,d]))
      }
      sum = sum + ((distE1/distE2) * (distE1/distE2))
    }
    w[i,j] = 1/sum
  }
}


#Fazendo plotar
dadosClassificados = X
dadosClassificados = cbind(dadosClassificados,0)
for(i in 1:tam){
  maior = 0
  for(j in 1:k){
    if(U[i,j] > maior){
      maior = U[i,j]
      indice = j
    }
  }
  dadosClassificados[i,3] = indice
}

CV = V
CV = cbind(V, k+1)
colnames(CV) <- c("X","Y", "V3")
colnames(dadosClassificados) <- c("X","Y", "V3")
dadosClassificados = rbind(dadosClassificados,CV)
family <- as.factor(dadosClassificados[,3])
#plot(dadosClassificados$X, dadosClassificados$Y, pch = c(15, 16, 17, 3)[family], col=family)
plot(dadosClassificados[,'X'], dadosClassificados[,'Y'], pch = c(15, 16, 17, 3)[family], col=family)

Vant = V

for(i in 1:k){
  for(j in 1:dim){
    sum1 = 0
    sum2 = 0
    for(l in 1:tam){
      sum1 = sum1 + (((a*w[l,i] * w[l,i]) + (b*U[l,i] * U[l,i]))* X[l,j])
      sum2 = sum2 + ((a*w[l,i] * w[l,i]) + (b*U[l,i] * U[l,i]))
    }
    V[i,j] = (sum1/sum2)
  }
}


erro = 1
contador = 0
contadorMax = 100
while((erro > th) && (contador <contadorMax)){
  contador = contador + 1
  
  #Recalculando Distancias
  for(i in 1:tam){
    for(j in 1:k){
      res = 0
      for(l in 1:dim){
        res = res + ((X[i,l] - V[j,l])*(X[i,l] - V[j,l]))
      }
      dist[i,j] = res
    }
  }
  
  #Salvando u anterior
  Uant = U
  
  #Atualizando U
  for(i in 1:tam){
    for(j in 1:k){
      resp = 1 + (dist[i,j]/ni[1,j]) #A distancia já está ao quadrado
      U[i,j] = 1/resp
    }
  }
  
  
  #Salvando valores de pesos anteriores para recalcular pesos
  wAnt = w
  
  #Encontrando os novos valores de pesos para cada cluster
  for(i in 1:tam){
    for(j in 1:k){
      sum = 0
      for(l in 1:k){
        #Calculando Distancia Euclidiana
        distE1 = 0
        distE2 = 0
        for(d in 1:dim){
          distE1 = distE1 + ((X[i,d] - V[j,d])*(X[i,d] - V[j,d]))
          distE2 = distE2 + ((X[i,d] - V[l,d])*(X[i,d] - V[l,d]))
        }
        sum = sum + ((distE1/distE2) * (distE1/distE2))
      }
      w[i,j] = 1/sum
    }
  }
  
  #Atualizando os centroides
  for(i in 1:k){
    for(j in 1:dim){
      sum1 = 0
      sum2 = 0
      for(l in 1:tam){
        sum1 = sum1 + (((a*w[l,i] * w[l,i]) + (b*U[l,i] * U[l,i]))* X[l,j])
        sum2 = sum2 + ((a*w[l,i] * w[l,i]) + (b*U[l,i] * U[l,i]))
      }
      V[i,j] = (sum1/sum2)
    }
  }
  
  #Verificando a condicao de parada(diferenca entre pesos menor que threshold)
  erro = 0
  for(i in 1:tam){
    sum = 0
    for(j in 1:k){
      sum = sum + abs(U[i,j] - Uant[i,j])
    }
    erro = erro + sum
  }
  
  #Fazendo plotar
  dadosClassificados = X
  dadosClassificados = cbind(dadosClassificados,0)
  for(i in 1:tam){
    maior = 0
    for(j in 1:k){
      if(U[i,j] > maior){
        maior = U[i,j]
        indice = j
      }
    }
    dadosClassificados[i,3] = indice
  }
  
  #outfile < sprint("plots%contador.pdf",contador)
  
  CV = cbind(V, k+1)
  colnames(CV) <- c("X","Y", "V3")
  colnames(dadosClassificados) <- c("X","Y", "V3")
  dadosClassificados = rbind(dadosClassificados,CV)
  family <- as.factor(dadosClassificados[,3])
  #plot(dadosClassificados$X, dadosClassificados$Y, pch = c(15, 16, 17, 3)[family], col=family)
  plot(dadosClassificados[,'X'], dadosClassificados[,'Y'], pch = c(15, 16, 17, 3)[family], col=family)
  
}

dev.off()
