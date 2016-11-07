
#setando o local de trabalho
setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos")

#Quantidade de Clusters
k = 3
#threshold
th = 0.01

#obtendo o conjunto de dados de treinamento
mydata = read.csv("dadosGerados_FCM2.csv")

#Quantidade de atributos no dataset
tam = nrow(mydata)

#Quantidade de dimencoes para dataset ja classificado
dim = length(mydata) - 1
#Quantidade de dimencoes para dataset nao classificado
#dim = length(mydata[1,])

#Passando os dados para a variável input
input <- mydata[,1:dim]

#Abrindo pdf para salvart plots
pdf("FCM_K3____.pdf")


#### Obtendo os maiores valores nas dimenções para gerar os clusters aleatoriamente####
#Setando uma matriz para armazenar o maior valor de cada dimencao
#Poderia ter uma para setar o menor valor também
max <- matrix(ncol = dim, nrow = 1, 0)

#Encontrando os valores mais altos da tabela
for(i in 1:tam){
	for(j in 1:dim)
		{
			if (input[i,j] > max[j]) {
				max[j] = input[i,j]
			}
		}
	}

clusters = matrix(ncol = dim, nrow = k, 0)

#Criando aleatoriamente os K Clusters
for(i in 1:k){
	for(j in 1:dim){
			clusters[i,j] <- runif(1,1,max[j])
		}
	}
	
V = clusters

X = input

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


dadosClassificados = X
for(i in 1:tam){
	maior = 0
	for(j in 1:k){
		if(w[i,j] > maior){
			maior = w[i,j]
			indice = j
		}
	}
	dadosClassificados[i,3] = indice
}


CV = cbind(V, k+1)
colnames(CV) <- c("X","Y", "V3")
dadosClassificados = rbind(dadosClassificados,CV)
family <- as.factor(dadosClassificados[,3])
col.list <- c("seagreen","green","blue","black")
palette(col.list)
plot(dadosClassificados$X, dadosClassificados$Y, pch = c(15, 16, 17, 3)[family], col=family)


erro = 1

contador = 0


while((erro > th)){ #&& (contador <10)){
contador = contador + 1
	#Calculando o centroide para cada entrada em relacao a cada cluster

	for(i in 1:k){
		for(j in 1:dim){
			sum1 = 0
			sum2 = 0
			for(l in 1:tam){
				sum1 = sum1 + (w[l,i] * w[l,i]) * X[l,j]
				sum2 = sum2 + (w[l,i] * w[l,i])
			}
			V[i,j] = (sum1/sum2)
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
	
	#Verificando a condicao de parada(diferenca entre pesos menor que threshold)
	erro = 0
	for(i in 1:tam){
		sum = 0
		for(j in 1:k){
			sum = sum + abs(w[i,j] - wAnt[i,j])
		}
		erro = erro + sum
	}
	
	#Fazendo plotar
dadosClassificados = X
for(i in 1:tam){
	maior = 0
	for(j in 1:k){
		if(w[i,j] > maior){
			maior = w[i,j]
			indice = j
		}
	}
	dadosClassificados[i,3] = indice
}

#outfile < sprint("plots%contador.pdf",contador)

CV = cbind(V, k+1)
colnames(CV) <- c("X","Y", "V3")
dadosClassificados = rbind(dadosClassificados,CV)
family <- as.factor(dadosClassificados[,3])
plot(dadosClassificados$X, dadosClassificados$Y, pch = c(15, 16, 17, 3)[family], col=family)

	
}



#Fazendo pela função cmeans do r
library(e1071)
cm = cmeans(X, 3, 100, verbose = TRUE, dist = "euclidean", method = "cmeans", m = 2)

b = cm$cluster
dadosClassificados2 = X
dadosClassificados2[,3] = b
CV2 = cbind(cm$centers, k+1)
colnames(CV2) <- c("X","Y", "V3")
dadosClassificados2 = rbind(dadosClassificados2,CV2)
family <- as.factor(dadosClassificados2[,3])
plot(dadosClassificados2$X, dadosClassificados2$Y, pch = c(15, 16, 17, 3)[family], col=family)

dev.off()

write.csv(w, file = "Matriz_U.csv")
