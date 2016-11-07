#setando o local de trabalho
setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos")

#Quantidade de Clusters
k = 4

#obtendo o conjunto de dados de treinamento
mydata = read.csv("dadosGerados25_S.csv")

#Quantidade de atributos no dataset
tam = length(mydata[,1])

#Quantidade de dimenções
dim = length(mydata[1,]) - 1

#Lendo os dados
input <- mydata[,1:dim]
#c = input[1:b,]

#Obtendo valor maior para gerar Rand()
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


#Algorithmic steps for k-means clustering

#Let  X = {x1,x2,x3,……..,xn} be the set of data points and V = {v1,v2,…….,vc} be the set of centers.
#1) Randomly select ‘c’ cluster centers.

X = input

#Criando aleatoriamente os K Clusters
#Usar Foreach e olhar l Apply --------------------------------------------------------------------------------
for(i in 1:k){
	for(j in 1:dim){
			clusters[i,j] <- runif(1,1,max[j])
		}
	}

#Forçando clusters para verificação
#clusters[1,1] = 3
#clusters[1,2] = 3
#clusters[2,1] = 18
#clusters[2,2] = 18
#clusters[3,1] = 0
#clusters[3,2] = 19

V = clusters

vDist = 0

dist = 10
contador = 0
#while(dist>1){


for(ww in 1:10){
contador = contador + 1
#2) Calculate the distance between each data point and cluster centers.
# Distancia euclidiana: sqrt((px-qx)²+(py-qy)²)
#Calculando distancia Euclidiana

#Calculo a distancia com todos os Clusters. Terminando de calcular a distancia, pego a menor.
#incremento a qnt de componentes daquele cluster e coloco a posição do dado no vetor do cluster.


#Vetor que contem a distancia de cada ponto com cada cluster
sumDist = matrix(ncol = tam, nrow = k, 0)
	
#Calculando as distancias e armazenando no Vetor sumDist
for(i in 1:tam){
	for(l in 1:k){
		sum = 0
		#Calculando a distancia euclidiana para cada dimenção
		for(j in 1:dim){
				sum = sum + ((X[i,j] - V[l,j])*(X[i,j] - V[l,j]))
			}
		sumDist[l,i] = sqrt(sum)
		}
	}

#3) Assign the data point to the cluster center whose distance from the cluster center is minimum of all the cluster centers..

clusterPoints = matrix(ncol = tam, nrow = k, 0)

inst_cluster = matrix(ncol = tam, nrow = k, 0)

qtdClusterPoints = matrix(ncol = 1, nrow = k, 0)

for(i in 1:tam){
	menor = 1000 ##ARRUMAR GAMBIARRA---------------------------------------------------
	for(j in 1:k){
		if(menor > sumDist[j,i]){
			menor = sumDist[j,i]
			ind = j
		}
	}
	qtdClusterPoints[ind,1] = qtdClusterPoints[ind,1] + 1 
	#clusterPoints[ind,i] = i
	clusterPoints[ind,qtdClusterPoints[ind,1]] = i
}

#4) Recalculate the new cluster center using: Vi = (1/Ci) sum de J=1 até Ci de Xi
#where, ‘ci’ represents the number of data points in ith cluster.
#Nada otimizado
novoV = matrix(ncol = dim, nrow = k, 0)
for(i in 1:k){
	for(l in 1:dim){
		sum = 0
		for(j in 1:qtdClusterPoints[i,1]){
			#if(clusterPoints[i,j] != 0){
					sum = sum + X[clusterPoints[i,j],l] 
					#sum = sum + clusterPoints[i,j]
				#}
		}
		if(qtdClusterPoints[i,1] > 0){
			novoV[i,l] = (1/qtdClusterPoints[i,1]) * sum
			}
		}
	}

#5) Recalculate the distance between each data point and new obtained cluster centers.
novoSumDist = matrix(ncol = tam, nrow = k, 0)
for(i in 1:tam){
	for(l in 1:k){
		sum = 0
		#Calculando a distancia euclidiana para cada dimenção
		for(j in 1:dim){
				sum = sum + ((X[i,j] - novoV[l,j])*(X[i,j] - novoV[l,j]))
			}
		novoSumDist[l,i] = sqrt(sum)
		}
	}

#6) If no data point was reassigned then stop, otherwise repeat from step 3).
distVet = sumDist - novoSumDist

dist = 0
for(i in 1:tam){
	for(j in 1:k){
		dist = dist + sqrt(distVet[j,i] * distVet[j,i])
		}
	}
	
	vDist[contador] = dist

V = novoV
}




