ssfcm_pedrycz <- function(db, V = NULL, fuzzy.matrix = NULL , alpha = 0.99, distance = "mahalanobis", th = 0.01, iter.max = 200){
  #source("SSFCM_utils.R")
  
  #cat("\n Variavel Values: ")
  #cat("\n V: ",typeof(V))
  #cat("\n Fuzzy: ",typeof(fuzzy.matrix))
  #cat("\n Alpha: ",alpha)
  #cat("\n Distance: ",distance)
  #cat("\n Threshold: ",th)
  #cat("\n Iter.max: ",iter.max)
  
  #dimens�o da base de dados e seu tamanho
  dim = ncol(db)-1
  N = nrow(db)  
  
  #Renomeando as colunas
  colnames(db) = c(1:dim,"class")
  
  #Encontrando apenas as instancias rotuladas
  b = db$class
  aux = b > 0
  b2 = b
  b2[aux] = 1
  
  #Encontrando a quantidade de classes
  rotulados = as.factor(db$class[aux])
  classes = as.numeric(names(table(rotulados)))
  nclasses = length(classes)
  
  #Ciando a matrix fuzzy caso n�o haja
  if(is.null(fuzzy.matrix)){
    fuzzy.matrix = matrix(ncol = nclasses, nrow = N, 0)
    for(i in 1:N){
      if(aux[i]){
        fuzzy.matrix[i, b[i]] = 1
      }
    }
  }
  
  #criando os prototipos caso n�o 
  if(is.null(V)){
    ##Encontrando os chutes iniciais do centroide 
    #Pois eu mesclei cluster com classe, e ele estava procurando a mais perto, 
    #portanto resolvi fazer a media dos pontos classificados como chute inicial do centroides (ARRUMAR DEPOIS)
    V = matrix(ncol = dim, nrow = nclasses, sample(0:130, size=nclasses*dim, replace=TRUE))
    
    #Fazendo na m�o, depois arrumo
    lista = NULL
    for(i in 1:nclasses){
      t = db[,dim+1] == i
      lista[[i]] = db[t,1:dim]
    }
    V = matrix(ncol = dim, nrow = nclasses)
    for(i in 1:nclasses){
      V[i,] = colMeans(lista[[i]])
    }
  }
  
  #Criando uma matrix de pertinencia vazia
  U = matrix(ncol = nclasses, nrow = N, 0)
  
  #Inicializando a matriz de pertinencia U
  U = update.partition_matrix(db,V,alpha,fuzzy.matrix,distance)
  
  erro = 1 + th
  t = 0
  #Inicializando o algoritmo
  while(erro > th){
  #for(t in 1:iter.max){
    t = t + 1
    V = update.prototypes(U,db)
    
    U2 = update.partition_matrix(db,V,alpha,fuzzy.matrix,distance)
    
    #Verificando a condicao de parada(diferenca entre pesos menor que threshold)
    erro = 0
    for(i in 1:N){
      sum = 0
      for(j in 1:nclasses){
        sum = sum + abs(U2[i,j] - U[i,j])
      }
      erro = erro + sum
    }
    
    U = U2
    cat("\n iteracao: ",t)
    cat("\n Diferenca: ",erro)
    #cat("\n Th: ",th)
  }
  
  #Encontrando a classificacao
  classificados = db
  classificados[,dim+1] = 0
  size = vector(length = nclasses)
  
  #Fiz na "mao" arrumar depois
  for(i in 1:N){
    for(j in 1:nclasses){
      if(U[i,j] == max(U[i,])){
        classificados[i,(dim+1)] = j
        size[j] = size[j] + 1
      }
    }
  }
  
  #Preparando o retorno
  res = NULL
  res$centers = V
  res$size = size
  res$cluster = classificados[,dim+1]
  res$membership = U
  res$iter = t
  res$withinerror = "TODO"
  res$call = "TODO"
  
  return(res)
}

update.prototypes <- function(U,db){
  #A equa��o foi separada em duas etapas C = A/B
  
  #Inicializando valores
  N = nrow(db)
  k = ncol(U)
  dim = ncol(db) - 1
  
  #Primeira parte
  A = matrix(ncol = dim, nrow = k)
  for(i in 1:k){
    for(j in 1:dim){
      A[i,j] = sum((U[,i]^2) * db[,j])
    }
  }
  
  #Segunda Parte
  B = vector(length = k)
  for(i in 1:k){
    B[i] = sum(U[,i]^2)
  }
  C = A/B
  return(C)
}

#Fun��o que retorna a matriz de pertinencia utilizando distancia de mahanalobis
update.partition_matrix <- function(db, V, alpha,FMatrix, distance){
  #Necessario utilizar a fun��o dist.mahalanobis
  source("SSFCM_utils.R")
  #Encontrando valores principais
  k = nrow(V)
  dim = ncol(db)-1
  N = nrow(db)
  
  #Finding b vector
  b = db$class
  aux = b > 0
  b[aux] = 1
  
  #Renomeando as colunas do DB
  colnames(db) = c(1:dim,"class")
  
  #To simplificate, The FUZZY ISODATA was separeted in 4 parts
  #Perhaps the update matrix partition is U = A * ((B/C)+D)
  #First Part
  A = 1 / (1+alpha)
  
  #Second Part
  B = vector(length = N)
  for(i in 1:N){
    B[i] = 1 + (alpha * (1 - (b[i]*sum(FMatrix[i,]))))
  }
  
  #Third Part
  if(distance == "mahalanobis"){
    maha = dist.mahalanobis(db,V)
  }
  else if(distance == "euclidian"){
    maha = dist.euclidian(db,V)
  }
  else{
    maha = dist.mahalanobis(db,V)
  }
  C = matrix(nrow = N, ncol = k)
  
  for(i in 1:N){
    for(j in 1:k){
      C[i,j] = sum(maha[i,j]/maha[i,])
    }
  }
  
  #Fourth Part
  D = matrix(nrow = N, ncol = k)
  for(i in 1:N){
    for(j in 1:k){
      D[i,j] = alpha * FMatrix[i,j] * b[i]
    }
  }
  
  #Finalizing equation 
  U = A* ((B/C)+D)
  return(U)
}


#Fun��o que retorna a distancia de mahanaloobis de todos os pontos para todos os clusters
dist.mahalanobis <- function(db, V){
  average = colMeans(V)
  variance = var(V)
  N = nrow(db)
  k = nrow(V)
  dim = ncol(db)-1
  
  d2_mahalanobis = matrix(nrow = N, ncol = k, 0)
  
  for(i in 1:N){
    for(j in 1:k){
      d2_mahalanobis[i,j] = mahalanobis(db[i,1:dim], V[j,], variance)
    }
  }
  return(d2_mahalanobis)
}

#Fun��o que retorna a distancia euclidiana de todos os pontos para todos os clusters
#Feito na m�o, depois arrumo
dist.euclidian <- function(db, V){
  N = nrow(db)
  k = nrow(V)
  dim = ncol(db)-1
  colnames(V) = colnames(db[,1:dim])
  distancia = rbind(db[,1:dim],V)
  distancia = as.matrix(dist(distancia))
  lista = NULL
  for(i in 1:k){
    lista[[i]] = distancia[N+i,]
  }
  d2_euclidian = matrix(nrow = N, ncol = k)
  for(i in 1:N){
    for(j in 1:k){
      d2_euclidian[i,j] = lista[[j]][[i]]
    }
  }
  return(d2_euclidian)
}

