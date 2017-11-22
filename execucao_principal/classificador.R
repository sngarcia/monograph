#limpar workspace
rm(list=ls())

#limpar tela
cat('\014')

#bibliotecas
library("RSNNS")
library("rgl")
library("SDMTools")
library("corrgram")
library("car")
library("lmtest")

#---------------------------------------
#FUNCOES
#Coloca amostra nos intervalos proporcionais entre 0 e 1
padroniza <- function(s)
{
  retorno <- (s - min(s))/(max(s)-min(s))
  return(retorno)
}

padroniza_classificacao<-function(s)
{
  retorno<-matrix(NA,length(s), 2)
  
  for(i in 1:length(s) ){
    if(s[i]==1)
      retorno[i,] <-c(0,0)
    if(s[i]==2)
      retorno[i,] <-c(0,1)
    if(s[i]==3)
      retorno[i,] <-c(1,0)
    if(s[i]==4)
      retorno[i,] <-c(1,1)
  }
  return(retorno)
}

despadroniza_classificacao<-function(s)
{
  retorno<-vector()
  
  for(i in 1:400){
    if(s[i]==0 && s[i+400]==0)
      retorno[i] <-1
    
    if(s[i]==0 && s[i+400]==1)
      retorno[i] <-2
    
    if(s[i]==1 && s[i+400]==0)
      retorno[i] <-3
    
    if(s[i]==1 && s[i+400]==1)
      retorno[i] <-4
  }
  return(retorno)
}

despadroniza <- function(x,mx,mn)
{
  retorno <- (x*(mx-mn))+mn
  return(retorno)
}

#carrega o dataset
carrega_subset <- function(arquivo) 
{
  dados <- read.table(arquivo,
                      header=TRUE,
                      sep=";", 
                      colClasses = c("character", rep("numeric",9)))
  return(dados)
}
#---------------------------------------


#CARREGAMENTO
#carrega a base de dados
ovos <- carrega_subset("dataset.csv")

#ANALISE DE DADOS
#Área do contorno
#plot(amostra_treino$lado_a_area, col=(amostra_treino$classificacao), main="Distribuição das classes de ovos", xlab="amostras", ylab="Área do contorno", pch=16)
#mtext("Característica: Área do Contorno", side = 3)
#legend("topright", legend=c("B", "Extra", "Jumbo","A"),
#       fill=unique(amostra_treino$classificacao), bty="n")

#hist(amostra_treino$lado_a_area, breaks =15, main="Histograma da Área do Contorno")

#Volume
#plot(amostra_treino$volume, col=(amostra_treino$classificacao), main="Distribuição das classes de ovos", xlab="amostras", ylab="Volume (pixels cúbicos)", pch=16)
#mtext("Característica: Volume (pixels cúbicos)", side = 3)
#legend("topright", legend=c("B", "Extra", "Jumbo","A"),
#       fill=unique(amostra_treino$classificacao), bty="n")

#hist(ovos$volume, breaks =15, main="Histograma do Volume")

#Altura
#plot(amostra_treino$lado_a_altura, col=(amostra_treino$classificacao), main="Distribuição das classes de ovos", xlab="amostras", ylab="Altura (pixels)", pch=16)
#mtext("Característica: Altura (pixels)", side = 3)
#legend("topright", legend=c("B", "Extra", "Jumbo","A"),
#       fill=unique(amostra_treino$classificacao), bty="n")

#hist(amostra_treino$lado_a_altura, breaks =15, main="Histograma da Altura")

#Largura
#plot(amostra_treino$lado_a_largura, col=(amostra_treino$classificacao), main="Distribuição das classes de ovos", xlab="amostras", ylab="Largura (pixels)", pch=16)
#mtext("Característica: Largura (pixels)", side = 3)
#legend("topright", legend=c("B", "Extra", "Jumbo","A"),
#       fill=unique(amostra_treino$classificacao), bty="n")
#hist(amostra_treino$lado_a_largura, breaks =15, main="Histograma da Largura")

#ANÁLISE DE CORRELAÇÃO - CORRELOGRAMA
#compara <- data.frame(amostra_treino$lado_a_area, amostra_treino$lado_a_altura, amostra_treino$lado_a_largura,
#                      amostra_treino$lado_b_area, amostra_treino$lado_b_altura, amostra_treino$lado_b_largura,
#                     amostra_treino$volume)

#colnames(compara) <- c("A-área","A-altura","A-largura","B-área", "B-altura", "B-largura", "Volume")

#gera o correlograma
#corrgram(compara,lower.panel = panel.shade,upper.panel = panel.pie, text.panel=panel.txt,main="Correlograma entre ativos", order=TRUE)

#RESOLVEMOS DISPENSAR O VOLUME COM BASE NA CORRELACAO ALTA ENTRE ESTE E A AREA ALÉM DO QUE O MESMO APRESENTA EVIDENCIAS DE MENOR NORMALIDADE NA DISTRIBUIÇÃO DOS DADOS
#ovos <- ovos[,-c(2)]

#analisar os dados em 3 d
#plot3d(ovos[,1:3], col=ovos[,4], size=8)

#observando a amostra de treino para ver se o conjunto esta visualmente representativo
#analisar os dados em 3 d
#plot3d(amostra_treino[,1:3], col=amostra_treino[,4], size=8)

#padronizando os conjuntos
#tabela atributo valor padronizada
#x_area <- padroniza(amostra_treino$area_contorno)
#x_altura <- padroniza(amostra_treino$altura)
#x_largura <- padroniza(amostra_treino$largura)


#padronizando as classes

ovos <- ovos[,-c(1,6,7,8,10)] #testes sem o volume, b-largura, b-altura

ovos$lado_a_area <- padroniza(ovos$lado_a_area)
ovos$lado_a_altura <- padroniza(ovos$lado_a_altura)
ovos$lado_a_largura <- padroniza(ovos$lado_a_largura)
ovos$lado_b_area <- padroniza(ovos$lado_b_area)
#ovos$lado_b_altura <- padroniza(ovos$lado_b_altura)
#ovos$lado_b_largura <- padroniza(ovos$lado_b_largura)
#ovos$volume <- padroniza(ovos$volume)

#tam_amostra_treino = 160

set.seed(1)
ovos<-ovos[sample(nrow(ovos)),]
indice_treino <-seq(1,160)
indice_teste <-seq(161, 280)
indice_validacao <-seq(281, 400)

amostra_treino <- ovos[indice_treino, ]
amostra_teste <- ovos[indice_teste, ]
amostra_validacao<-ovos[indice_validacao,]



x_treino <-amostra_treino[,-c(5)]  
y_treino = matrix(NA, 160, 2)
y_treino <- padroniza_classificacao(amostra_treino$classificacao)  

maxEpocas = 10000

nNeuronios<- 54
taxa_aprendizado <- 0.045


#set.seed(20)
#for(j in 1:1){
  #configuracoes da MLP
  #nNeuronios = nNeuronios+3  #sample(50:100, 1)
  #maxEpocas <-sample(50000:100000, 1) 
  
  #treinamento da MLP
  redeMLP <- NULL
  
  
  #Taxa de aprendizado
  #taxa_aprendizado <- taxa_aprendizado+0.005   #0.000001* sample(1:100000, 1)
  
  #load('redeMLP.Rdata')
  
  #Backpropagation
  print("treinando a rede na serie ajustada...")
  print("Neuronios ")
  print(nNeuronios)
  #print("Max epocas")
  #print(maxEpocas)
  print("aprendizado ")
  print(taxa_aprendizado)
  
  
  redeMLP<-mlp(x_treino, y_treino, size=nNeuronios, maxit=maxEpocas, initFunc="Randomize_Weights",
               initFuncParams=c(-0.3, 0.3), learnFunc="Std_Backpropagation",
               learnFuncParams=c(taxa_aprendizado), updateFunc="Topological_Order",
               updateFuncParams=c(0), hiddenActFunc="Act_Logistic",
               shufflePatterns=F, linOut=TRUE)
  
  plot(redeMLP$IterativeFitError,type="l",main="Erro da MLP")
  
  print("Erro da última interação: ")
  print(redeMLP$IterativeFitError[length(redeMLP$IterativeFitError)])
  
  erro_interacao=redeMLP$IterativeFitError[length(redeMLP$IterativeFitError)]

  
  #save(redeMLP, file='redeMLP.Rdata')
  
  #EXECUTANDO A PREVISOES COM O MODELO TREINADO
  #EXECUCAO NA AMOSTRA DE TESTE
  #x_teste_area <- padroniza(amostra_teste$area_contorno)
  #x_teste_altura <- padroniza(amostra_teste$altura)
  #x_teste_largura <- padroniza(amostra_teste$largura)
  #x_teste <- cbind(x_teste_area,x_teste_altura,x_teste_largura)
  
  
  yhat_treino = matrix(NA, 160, 2)
  x_treino <-amostra_treino[,-c(5)]                                    #
  y_treino = matrix(NA, 160, 2)
  
  y_treino<- padroniza_classificacao(amostra_treino$classificacao) #
  
  for (i in 1:dim(amostra_treino)[1])                       #
  {
    #print(i)
    yhat_treino[i,] = predict(redeMLP,x_treino[i,])
  }
  
  erro<-0
  for(i in 1: dim(amostra_treino)[1]){                      #
    if(round(yhat_treino[i,1])!=y_treino[i,1] || round(yhat_treino[i,2])!=y_treino[i,2] ){
      erro<-erro+1
    }
  }
  print(paste("Classificações Erradas na parcela Treino: " ,erro))
  print(paste("Percentual de Acerto:", ((1-(erro/160))*100), "%"))
  
  yhat_teste = matrix(NA, 120, 2)
  x_teste <-amostra_teste[,-c(5)]                                    #
  y_teste= matrix(NA, 120, 2)
  
  y_teste<- padroniza_classificacao(amostra_teste$classificacao)     #
  
  for (i in 1:dim(amostra_teste)[1])                       #
  {
    #print(i)
    yhat_teste[i,] = predict(redeMLP,x_teste[i,])
  }
  
  erro<-0
  for(i in 1: dim(amostra_teste)[1]){                      #
    if(round(yhat_teste[i,1])!=y_teste[i,1] || round(yhat_teste[i,2])!=y_teste[i,2] ){
      erro<-erro+1
    }
  }
  print(paste("Classificações Erradas na parcela Teste: ", erro))
  print(paste("Percentual de Acerto: ", ((1-(erro/120))*100), "%"))
#}


yhat_Validacao = matrix(NA, 120, 2)
x_validacao <-amostra_validacao[,-c(5)]                                    #
y_validacao= matrix(NA, 120, 2)

y_validacao<- padroniza_classificacao(amostra_validacao$classificacao) #

for (i in 1:dim(amostra_validacao)[1])                       #
{
  #print(i)
  yhat_Validacao[i,] = predict(redeMLP,x_validacao[i,])
}

erro<-0
for(i in 1: dim(amostra_validacao)[1]){                      #
  if(round(yhat_Validacao[i,1])!=y_validacao[i,1] || round(yhat_Validacao[i,2])!=y_validacao[i,2] ){
    erro<-erro+1
  }
}
print(paste("Classificações Erradas na parcela Validação: ", erro))
print(paste("Percentual de Acerto:", ((1-(erro/120))*100), "%"))

#################################################
# Execução com toda a amostra                 
#################################################
set.seed(2)
ovos<-ovos[sample(nrow(ovos)),]

yhat_final<-matrix(NA, 400, 2)
x_final<-ovos[-c(5)]
y_final<- matrix(NA, 400, 2)
y_final<- padroniza_classificacao(ovos$classificacao)

for (i in 1:dim(ovos)[1]){
  yhat_final[i,]<-predict(redeMLP,x_final[i,])
}

erros<-0
for(i in 1: dim(ovos)[1]){                      #
  if(round(yhat_final[i,1])!=y_final[i,1] || round(yhat_final[i,2])!=y_final[i,2] ){
    erros<-erros+1
  }
}
print(paste("Erros Totais: ", erros))
print(paste("Percentual de Acerto:", ((1-(erros/400))*100), "%"))

y_1<-despadroniza_classificacao(y_final)
y_2<-despadroniza_classificacao(round(yhat_final))
correlacao<-cor(y_1, y_2, use = "na.or.complete")
print(paste("Coeficiente de Correlação:", correlacao))

erromlp_final <- mean(sqrt((y_final-yhat_final)^2))
print(paste("EQM: ", erromlp_final))
