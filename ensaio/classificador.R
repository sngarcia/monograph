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

despadroniza <- function(x,mx,mn)
{
  retorno <- (x*(mx-mn))+mn
  return(retorno)
}

#carrega, trata e separa apenas uma parte do dataset
carrega_subset <- function(arquivo) 
{
  dados <- read.table(arquivo,
                      header=TRUE,
                      sep=",",
                      colClasses=rep("numeric",7),
                      na="?")
  return(dados)
}
#---------------------------------------


#CARREGAMENTO
#carrega a base de dados
ovos <- carrega_subset("dados.csv")
ovos <- ovos[,-c(1,6)]

#notas <- padroniza(ovos$volume)

#boxplot(notas, xlab="", ylab="", main="")
#abline(h=mean(notas), col="red", lty=2)
#mtext("", side = 3)

#x <- notas
#h<-hist(x, breaks=30, xlab="", ylab="Frequência", main="")
#xfit<-seq(min(x),max(x),length=40)
#yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
#yfit <- yfit*diff(h$mids[1:2])*length(x)
#lines(xfit, yfit, lwd=2)
#abline(v=mean(notas), col="red", lty=2)

#qqPlot(x, pch=16,cex=1.5,las=1)

#n <- length(x)

#resultado = shapiro.test(x)

#print(resultado)



#ANALISE DE DADOS
#Área do contorno
ovos_desordenados<-ovos[sample(nrow(ovos)),]
 
plot(ovos_desordenados$area_contorno, col=(ovos_desordenados$classificacao+1), main="Distribuição das classes de ovos", xlab="Amostras", ylab="Área do contorno", pch=16)
mtext("Característica: Área do Contorno", side = 3)
legend("topright", legend=c("E", "A"),
       fill=unique(ovos$classificacao+1), bty="n")

hist(ovos_desordenados$area_contorno, breaks =15, main="Histograma da Área do Contorno", col="blue", xlab="Área do contorno (pixels)", ylab = "Frequência")

#Volume
plot(ovos_desordenados$volume, col=(ovos_desordenados$classificacao+1), main="Distribuição das classes de ovos", xlab="Amostras", ylab="Volume (pixels cúbicos)", pch=16)
mtext("Característica: Volume (pixels cúbicos)", side = 3)
legend("topright", legend=c("E", "A"),
       fill=unique(ovos$classificacao+1), bty="n")
hist(ovos_desordenados$volume, breaks =15, main="Histograma do Volume", col="blue", xlab="Volume (pixels)", ylab = "Frequência")

#Altura
plot(ovos_desordenados$altura, col=(ovos_desordenados$classificacao+1), main="Distribuição das classes de ovos", xlab="Amostras", ylab="Altura (pixels)", pch=16)
mtext("Característica: Altura (pixels)", side = 3)
legend("topright", legend=c("E", "A"),
       fill=unique(ovos$classificacao+1), bty="n")
hist(ovos$altura, breaks =15, main="Histograma da Altura", col="blue", xlab="Altura (pixels)", ylab = "Frequência")

#Largura
plot(ovos_desordenados$largura, col=(ovos_desordenados$classificacao+1), main="Distribuição das classes de ovos", xlab="Amostras", ylab="Largura (pixels)", pch=16)
mtext("Característica: Largura (pixels)", side = 3)
legend("topright", legend=c("E", "A"),
       fill=unique(ovos$classificacao+1), bty="n")
hist(ovos$largura, breaks =15, main="Histograma da Largura", col="blue", xlab="Largura (pixels)", ylab = "Frequência")

#ANÁLISE DE CORRELAÇÃO - CORRELOGRAMA
compara <- data.frame(ovos$area_contorno,ovos$volume,ovos$altura,ovos$largura)
colnames(compara) <- c("Área do Contorno","Volume","Altura","Largura")

#gera o correlograma
corrgram(compara,lower.panel = panel.shade,upper.panel = panel.pie, text.panel=panel.txt,main="Correlograma entre ativos", order=TRUE)

#RESOLVEMOS DISPENSAR O VOLUME COM BASE NA CORRELACAO ALTA ENTRE ESTE E A AREA ALÉM DO QUE O MESMO APRESENTA EVIDENCIAS DE MENOR NORMALIDADE NA DISTRIBUIÇÃO DOS DADOS
ovos <- ovos[,-c(2)]

#analisar os dados em 3 d
plot3d(ovos[,1:3], col=ovos[,4]+1, size=8)

#Separa o conjunto de testes e de treino
set.seed(1)
tam_amostra_treino = 60
train_ind <- sample(seq_len(dim(ovos)[1]), size = tam_amostra_treino)
amostra_treino <- ovos[train_ind, ]
amostra_teste <- ovos[-train_ind, ]

#observando a amostra de treino para ver se o conjunto esta visualmente representativo
#analisar os dados em 3 d
plot3d(amostra_treino[,1:3], col=amostra_treino[,4], size=8)

#padronizando os conjuntos
#tabela atributo valor padronizada
x_area <- padroniza(amostra_treino$area_contorno)
x_altura <- padroniza(amostra_treino$altura)
x_largura <- padroniza(amostra_treino$largura)

x <- cbind(x_area,x_altura,x_largura)

#padronizando as classes
y <- padroniza(amostra_treino$classificacao)

#configuracoes da MLP
nNeuronios = 70
maxEpocas <- 100000

#treinamento da MLP
redeMLP <- NULL

load('redeMLP.Rdata')

#Backpropagation
#print("treinando a rede na serie ajustada...")

#redeMLP<-mlp(x, y, size=nNeuronios, maxit=maxEpocas, initFunc="Randomize_Weights",
#            initFuncParams=c(-0.3, 0.3), learnFunc="Std_Backpropagation",
#            learnFuncParams=c(0.00001), updateFunc="Topological_Order",
#            updateFuncParams=c(0), hiddenActFunc="Act_Logistic",
#            shufflePatterns=F, linOut=TRUE)

plot(redeMLP$IterativeFitError,type="l",main="Erro da MLP Classificação")
print(redeMLP$IterativeFitError[length(redeMLP$IterativeFitError)])

#save(redeMLP, file='redeMLP.Rdata')

#EXECUTANDO A PREVISOES COM O MODELO TREINADO
#EXECUCAO NA AMOSTRA DE TESTE
x_teste_area <- padroniza(amostra_teste$area_contorno)
x_teste_altura <- padroniza(amostra_teste$altura)
x_teste_largura <- padroniza(amostra_teste$largura)
x_teste <- cbind(x_teste_area,x_teste_altura,x_teste_largura)

yhat_teste = vector()

for (i in 1:dim(amostra_teste)[1])
{
  yhat_teste[i] = round(predict(redeMLP,x_teste[i,]))
}

y_teste <- padroniza(amostra_teste[,4])

#CALCULO DO ERRO
erromlp_teste <- mean(sqrt((y_teste-yhat_teste)^2))
erromlp_teste <- despadroniza(erromlp_teste,2,1)

print(paste("Erro no conjunto de teste:",erromlp_teste))

#MATRIZ DE CONFUSAO
print("MATRIZ DE CONFUSAO NO TREINO")
confusao <- confusion.matrix(y_teste,yhat_teste)
print(confusao)

#acerta os dados para geracao de um grafico de barras
resultado <- c(confusao[1],confusao[2],confusao[4],confusao[3])
bp <- barplot(resultado, names.arg = c("A-A","A-E","E-E","E-A"),col = c("green","red","green","red"),
        xlab="Previsoes",ylab="Quantidade")
#text(x = bp, y = 4, label = resultado, pos = 2, cex = 0.8)

#CALCULO DA ACURACIA
vetor_acuracia = sqrt((y_teste-yhat_teste)^2)
print(paste("Acurácia: ", (length(which(vetor_acuracia == 0))*100)/length(vetor_acuracia), "%"))

#PLOTANDO A CLASSIFICAO QUE A REDE FEZ
plot3d(amostra_teste[,1:3], col=yhat_teste+1, size=8)
plot(cbind(amostra_teste[,2],amostra_teste[,3]), col=yhat_teste+2)

#GERANDO SUPERFICIE DE SEPARACAO
#GERAR O CUBO MUITO LOUCO