############## Lista 1 ###########
ExperimentDesign = expand.grid(A= gl(3,1, labels=c(10,12,14)),
                               B= gl(2,1, labels=c(25,30)),
                               C = gl(2,1,labels=c(200,250)))

respostas = c(-3,0,5,-1,2,7,-1,2,7,1,6,10,-1,1,4,0,3,9,0,1,6,1,5,11)


dados = data.frame(rep= rep(1:2, each=12),rbind(ExperimentDesign),y = respostas)

aggregate(y~A +B +C,
          FUN = sum, data = dados) #soma das replicas

ajuste = aov(y~A+B+C+A*B+A*C+B*C+A*B*C, data = dados)
summary(ajuste)

#grafico dos efeitos principais

plot.design(y~A+B+C+A*B+A*C+B*C+A*B*C, data = dados, main = "Grafico de efeitos principais Lista 1", xlab = "Fatores", ylab = "Média da resposta")

#grafico y barra das cominações de tratamento
library("dae")
interaction.ABC.plot(y, x.factor=A,groups.factor=B, trace.factor=C, data=dados,
                     title= "Efeito de A,B e C na variável resposta", ylab = "Resposta média")

#grafico pontos e niveis
library(scatterplot3d)
scatterplot3d(dados$A,dados$B,dados$C, color = "red", pch=19)

#graficos supostos
par(mfrow=c(2,2))
plot(ajuste)
box("outer")
############## Lista 1 ###########


############## Lista 3 ###########
ExperimentDesign = expand.grid(A= gl(2,1, labels=c("-", "+")),
                               B= gl(2,1, labels=c("-", "+")),
                               )

respostas = c(18.2,27.2,15.9,41.0,18.9,24,14.5,43.9,12.9,22.4,15.1,36.3,14.4,22.5,14.2,39.9)


dados = data.frame(rep= rep(1:4, each=4),rbind(ExperimentDesign),y = respostas)

aggregate(y~A +B,
          FUN = sum, data = dados) #soma das replicas

ajuste = aov(y~A+B+A*B, data = dados)
summary(ajuste)
#model.tables(ajuste,'effects')
#grafico dos efeitos principais

plot.design(y~A+B+A*B, data = dados, main = "Grafico de efeitos principais Lista 1", xlab = "Fatores", ylab = "Média da resposta")

#grafico y barra das cominações de tratamento
library("dae")
interaction.plot(dados$A,dados$B, dados$y, type="b", pch=c(18,24), leg.bty="o",
                  main= "Efeito de A,B na variável resposta", ylab = "Resposta média",xlab = "Nível Fator A",trace.label = "Nível Fator B")

#graficos supostos
par(mfrow=c(2,2))
plot(ajuste)

#q2

ExperimentDesign = expand.grid(A= gl(2,1, labels=c("0", "a")),
                               B= gl(2,1, labels=c("0", "b")),
                               C= gl(2,1, labels=c("0", "c")),
                               D= gl(2,1, labels=c("0", "d"))
)

respostas = c(0.75,0.98,0.72,0.98,0.63,0.67,0.65,0.8,0.6,0.81,0.63,0.79,0.56,0.65,0.55,0.69)


dados = data.frame(combinacao = 1:16,rbind(ExperimentDesign),y = respostas)

ajuste <- aov(y~A+B+C+D+A*B+A*C+A*D+B*C+B*D+C*D+A*B*C+A*B*D+A*C*D+B*C*D, data=dados)
summary(ajuste)

ajuste1 <- aov(y~A+B+C+D+A*B+A*C+A*D+B*C+B*D+C*D+A*B*C+A*B*D+A*C*D+B*C*D, data=dados)


model.tables(ajuste,'effects')
#grafico dos efeitos principais

plot.design(y~A+B+A*B, data = dados, main = "Grafico de efeitos principais Lista 1", xlab = "Fatores", ylab = "Média da resposta")

#grafico y barra das cominações de tratamento
library("dae")
interaction.plot(dados$A,dados$B, dados$y, type="b", pch=c(18,24), leg.bty="o",
                 main= "Efeito de A,B na variável resposta", ylab = "Resposta média",xlab = "Nível Fator A",trace.label = "Nível Fator B")

#graficos supostos
par(mfrow=c(2,2))
plot(ajuste)


############## Lista 3 ###########
