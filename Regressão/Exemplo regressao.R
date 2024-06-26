#Aqui temos os dados da questão 6 do livro Análise de Modelos de Regressão Linear com Aplicações (CHARNET et al, 2008).
#A fim de tornar o exemplo mais educativo a autora adicionou o contexto aos dados

x = c(10.875,11,11.125,11.5,12,12.25,12.5,13.75,13.75,14,14.5,15.5,16.25,17,19) #variavel independente
y=c(71.05,70.59,69.21,68.82,69.24,68.14,67.8,67.03,67.42,66.94,68.8,65.14,64.04,63.98,62.96) #variavel dependente
dados = data.frame(x,y)

ajuste = lm(y~x,dados)

dados2 = dados[-c(11,15),]
ajuste2 = lm(y~x,dados2)

dados3 = data.frame(x=x,y=y)
dados3 = dados3[-11,]
ajuste3 = lm(y~x,dados3)

par(mar=c(5, 6, 6, 4))
plot(x,y, main = , ylab="Peso (kg)", xlab = "Tempo de exercícios semanais (h)",type="p",pch=16, cex.axis=1.3,cex.lab=1.5)
abline(ajuste, lwd =2,col="#b37400")
abline(ajuste2,lty=2,lwd =2,col='#674ea7')
abline(ajuste3, lty=3,lwd=3,col="#f71aaa")
legend(x=c(17,17),y=c(71,71),legend=c(expression(hat(r[1])),expression(hat(r[2])),expression(hat(r[3]))), lty=c(1,2,3),lwd=c(2,2,3),col=c("#b37400","#674ea7",'#f71aaa'),cex=1.5,box.col=0,box.lty=0)

par(mfrow=c(1,3), mar=c(5, 6, 6, 4))
plot(x,y, main = , ylab="Peso (kg)", xlab = "Tempo de exercícios semanais (h)",type="p",pch=16, cex.axis=1.3,cex.lab=1.5)
abline(ajuste, lwd =2,col="#b37400")
plot(x,y, main = , ylab="Peso (kg)", xlab = "Tempo de exercícios semanais (h)",type="p",pch=16, cex.axis=1.3,cex.lab=1.5)
abline(ajuste2,lty=2,lwd =2,col='#674ea7')
plot(x,y, main = , ylab="Peso (kg)", xlab = "Tempo de exercícios semanais (h)",type="p",pch=16, cex.axis=1.3,cex.lab=1.5)
abline(ajuste3, lty=3,lwd=3,col="#f71aaa")
