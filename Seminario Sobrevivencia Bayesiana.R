######################### relatorio dados hepatite

require(survival)
#figura 1 
t = c(1, 2, 3, 3, 3, 5, 5, 16, 16, 16, 16, 16, 16, 16, 16,1, 1, 1, 1, 4, 5, 7, 8, 10, 10, 12, 16, 16, 16)
c = c(0,0,1,1,rep(0,11),rep(1,3),0,0,rep(1,4),rep(0,5))
dados = Surv(t,c)
tipo = c(rep("Controle",15), rep("Esteroide",14))
kmfit = survfit(dados ~ tipo)
par(mfrow=c(1,1))
plot(kmfit, xlab="Tempo", ylab = "Sobrevivência Estimada", main= "Curvas de Sobrevivência para Dados de Hepatite (Colossimo, 2006)",
     mark.time = T, conf.int = FALSE, lty=c(1,2))
legend("bottomleft",c("Estimativa KM p/ Controle","Estimativa KM p/ Tratamento"), 
       ,lty = c(1,2), bty="n")

#figura 2

temp <- survfit(dados~tipo,type="fh")

plot(temp, #main = "Representação Gráfica: Dados Hepatite e Estimativa NA",
     xlab="Tempo", ylab = "Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE, col="dark green",lty = c(1,2))
legend("bottomleft",c("Estimativa KM p/ Controle","Estimativa KM p/ Tratamento"), 
       ,lty = c(1,2), col="dark green")

#### figura 3
tc=t[1:15]
tt=t[16:29]
cc=c[1:15]
ct=c[16:29]

a_c = sum(cc)/sum(tc)
a_t = sum(ct)/sum(tt)

plot(kmfit, xlab="Tempo", ylab = "Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE, lty=c(1,2))
curve(1-pexp(x,a_c), add=T, col='purple', lty=1, lwd=2)
curve(1-pexp(x,a_t), add=T, col='purple', lty=2, lwd=2)
legend("bottomleft",c("Estimativa KM p/ Controle","Estimativa KM p/ Tratamento", "Estimativa Paramétrica (Exp) p/ Controle","Estimativa Paramétrica (Exp) p/ Tratamento"), 
       ,lty = c(1,2,1,2), col=c("black", "black","purple","purple"), lwd=c(1,1,2,2))

#### figura 4
L = function(theta,t,c) theta^(sum(c))*exp(-(sum(t)*theta))
a1=sum(ct)+0.01
a2 = sum(tt)+0.01
par(mfrow=c(1,3))
curve(L(theta,tt,ct), from=0,to=1,main="(a)",xname="theta", xlab="alpha",ylab="Valor da Verossimilhança")
curve(dgamma(x,0.01,0.01), col='blue',,main="(b)", xlab="alpha",ylab="Densidade")
curve(dgamma(x,a1,a2),col='red',xlab="alpha",main="(c)",ylab="Densidade")


###### fig 5
trat = survfit(Surv(tt,ct)~1)
(a1 -1)/a2 #moda de alfa/x 

par(mfrow=c(1,1))
plot(trat, xlab="Tempo", ylab="Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE) #,main="Curva para Tratamento com Esteroide")
curve(1-pexp(x,0.0646),col='purple',lty=2,lwd=2,add=T)
curve(1-pexp(x,0.0657),col='black',lty=1,lwd=2,add=T)
curve(1-pexp(x,0.0602),col='orange',lty=2,lwd=3,add=T)
legend("bottomleft",c("Estimativa KM p/ Tratamento", "Estimativa Paramétrica (Exp) Frequentista p/ Tratamento","Estimativa Paramétrica (Exp) c/ EPQ p/ Tratamento","Estimativa Paramétrica (Exp) c/ EP0-1 p/ Tratamento"), 
       ,lty = c(1,2,1,2), col=c("black", "purple","black","orange"), lwd=c(1,2,2,3))

######## fig 6
par(mfrow=c(1,1))
plot(kmfit, mark.time = T,xlab="Tempos", ylab="Sobrevivência Estimada")
curve(1-plnorm(x,2.327981,1.603826),col='purple',lty=2,lwd=2,add=T)
curve(1-plnorm(x,2.111,1.499),col='black',lty=1,lwd=2,add=T)
curve(1-plnorm(x,2.098,1.45),col='orange',lty=2,lwd=3,add=T)
legend("bottomleft",c("Estimativa KM p/ Tratamento", "Estimativa Paramétrica (LN) Frequentista p/ Tratamento","Estimativa Paramétrica (LN) c/ EPQ p/ Tratamento","Estimativa Paramétrica (LN) c/ EP0-1 p/ Tratamento"), 
       ,lty = c(1,2,1,2), col=c("black", "purple","black","orange"), lwd=c(1,2,2,3))

#### fig 7
ajuste.log1 = survreg(Surv(tt,ct) ~ 1, dist="lognormal")
par(mfrow=c(1,1))
plot(trat, mark.time = T, conf.int = F,xlab="Tempos", ylab="Sobrevivência Estimada", main="Curvas de Sobrevivência para Grupo Tratamento")
curve(1-pexp(x,0.0657),col='blue',lty=1,lwd=2,add=T)
curve(1-plnorm(x,2.111,1.499),col='red',lty=1,lwd=2,add=T)
legend("bottomleft",c("Estimativa Não Paramétrica (KM)", "Estimativa Paramétrica Bayesiana (Exp) c/ EPQ ","Estimativa Paramétrica Bayesiana (LN) c/ EPQ "), 
       ,lty = c(1,1,1), col=c("black","blue", "red"), lwd=c(1,2,2), bty = "n")


######codigo winbugs
#model{
  #likelihood
 # for(i in 1:n){t[i]~dexp(alpha)I(c[i],)}
  #prior
  #alpha~dgamma(0.1,0.1)
#}

#Data
#list(t=c(1,1,1,NA,NA,5,7,8,10,NA,NA,NA,NA,NA),
 #    c=c(0,0,0,1,4,0,0,0,0,10,12,16,16,16), n=14)

#model{
  #likelihood
 # for(i in 1:n){
  #  t[i]~dlnorm(mu,tau)I(c[i],)
  #}
  #prior
  #mu ~ dnorm(3,2)
  #tau ~ dgamma(9,0.05)
#}

#Data
#list(t=c(1,1,1,NA,NA,5,7,8,10,NA,NA,NA,NA,NA),
 #    c=c(0,0,0,1,4,0,0,0,0,10,12,16,16,16), n=14)