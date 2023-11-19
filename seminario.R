######################### relatorio dados hepatite

require(survival)
#figura 1 
t = c(1, 2, 3, 3, 3, 5, 5, 16, 16, 16, 16, 16, 16, 16, 16,1, 1, 1, 1, 4, 5, 7, 8, 10, 10, 12, 16, 16, 16)
c = c(0,0,1,1,rep(0,11),rep(1,3),0,0,rep(1,4),rep(0,5))
dados = Surv(t,c)
tipo = c(rep("Controle",15), rep("Esteroide",14))
kmfit = survfit(dados ~ tipo)
par(mfrow=c(1,1))
plot(kmfit, xlab="Tempo", ylab = "Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE, lty=c(1,2))
legend("bottomleft",c("Estimativa KM p/ Controle","Estimativa KM p/ Tratamento"), 
      ,lty = c(1,2))

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
plot(kmfit, mark.time = T,xlab="Tempos", ylab="Sobrevivência Estimada")
curve(1-pexp(x,0.0657),col='black',lty=1,lwd=2,add=T)
curve(1-plnorm(x,2.111,1.499),col='black',lty=2,lwd=1,add=T)
legend("bottomleft",c("Estimativa KM p/ Tratamento", "Estimativa Paramétrica (Exp) c/ EPQ p/ Tratamento","Estimativa Paramétrica (LN) c/ EPQ p/ Tratamento"), 
       ,lty = c(1,1,2), col=c("black", "black"), lwd=c(1,2,1))





#############33
skm = c(0.786,0.786,0.786,0.786, 0.786,0.698, 0.611,0.524,0.437,0.437,0.437,0.437,0.437,0.437)

par(mfrow=c(1,3))
plot(s1,skm)
plot(s2,skm)
plot(s3,skm)

plot(geral1, xlab="Tempos", ylab="S(t)", main="Curva para Controle")
#lines(c(0,time1),c(1,tab.wei1), lty=2, col= "red")
#lines(c(0,time1),c(1,tab.exp1), lty=2, col= "blue")
lines(c(0,time1),c(1,tab.log1), lty=2, col= "green")
legend("bottomleft",lty=c(1,2), col=c("black","red"),
       c("Kaplan-Meier", "Weibull"),bty="n",cex=1)

plot(survfit(dados~tipo), mark.time=T)

ajuste.exp1 = survreg(dados~tipo, dist="exponential")
ajuste.wei1 = survreg(dados~tipo, dist="weibull")
ajuste.log1 = survreg(dados~tipo, dist="lognormal")

geral1 = survfit(dados~tipo)

tab.kap1 = geral1$surv
time1 = geral1$time

alpha.exp1 = ajuste.exp1$coefficients[1] + ajuste.exp1$coefficients[2]
tab.exp1= exp(-time1/alpha.exp1)

alpha.wei1 = ajuste.wei1$coefficients[1]
gama.wei1 = ajuste.wei1$scale
tab.wei1 = exp(-(time1/alpha.wei1)^(1/gama.wei1))

alpha.log1 = ajuste.log1$coefficients[1]+ajuste.log1$coefficients[2]
gama.log1 = ajuste.log1$scale
tab.log1 = pnorm((-log(time1)+alpha.log1)/gama.log1)

plot(geral1, xlab="Tempos", ylab="S(t)", main="Curvas para Dados Hepatite")
lines(c(0,time1),c(1,tab.wei1), lty=2, col= "red")
points(c(0,time1),c(1,tab.log1), lty=2, col= "blue")
lines(c(0,time1),c(1,tab.log1), lty=2, col= "green")
legend(14,0.95,lty=c(1,2), col=c("black","red"),
       c("Kaplan-Meier", "Weibull"),bty="n",cex=1)


plot(kmfit, mark.time = T,xlab="Tempos", ylab="S(t)")



####################### carregando base de dados
infarto = read.table('http://sobrevida.fiocruz.br/dados/infarto.dat', 
           header=TRUE)
head(infarto)

tempo = infarto$fim - infarto$ini

infarto = cbind(infarto, tempo)
####################### analise exploratoria
summary(infarto$status) 
#maior parte das observações está censurada

cens = table(infarto$status)
prop.table(cens)

pie(cens, labels=c(0.81,0.16),main="Censuras e Falhas",
    col=c("blue","light blue"),border="brown",clockwise=TRUE)

legend("topleft", legend = c("Censura", "Falha"),
      fill =  c("blue", "lightblue"))


boxplot(infarto$tempo~infarto$hospital)

###################### analise sobrevivencia

library(survival)

mod.hospital =survfit(Surv(infarto$tempo,infarto$status)~infarto$hospital)
plot(mod.hospital,ylim=c(0,1),xlim = c(0,19),col=rainbow(25), xlab='Tempo', ylab='S(t) estimada')
legend("bottomleft", c(names(table(infarto$hospital))), lwd=2,  
       col=rainbow(25),bty='n')
unique(infarto$hospital)

mod.sexo = survfit(Surv(infarto$tempo,infarto$status)~infarto$sexo)
plot(mod.sexo,ylim=c(0,1),xlim = c(0,19),col=rainbow(2), xlab='Tempo', ylab='S(t) estimada')
legend("bottomleft", c(unique(infarto$sexo)), lwd=2,  
       col=rainbow(2),bty='n')
unique(infarto$hospital)

#t2<- infarto$fim[infarto2$hospital=="b"]
#t3<- infarto$fim[infarto2$hospital=="c"]
#t4<- infarto$fim[infarto2$hospital=="d"]
#c3<- infarto$status[infarto2$hospital=="c"]
#c4<- infarto$status[infarto2$hospital=="d"]
#d2=Surv(t2,c2)
#d3=Surv(t3,c3)
#d4=Surv(t4,c4)

t0<- infarto$fim[infarto$sexo=="M"]
t1<- infarto$fim[infarto$sexo=="F"]
c1<- infarto$status[infarto$sexo=="M"]
c2<- infarto$status[infarto$sexo=="F"]
d0= Surv(t0,c1)
d1=Surv(t1,c2)

plot(ecdf(t0))
plot(ecdf(t1))

ajuste.exp1 = survreg(d0 ~ 1, dist="exponential")
ajuste.exp0 = survreg(d0 ~ 1, dist="exponential")
ajuste.wei1 = survreg(d0 ~ 1, dist="weibull")
ajuste.log1 = survreg(d0 ~ 1, dist="lognormal")
plot(ecdf(t2))
curve(plnorm(x, 10, 1/2), add=T, col='red', lwd=3)
curve(pexp(x, 0.5), add=T, col='blue', lwd=3)
curve(pweibull(x, 0.5, 100), add=T, col='green', lwd=3)

kmfit = survfit(d0 ~ 1)
plot(kmfit, main = "Sobrevivência do Hospital 'a'",ylab="Sobrevivência Estimada",mark.time = T, conf.int = FALSE)
curve(plnorm(x, res$par[1], res$par[2]), add=T, col='red', lwd=3)
curve(pexp(x, res$par[1], res$par[2]), add=T, col='blue', lwd=3)
curve(1-pweibull(x, 150, 550), add=T, col='green', lwd=3)


tab.exp1= exp(-kmfit$time/ajuste.exp0$coefficients[1])
points(c(0,kmfit$time),c(1,tab.exp1),col= "red", lty=2)
tab.wei0= exp(-kmfit$time/ajuste.wei0$coefficients[1])
tab.log0= exp(-kmfit$time/ajuste.log0$coefficients[1])
points(c(0,kmfit$time),c(1,tab.wei0),col= "blue", lty=2)
points(c(0,kmfit$time),c(1,tab.log0),col= "green", lty=2)





legend(14,0.95,lty=c(1,2),col=c("black","violet"),
       c("Kaplan-Meier", "Exponencial"),bty="n",cex=1)

plot(survfit(d0 ~ 1), main = "Sobrevivência do Hospital 'a'",ylab="Sobrevivência Estimada",mark.time = T, conf.int = FALSE)
curve(dexp(x, ajuste.exp0$coefficients), add=T, col='red')




curve(dweibull(x, ajuste.wei1$coefficients[1]), add=T, col="red")
curve(dlnorm(x,ajuste.log1$coefficients[1]), add=T, col='green')
