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
#curve(1-plnorm(x,1.947,sqrt(2.5)),col='orange',lty=2,lwd=3,add=T)
legend("bottomleft",c("Estimativa KM p/ Tratamento", "Estimativa Paramétrica (LN) Frequentista p/ Tratamento","Estimativa Paramétrica (LN) c/ EPQ p/ Tratamento","Estimativa Paramétrica (LN) c/ EP0-1 p/ Tratamento"), 
       ,lty = c(1,2,1,2), col=c("black", "purple","black","orange"), lwd=c(1,2,2,3))

#### fig 7
ajuste.log1 = survreg(Surv(tc,cc) ~ 1, dist="exponential")
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

########## apresentação

#fig 1
par(mfrow=c(1,3))
curve(dnorm(x,1,0.5),xlab=expression(paste(theta)),xlim=c(-1,3), main="(a)", ylab=expression(paste("P(",theta,")")))
curve(dlnorm(x,0,0.7),xlab=expression(paste(theta)),xlim=c(-1,3),main="(b)", ylab=expression(paste("P(",theta,")")))
curve(dunif(x,),xlab=expression(paste(theta)),xlim=c(-1,3),main="(c)", ylab=expression(paste("P(",theta,")")))

# fig 2

L = function(theta, sx,n) (theta^(sx))*((1-theta))^(n-sx)
sx=4 #sucesso
n=10 #tamanho amostral
par(mfrow=c(1,1))
curve(L(theta,sx,n), from=-1,to=1.5,xname="theta", xlab=expression(paste(theta)),ylab="Valor da Verossimilhança")

# fig 3
L = function(theta,t,c) theta^(sum(c))*exp(-(sum(t)*theta))
a1=sum(cc)+0.01
a2 = sum(tc)+0.01
par(mfrow=c(2,3))
curve(L(theta,tc,cc), from=0,to=1,main="Verossimilhança Controle",xname="theta", xlab=expression(alpha),ylab="Valor da Verossimilhança")
curve(dgamma(x,0.01,0.01), col='blue',main="À Priori Controle", xlab=expression(alpha),ylab="Densidade")
curve(dgamma(x,a1,a2),col='red',xlab=expression(alpha),main="À Posteriori Controle",ylab="Densidade")
a1=sum(ct)+0.01
a2 = sum(tt)+0.01
curve(L(theta,tt,ct), from=0,to=1,main="Verossimilhança Tratamento",xname="theta", xlab=expression(alpha),ylab="Valor da Verossimilhança")
curve(dgamma(x,0.01,0.01), col='blue',,main="À Priori Tratamento", xlab=expression(alpha),ylab="Densidade")
curve(dgamma(x,a1,a2),col='red',xlab=expression(alpha),main="À Posteriori Tratamento",ylab="Densidade")

# fig 4
par(mfrow=c(1,1))
plot(kmfit, xlab="Tempo", ylab = "Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE, lty=c(1,2))
curve(1-pexp(x,0.0133), add=T, col='blue', lty=1, lwd=3)
curve(1-pexp(x,0.0134), add=T, col='red', lty=2, lwd=3)
curve(1-pexp(x,0,0067), add=T, col='dark green', lty=3, lwd=3)
curve(1-pexp(x,0.0648), add=T, col='purple', lty=1, lwd=3)
curve(1-pexp(x,0.0649), add=T, col='black', lty=2, lwd=3)
curve(1-pexp(x,0.0556), add=T, col='dark orange', lty=3, lwd=3)
legend("bottomleft",c(
                      "Estimativa Frequentista p/ Controle","Estimativa sob PQ p/ Controle",
                      "Estimativa sob P01 p/ Controle","Estimativa Frequentista p/ Tratamento",
                      "Estimativa sob PQ p/ Tratamento","Estimativa sob P01 p/ Tratamento"), 
       ,lty = c(1,2,3,1,2,3), lwd=c(rep(3,6)), bty='n',
       col=c("blue","red","dark green","purple", "black","dark orange"))

#fig 5
L = function(theta,t,c) theta^(sum(c))*exp(-(sum(t)*theta))
a1=sum(cc)+5
a2 = sum(tc)+0.5
par(mfrow=c(2,3))
curve(L(theta,tc,cc), from=0,to=1,main="Verossimilhança Controle",xname="theta", xlab=expression(alpha),ylab="Valor da Verossimilhança")
curve(dgamma(x,5,0.5), col='blue',main="À Priori Controle", xlab=expression(alpha),ylab="Densidade")
curve(dgamma(x,a1,a2),col='red',xlab=expression(alpha),main="À Posteriori Controle",ylab="Densidade")
a1=sum(ct)+2
a2 = sum(tt)+1.9
curve(L(theta,tt,ct), from=0,to=1,main="Verossimilhança Tratamento",xname="theta", xlab=expression(alpha),ylab="Valor da Verossimilhança")
curve(dgamma(x,2,1.9), col='blue',,main="À Priori Tratamento", xlab=expression(alpha),ylab="Densidade")
curve(dgamma(x,a1,a2),col='red',xlab=expression(alpha),main="À Posteriori Tratamento",ylab="Densidade")

par(mfrow=c(1,1))
plot(kmfit, xlab="Tempo", ylab = "Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE, lty=c(1,2))
curve(1-pexp(x,0.0133), add=T, col='blue', lty=1, lwd=3)
curve(1-pexp(x,0.0134), add=T, col='red', lty=2, lwd=3)
curve(1-pexp(x,0.0465), add=T, col='dark green', lty=3, lwd=3)
curve(1-pexp(x,0.0648), add=T, col='purple ', lty=1, lwd=3)
curve(1-pexp(x,0.0647), add=T, col='black', lty=2, lwd=3)
curve(1-pexp(x,0.0819), add=T, col='dark orange', lty=3, lwd=3)
legend("bottomleft",c(
  "Estimativa Frequentista p/ Controle","Estimativa c/ Priori N.I. p/ Controle",
  "Estimativa c/ Priori I. p/ Controle","Estimativa Frequentista. p/ Tratamento",
  "Estimativa c/ Priori N.I. p/ Tratamento","Estimativa c/ Priori I. p/ Tratamento"), 
  ,lty = c(1,2,3,1,2,3), lwd=c(rep(3,6)), bty='n',
  col=c("blue","red","dark green","purple", "black"," dark orange"))

#
L = function(theta,t,c) {
 a = (2*pi*sqrt(2))^-(sum(c)/2) 
 b= sum(c*(log(t)-theta)^2)
 c = exp(-1/4 * b)
 e=(log(t)-theta)/sqrt(2)
 d = prod(t^(-c))*((1-pnorm(e,0,1))^(1-c))
 a*c*d
} 
par(mfrow=c(1,2))
curve(L(theta,tc,cc), from=0,to=10,xname="theta", xlab="",ylab="Valor da Verossimilhança")
curve(dgamma(x,9,0.1), col='blue', xlab="",ylab="Densidade")

L = function(theta,t,c) {
  a = (2*pi*sqrt(3))^-(sum(c)/2) 
  b= sum(c*(log(t)-theta)^2)
  c = exp(-1/6 * b)
  e=(log(t)-theta)/sqrt(3)
  d = prod(t^(-c))*((1-pnorm(e,0,1))^(1-c))
  a*c*d
} 
curve(L(theta,ct,tt), from=0,to=5,xname="theta", xlab="",ylab="Valor da Verossimilhança")
curve(dnorm(x,3,2), from=0,to=5,col='blue', xlab="",ylab="Densidade")

##
par(mfrow=c(1,1))
plot(kmfit, xlab="Tempo", ylab = "Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE, lty=c(1,2))
curve(1-plnorm(x,4.77,2.27), add=T, col='blue', lty=1, lwd=3)
curve(1-plnorm(x,3.107,2),col='dark green',lty=2,lwd=3,add=T)

curve(1-plnorm(x,2.3279,1.6038),col='purple',lty=1,lwd=3,add=T)
curve(1-plnorm(x,1.927,3),col='dark orange',lty=2,lwd=3,add=T)
legend("bottomleft",c(
  "Estimativa Frequentista p/ Controle","Estimativa c/ Priori I. p/ Controle",
  "Estimativa Frequentista. p/ Tratamento","Estimativa c/ Priori I. p/ Tratamento"), 
  ,lty = c(1,2,1,2), lwd=c(3,3,3,3), bty='n',
  col=c("blue","dark green","purple"," dark orange"))

##
par(mfrow=c(1,1))
plot(kmfit, xlab="Tempo", ylab = "Sobrevivência Estimada",
     mark.time = T, conf.int = FALSE, lty=c(1,2))
curve(1-plnorm(x,3.107,2),col='black',lty=2,lwd=3,add=T)
curve(1-pexp(x,0.0465), add=T, col='black', lty=1, lwd=2)
curve(1-pexp(x,0.0819), add=T, col='dark green', lty=1, lwd=2)
curve(1-plnorm(x,1.927,3),col='dark green',lty=2,lwd=3,add=T)
legend("bottomleft",c(
  "Estimativa Bayesiana c/ Exp p/ Controle","Estimativa Bayesiana c/ LN p/ Controle",
  "Estimativa Bayesiana c/ Exp p/ Tratamento","Estimativa Bayesiana c/ LN p/ Tratamento"), 
  ,lty = c(1,2,1,2), lwd=c(2,3,2,3), bty='n',
  col=c("black","black","dark green"," dark green"))

#criterio para decidir curva para tratamento
#grafico c km
a_c = 0.0465
a_t = 0.0819
mu_c = 3.107
mu_t = 1.927
sig2_c = 2
sig2_t = 3
sobrev_e_c = exp(-tc*a_c)
sobrev_l_c = 1-pnorm((log(tc)-mu_c)/(sqrt(sig2_c)))
sobrev_e_t = exp(-tt*a_t)
sobrev_l_t = 1-pnorm((log(tt)-mu_t)/(sqrt(sig2_t)))
kmfit = survfit(dados~tipo)
summary(kmfit)
tt
ct
km_c = c(1,1,rep(0.846,13))
km_t = c(rep(0.786,5),0.698,0.611,0.524,rep(0.437,6))
par(mfrow=c(2,2))
plot(pch=18,km_c, sobrev_e_c, xlab="Estimativas KM",xlim=c(0.5,1),ylim=c(0.5,1), ylab="Estimativas Paramétricas", main="Caso Controle: Exp")
abline(a=0,b=1)

plot(pch=18,col='red',km_c, sobrev_l_c,xlab="Estimativas KM", xlim=c(0.5,1),ylim=c(0.5,1),ylab="Estimativas Paramétricas", main="Caso Controle: LN")
abline(a=0,b=1)

plot(pch=18,km_t,col='red', sobrev_e_t,xlab="Estimativas KM", xlim=c(0.5,1),ylim=c(0.5,1),ylab="Estimativas Paramétricas", main="Caso Tratamento: Exp")
abline(a=0,b=1)

plot(pch=18,km_t, sobrev_l_t,xlab="Estimativas KM",xlim=c(0.5,1),ylim=c(0.5,1), ylab="Estimativas Paramétricas", main="Caso Tratamento: LN")
abline(a=0,b=1)


#bic controle
L = function(theta,t,c) theta^(sum(c))*exp(-(sum(t)*theta))
par=0.0133
res = optim(par=par, fn=L, t=tc, c=cc, method='Nelder-Mead')
res$par

b_e_c = 1*log(length(tc)) -2*log(0.35245)

par=0.0648
res = optim(par=par, fn=L, t=tt, c=ct, method='Nelder-Mead')
res$par

b_e_t = 1*log(length(tt)) -2*log(0.47304)

L = function(theta,t,c) {
  a = (2*pi*sqrt(2))^-(sum(c)/2) 
  b= sum(c*(log(t)-4.77)^2)
  c = exp(-1/4 * b)
  e=(log(t)-4.77)/sqrt(2)
  d = prod(t^(-c))*((1-pnorm(e,0,1))^(1-c))
  a*c*d
} 

par=4.77
res = optim(par=par, fn=L, t=tt, c=ct, method='Nelder-Mead')
res$par

b_e_c = 1*log(length(tc)) -2*log(0.35245)


L = function(theta,t,c) {
  a = (2*pi*sqrt(3))^-(sum(c)/2) 
  b= sum(c*(log(t)-theta)^2)
  c = exp(-1/6 * b)
  e=(log(t)-theta)/sqrt(3)
  d = prod(t^(-c))*((1-pnorm(e,0,1))^(1-c))
  a*c*d
} 

##### estimativas frequentistas 
ajust1 = survreg(Surv(tc,cc)~1,dist='exponential')
ajust1 = survreg(Surv(tc,cc)~1,dist='lognormal')
ajust1 = survreg(Surv(tt,ct)~1,dist='exponential')
ajust1 = survreg(Surv(tt,ct)~1,dist='lognormal')
#alpha = 4.3175 -1.5813TipoEsteroide
