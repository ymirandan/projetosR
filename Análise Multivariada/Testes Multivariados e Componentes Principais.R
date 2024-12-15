#install.packages("MVA")
#install.packages("HSAUR2")
#install.packages("MVN")
#install.packages("mvtnorm")
#install.packages("MVTests")
library("MVA")
library("HSAUR2")
require(MVN)
library(mvtnorm)
library(MASS)
library(MVTests)
library("psych")
#demo("Ch-MVA")

################### REPLICA LIVRO

#1.6: pagina 19


medidas = data.frame(peitoral= c(34, 37 , 38 , 36 , 38 , 43 ,40 , 38 , 40 , 41 ,
                              36 , 36 , 34 , 33 , 36 , 37 , 34 , 36 , 38 ,35),
                     cintura = c(30 , 32 , 30 , 33 , 29 , 32 , 33 , 30 , 30 , 32 ,
                               24 , 25 , 24 , 22 , 26 , 26 , 25 , 26 , 28 , 23),
                     quadril = c(32 ,37 , 36 , 39 , 33 , 38 , 42 , 40 , 37 , 39 , 35 , 37 , 37 , 34 ,
                              38 , 37 , 38 , 37 , 40 , 35 ),
                     genero = c(rep("masculino",10), rep("feminino",10)))
x = medidas[,-4]
masc = x[1:10,]
fem = x[11:20,]

describe(x)
describe(fem)
describe(masc)

#gráficos descritivos
plot(x)
cor(x)

plab = "Medida Peitoral"
clab = "Medida Cintura"
qlab = "Medida Quadril"

plot(x$peitoral ~x$cintura, xlab =clab, ylab=plab, main = "Base com Dados Globais")
rug(x$cintura, side=1)
rug(x$peitoral, side=2)

#layout(matrix(1:8,nc=2))
#sapply(colnames(x), function(y){
#      qqnorm(x[[y]], main=y)
#      qqline(x[[y]])
 # }) criar função que faz sozinho
par(mfrow=c(1,3),mar = c(5,7,5,5))
qqnorm(medidas$peitoral, main = "QQplot da VariÃ¡vel 'peitoral'");qqline(medidas$peitoral)
qqnorm(medidas$cintura, main = "QQplot da VariÃ¡vel 'cintura'");qqline(medidas$cintura)
qqnorm(medidas$quadril, main = "QQplot da VariÃ¡vel 'quadril'");qqline(medidas$quadril)

cm = colMeans(x)
s = cov(x)
d = apply(x, MARGIN=1,function(x) t(x-cm)%*% solve(s)%*% (x-cm))

par(mar = c(5,7,5,5))
plot(qchisq((1:nrow(x)-1/2)/nrow(x), df=3),sort(d),
     xlab=expression(paste("Quantil ", chi[3]^2)), 
     ylab = "DistÃ¢ncia Ordenada",  main = "GrÃ¡fico Qui Quadrado")
oups = which(rank(abs(qc-sd), ties ="random") > nrow(x)-3)
text(qc[oups],sd[oups]-1.5,names(oups))
abline(a=0,b=1)

#testes multivariados
mvn(data= x,mvnTest = "royston",alpha=0.05)

grupo = c(rep(1,10), rep(2,10)) #codificando masc = 1 e fem = 2
result = TwoSamplesHT2(x, group = grupo, alpha = 0.05)
summary(result) #vetor de medias pop masculino é diferente do feminino; apenas a variavel cintura difere entre os grupos

################### NOVA BASE
########### POTTERY + PC ##########
data("pottery")
pot = pottery[-10]
pot_pca = prcomp(pot, scale=T)
summary(pot_pca)
pairs(main = "RelaÃ§Ã£o das Componentes Principais", pot_pca$rotation[,1:4],lower.panel = NULL, xlim=c(-0.8, 0.8),ylim=c(-0.8, 1.2),
      panel=function(x,y, ...){
        text(x,y,adj = c(0.5,-0.5),col="red", cex=1.7)
        bvbox(cbind(x,y), add=TRUE)
      })
names(pot)
#usando pcs como variaveis explicativas
pot_reg = lm(pot$Al2O3 ~predict(pot_pca))
summary(pot_reg)

pot_pca$sdev^2
#predict(pot_pca)[,1]

############ USairpollution

#regressao com cps
us_pca = prcomp(USairpollution[,-1],scale=T)
us_reg = lm(USairpollution$SO2 ~predict(us_pca))
summary(us_reg)


pollution = with(USairpollution,equal.count(SO2,4))
plot(cloud(precip ~ temp * wind | pollution, panel.aspect=0.9, data=USairpollution))

############# distancia multidimensional scalling
