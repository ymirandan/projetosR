
install.packages("HSAUR"); library("HSAUR")
install.packages("psych"); library("psych")

data("heptathlon", package = "HSAUR")
heptathlon
head(heptathlon)

names(heptathlon)

summary(heptathlon)


describe(heptathlon)


plot(heptathlon, pch=19)

boxplot(heptathlon$hurdles, col=2, pch=19, main="hurdles")

var(heptathlon)
cor(heptathlon)
round(cor(heptathlon[, -8]), 2)

A=cor(heptathlon[, -8])
cortest.bartlett(A, n = nrow(heptathlon))


### PCA usando a matriz de correlação R, ou seja, considerando
### as var´s padronizadas.

heptathlon_pca_R <- prcomp(heptathlon[, -8], scale = T) ## retirando a variável "score"

tr(cor(heptathlon[, -8])) ## tem que ser = p = Traço(R)=V.Total !!
sum(diag(cor(heptathlon[, -8]))) ## outra forma

print(heptathlon_pca_R)
summary(heptathlon_pca_R)
names(print(heptathlon_pca_R))
print(heptathlon_pca_R)$sdev^2
sum(print(heptathlon_pca_R)$sdev^2)

Gama=heptathlon_pca_R$rotation; Gama[,1]
round(Gama%*%t(Gama), 2)

summary(heptathlon_pca_R)
names(heptathlon_pca_R)

predict(heptathlon_pca_R)[,1:2]

plot(heptathlon_pca_R)
screeplot(heptathlon_pca_R, type = c("lines"))

plot(predict(heptathlon_pca_R)[,1:2], pch=19)

subset(predict(heptathlon_pca_R), predict(heptathlon_pca_R)[,1] > 4)
subset(predict(heptathlon_pca_R), predict(heptathlon_pca_R)[,1] < -3)

biplot(heptathlon_pca_R, 
      pc.biplot = T, cex = 0.7, expand = 0.8)

CP1_hat=predict(heptathlon_pca_R)[,1]
plot(CP1_hat, pch=19)
summary(CP1_hat)
hist(CP1_hat)
var(CP1_hat)

CP2_hat=predict(heptathlon_pca_R)[,2]
plot(CP2_hat, pch=19)
summary(CP2_hat)
hist(CP2_hat)
var(CP2_hat)

cor(CP1_hat, CP2_hat)

plot(CP1_hat, CP2_hat, pch=19)
identify(CP1_hat, CP2_hat)

heptathlon[c(1,25),]

plot(CP1_hat, heptathlon[,8], pch=19)
identify(CP1_hat, heptathlon[,8])

plot(CP2_hat, heptathlon[,8], pch=19)
identify(CP2_hat, heptathlon[,8])

cbind(CP1_hat,CP2_hat)

hc <- hclust(dist(predict(heptathlon_pca_R)[,1:3]), "sin")
plot(hc)
plot(hc, hang = -1)


#Aula dis 30/09 
#Análise de Cluster
cbind(CP1_hat,CP2_hat)
hc_sin<-hclust(dist(predict(heptathlon_pca_R)[,1:3]),"sing")
plot(hc_sin, hang = -1)
rect.hclust(hc_sin, k = 5, border = c(2,3,4,5))

d1<-(dist(predict(heptathlon_pca_R)[,1:3]))
d2<-cophenetic(hc_sing)
cor(d1,d2)

hc_ave <-hclust(dist(predict(heptathlon_pca_R)[,1:3]),"ave")
plot(hc_ave, hang = -1)
rect.hclust(hc_sin, k = 5, border = c(2,3,4,5))
d1<-(dist(predict(heptathlon_pca_R)[,1:3]))
d2<-cophenetic(hc_ave)
cor(d1,d2)

hc_com <-hclust(dist(predict(heptathlon_pca_R)[,1:3]),"com")
plot(hc_com, hang = -1)
rect.hclust(hc_sin, k = 5, border = c(2,3,4,5))

d1<-(dist(predict(heptathlon_pca_R)[,1:3]))
d2<-cophenetic(hc_com)
cor(d1,d2)


g<-cutree(hc_ave,k=5)
table(g)
teste<-cbind(heptathlon,g)
teste
pairs(teste,col=g)

names(teste)


g3<-sunset(teste,teste$g==3)
pairs(g3[,-9],pch=19)
describe(g3[,-9])
var(g3[,-9])
round(cor(g3[,-9]))

round(sapply(g3[,-9], function(x)sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100),4)
