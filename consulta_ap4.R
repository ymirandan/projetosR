##################### 2 fatores aleatorios
r=4
a=3
b= 3

A = as.factor(c(rep(1,r),rep(2,r),rep(3,r))) #material

B = as.factor(c(rep(15,r*a),rep(70,r*a),rep(125,r*a))) #temperatura

y = c(130,155,74,180,
      150,188,159,126,
      138,110,168,160,
      34,40,80,75,
      136,122,106,115,
      174,120,150,139,
      20,70,82,58,
      25,70,58,45,
      96,104,82,60)

dados = data.frame(y,A,B);dados

#aggregate(y~material +temp,
 #         FUN = sum, data = dados)

ajuste = aov(y~ A +B + A*B, data=dados)

summary(ajuste)

QM = summary(ajuste)[1][[1]][[3]] #quadrados médios A, B, AB e E

#FA = QMA/QMAB ; FB = QMB/QMAB ; FAB = QMAB/QME

Fa = QM[1]/QM[3]

gl_ab = (a-1) * (b-1)

pf(Fa,a-1,gl_ab,lower.tail = F) #mantem h0

Fb = QM[2]/QM[3];Fb

pf(Fb,b-1,gl_ab,lower.tail = F) #rejeita h0

Fab = QM[3]/QM[4];Fab

pf(Fab,gl_ab,gl_e,lower.tail = F) #rejeita h0


sigma_chapeu = QM[4]

sigma_ab = (QM[3] - QM[4])/r

sigma_b = (QM[2] - QM[3])/(a*r)

sigma_a = (QM[1] - QM[3])/(b*r)

#grafico dos efeitos principais

library("dae")
interaction.plot(dados$A,dados$B, dados$y, type="b", pch=c(18,24,22), leg.bty="o",
                 main= "Efeito de A,B na variável resposta", ylab = "Resposta média",xlab = "Nível Fator A",trace.label = "Nível Fator B")

plot.design(y~ as.factor(A)+as.factor(B) , data= dados, xlab = "Fatores", ylab = "Média da resposta",
            main='Grafico de efeitos principais')

############################### modelos mistos
r=4
a=3
b= 3

A = as.factor(c(rep(1,r),rep(2,r),rep(3,r))) #material

B = as.factor(c(rep(15,r*a),rep(70,r*a),rep(125,r*a))) #temperatura

y = c(130,155,74,180,
      150,188,159,126,
      138,110,168,160,
      34,40,80,75,
      136,122,106,115,
      174,120,150,139,
      20,70,82,58,
      25,70,58,45,
      96,104,82,60)

dados = data.frame(y,A,B);dados #a fixo

ajuste = aov(y~ A +B + A*B, data=dados)

summary(ajuste)

QM = summary(ajuste)[1][[1]][[3]] #quadrados médios A, B, AB e E

#FA = QMA/QMAB ; FB = QMB/QME ; FAB = QMAB/QME

Fa = QM[1]/QM[3]

gl_ab = (a-1) * (b-1)

pf(Fa,a-1,gl_ab,lower.tail = F) #mantem h0

Fb = QM[2]/QM[4];Fb

pf(Fb,b-1,gl_ab,lower.tail = F) #rejeita h0

Fab = QM[3]/QM[4];Fab

pf(Fab,gl_ab,gl_e,lower.tail = F) #rejeita h0


sigma_chapeu = QM[4]

sigma_ab = (QM[3] - QM[4])/r

sigma_b = (QM[2] - QM[3])/(a*r)

#grafico dos efeitos principais

library("dae")
interaction.plot(dados$A,dados$B, dados$y, type="b", pch=c(18,24,22), leg.bty="o",
                 main= "Efeito de A,B na variável resposta", ylab = "Resposta média",xlab = "Nível Fator A",trace.label = "Nível Fator B")

plot.design(y~ as.factor(A)+as.factor(B) , data= dados, xlab = "Fatores", ylab = "Média da resposta",
            main='Grafico de efeitos principais')