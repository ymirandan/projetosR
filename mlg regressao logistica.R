#z = c((1:30)^2)
#plot(x,z)

#w = c(log(1:30))
#plot(x,w)

#k=c(seq(1,15,3),seq(15,1,-3))
#x=c(1:length(k))
#plot(x,k)
y <- c(sqrt(1:30))
x <- c(1:30)
plot(x,y)

b_est <- function(x,y){
  m = lm(y~x)
  b1_0 = m$coefficients[1]
  b2_0 = m$coefficients[2]
  erro <- 1
  b_est <- c(b1_0,b2_0)
  b_est1 <- NULL
  i=0
  while (erro>0.00001){
    n = b_est[1] + b_est[2]*x
    mu <- n^2
    V <- diag(mu, length(x))
    W <- diag(4, length(x))
    Z <- n + solve(sqrt(W))%*%solve(sqrt(V))%*%(y-mu)
    X <- cbind(1,x)
    A <- t(X)%*%W%*%X
    B <- t(X)%*%W%*%Z
    b_est1 <- c(solve(A)%*%B)
    erro <- max(abs(b_est1 - b_est))
    b_est <- b_est1
    i=i+1
  }
lista = list(interações=i, parametros = b_est)
return(lista)
}

#glm(y~x,family=poisson(link="sqrt")) #comparando com a função

b_esti = b_est(x,y)
f = function(x) (b_esti$parametros[1]+ b_esti$parametros[2]*x)^2
m1 = lm(y~x)

plot(x,y, main="Dados de contagem: preditor é a raiz da média")
abline(m1$coefficients,lty = 2, lwd=1, col = 'red')
curve(f,col="blue",add=T)
#text(15, 4,expression(hat(y) == hat(beta[0]) + hat(beta[1])*x),col="red")
#text(14, 2,expression(hat(y) == exp(hat(beta[0]) + hat(beta[1])*x)),col="blue")


