## ---------------------------
## Programa de Contagem
## Eder, Marinalva e Terezinha
## ---------------------------

## ----------------
## [0] Bibliotecas
## ----------------

library(statmod)
library(MASS)
library(tidyverse)

## -----------------------
## [1] Leitura dos dados
## ------------------------

dados = read.csv('sensa101e112.csv',header = T, sep = ',')
names(dados)=c('amp','ang', 'med', 'Y', 'grauam','ang2','cheia', 'med4', 'lado', 'Erro')

## --------------------------------
## [1] Transformação dos dados
## --------------------------------

dados$amp = factor(dados$amp)
dados$ang = factor(dados$ang)
dados$med4 = factor(dados$med4)
dados$grauam = factor(dados$grauam)
dados$cheia = factor(dados$cheia)
dados$lado = factor(dados$lado)

## --------------------------------
## [2] Matriz dos dados
## --------------------------------

sensor.dat = data.frame(dados$Y, dados$grauam, dados$cheia,
                        dados$med4, dados$lado)

## --------------------------------
## [3] Modelo
## --------------------------------

fit.model=glm.nb(dados.Y ~ dados.grauam * dados.cheia +
                   dados.med4 + dados.lado,sensor.dat)
summary(fit.model)

## --------------------------------
## [4] Avaliação do modelo
## --------------------------------

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
par(mfrow=c(2,2))
a <- max(td)
b <- min(td)

# Distância de Cook

par(mfrow=c(1,2))
plot(di,xlab="Index", ylab="Cook's distance", pch=16)


data.frame(y1 = di, x1 = seq_along(di)) %>%
  ggplot(data = .) +
  geom_segment(aes(x = x1,
                   xend = x1,
                   y = 0,
                   yend = y1)) +
  geom_point(aes(x = x1, y = y1), size = 2.5) +
  theme_bw() +
  labs(x = "Observations number", y = "Cook's distance") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black"),
        axis.line = element_line(size = 0.5, colour = "black"))




# Residual deviance
plot(td,xlab="Index", ylab="Resídual deviance",
ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)


data.frame(y1 = td, x1 = seq_along(td)) %>%
  ggplot(data = .) +
  geom_point(aes(x = x1, y = y1), size = 2.5) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  geom_hline(yintercept = -2, linetype = "dashed") +
  scale_y_continuous(limits = c(b - 1, a + 1)) +
  theme_bw() +
  labs(x = "Observations number", y = "Residual deviance") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(color = "black", face = "bold"),
        legend.text = element_text(color = "black"),
        axis.line = element_line(size = 0.5, colour = "black"))



# Envelope binomial-negativa

par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
fi <- fit.model$theta
w <- fi*fitted(fit.model)/(fi + fitted(fit.model))
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
fi <- fit.model$theta
e <- matrix(0,n,100)
#
for(i in 1:100){
  resp <- rnegbin(n, fitted(fit.model),fi)
  fit <- glm.nb(resp ~ X)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit,type="deviance")/sqrt(1-h))}
#
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Percentil da N(0,1)",
       ylab="Componente do Desvio", ylim=faixa, pch=16, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
#------------------------------------------------------------#
tdw=sort(td)
tdw
