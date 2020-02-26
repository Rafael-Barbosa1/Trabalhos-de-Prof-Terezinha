#--- Assunto: (Programa 2) - Ajuste do modelo Gama - para o clorofila -a
#--- Modificações: Rafael Barbosa
#--- Data: 12/09/2019



#---------- Limpando o R


rm(list = ls())


#---------- Pacotes necessários


if(!require(tidyverse)) {
  install.packages("tidyverse", dependencies <- T);
  require(tidyverse)
}


if(!require(ggpubr)) {
  install.packages("ggpubr", dependencies <- T);
  require(ggpubr)
}


#---------- Leitura dos dados

dados <- read.csv('dataset2.csv', header = T, sep = ';', dec = '.')


#---------- Matriz de dados

ID <- dados[,1]
Place <- dados[,2]
CodCycle <- factor(dados[,3])
Zone <- factor(dados[,4])
Cota <- dados[,5]
Depth <- dados[,6]
Mes <- dados[,7]
Year <- dados[,8]
Temp <- dados[,9]
TP <- dados[,10]
Chlra <- dados[,11]
Tr <- dados[,12]
Turb <- as.numeric(dados[,13])
STS <- dados[,14]
DO <- dados[,15]
Orthop <- dados[,16]
NH4 <- dados[,17]



#---------- Gráficos


#--- Boxplot


boxplot_zona <-
  dados %>%
  mutate(Zone = factor(x = Zone, labels = c("Lac","Tran","Riv"))) %>%
  ggplot(data = ., aes(x = Zone, y = Tr)) +
  geom_boxplot(colour = "black", fill = "grey") +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Zone", y = "Secchi depth (m)",
       tag = "A")




boxplot_ciclo <-
  dados %>%
  mutate(Cod_cycle = factor(x = Cod_cycle,
                            labels = c("LW","R","HW","F"))) %>%
  ggplot(data = ., aes(x = Cod_cycle, y = Tr)) +
  geom_boxplot(colour = "black", fill = "grey") +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Cycle", y = "Secchi depth (m)",
       tag = "B")



#--- Histograma

histograma <-
  dados %>%
  ggplot(data = ., aes(x = Tr)) +
  geom_histogram(colour = "black", fill = "grey", bins = 8) +
  geom_rug(sides = "b", aes(y = 0), position = "jitter", colour = "black") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Secchi depth (m)", y = "Frequency", tag = "C")


ggarrange(boxplot_zona, boxplot_ciclo, histograma, nrow = 1)



#---------- Ajuste do modelo


library(MASS)
library(ggplot2)
model1=glm(formula = Tr~CodCycle+Chlra+Zone*STS+Depth+NH4,family=inverse.gaussian(link=log))
anova(model1, test="F")
summary(model1)

#Tecnicas de diagnostico
fit.model=model1
par(mfrow=c(1,1))
#envelope normal inversa
rig <- function(n, mu = stop("no shape arg"), lambda = 1)
{
  if(any(mu<=0)) stop("mu must be positive")
  if(any(lambda<=0)) stop("lambda must be positive")
  if(length(n)>1) n <- length(n)
  if(length(mu)>1 && length(mu)!=n) mu <- rep(mu,length=n)
  if(length(lambda)>1 && length(lambda)!=n) lambda <- rep(lambda,length=n)
  y2 <- rchisq(n,1)
  u <- runif(n)
  r1 <- mu/(2*lambda) * (2*lambda + mu*y2 - sqrt(4*lambda*mu*y2 + mu^2*y2^2))
  r2 <- mu^2/r1
  ifelse(u < mu/(mu+r1), r1, r2)
}
#------------------------------------------------------------#
#
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
mu <-predict(fit.model,type="response")
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ro <- resid(fit.model,type="response")
fi <- (n-p)/sum((ro^2)/(fitted(fit.model)^3))
td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
#
e <- matrix(0,n,100)
#
for(i in 1:100){
  resp <- rig(n,mu,fi)
  fit <- glm(resp ~ X, family=inverse.gaussian(link=log))
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  ro <- resid(fit,type="response")
  phi <- (n-p)/sum((ro^2)/(fitted(fit)^3))
  e[,i] <- sort(resid(fit,type="deviance")*sqrt(phi/(1-h)))}
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
qqnorm(td, xlab="Standard normal percentiles",ylab="Deviance components", ylim=faixa, pch=16, main="")
par(new=TRUE)
#
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")





dfchla <- data.frame(qqnorm(y = td, plot.it = F), qqnorm(y = e1, plot.it = F),
                     qqnorm(y = e2, plot.it = F), qqnorm(y = med, plot.it = F))



ggplot(dfchla, aes(x)) +
  geom_point(aes(x = x, y = y), colour = "black", size = 3)+
  geom_line(aes(x = x.1, y = y.1), colour = "blue", linetype = 1)+
  geom_line(aes(x = x.3, y = y.2), colour = "blue", linetype = 1) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black"))  +
  labs(x = "Standart normal percentiles", y = "Deviance components")



#------------------------------------------------------------#
par(mfrow=c(1,3))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
soma <- resid(fit.model, type="pearson")
soma <- sum(soma^2)
fi <- (n-p)/soma
ts <- resid(fit.model,type="pearson")*sqrt(fi/(1-h))
td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
par(mfrow=c(2,2))
di <- (h/(1-h))*(ts^2)
a <- max(td)
b <- min(td)
#
plot(fitted(fit.model),h,xlab="Fit values", ylab="Leverage", pch=16)
#distancia de Cook
plot(di,xlab="Index", ylab="Cook's distance", pch=16)
eta <- predict(fit.model)
z <- eta + resid(fit.model, type="pearson")/sqrt(w)
plot(predict(fit.model),z,xlab="Preditor Linear",
     ylab="Variavel z", pch=16)
lines(smooth.spline(predict(fit.model), z, df=2))
#----------------------------



smooth_sp <- smooth.spline(predict(fit.model), z, df = 2)


dados1 <-
  data.frame(fitted_model1 = fitted(fit.model), h1 = h, di1 = di, z1 = z,
             smooth_x1 = smooth_sp$x, smooth_y1 = smooth_sp$y,
             predict1 = predict(fit.model))



leverage_fitvalues <-
  ggplot(data = dados1) +
  geom_point(aes(x = fitted_model1, y = h1), size = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Fit values", y = "Leverage")


cooks_distance <-
  ggplot(data = dados1) +
  geom_point(aes(x = seq_along(di1), y = di1), size = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Index", y = "Cook's distance")



varZ_preditlinear <-
  ggplot(data = dados1) +
  geom_point(aes(x = predict1, y = z1), size = 2) +
  geom_line(aes(x = smooth_x1, y = smooth_y1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(colour = "black", face = "bold", size = 14),
        axis.title.x = element_text(colour = "black", face = "bold", size = 14),
        axis.text = element_text(colour = "black", size = 16),
        strip.text.x = element_text(size = 12, colour = "white"),
        strip.text.y = element_text(size = 12, colour = "white"),
        legend.title = element_text(size = 16, color = "black", face = "bold"),
        legend.text = element_text(size = 16, color = "black"),
        axis.line = element_line(size = 0.5, colour = "black")) +
  labs(x = "Preditor linear", y = "Variável z")


ggarrange(leverage_fitvalues, cooks_distance, varZ_preditlinear, nrow = 1)
