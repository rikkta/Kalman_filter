library(dplyr)
library(timeSeries)
library(ggplot2)
library(lattice)
library(smooth)
library(TTR)
library(TSstudio)
library(photobiologyWavebands)
library(ggspectra)
library(ggfortify)
library(forecast)
library(dlm)
library(zoo)
library(ggthemes)
library(IRdisplay)
library(magrittr)
library(tidyverse)
library(scales)
library(gridExtra)


df <- read.csv("./GOOG.csv")
# Ensuite, le fichier goog a été chargé. Cela contient le cours de l'action
# Google du 01/01/2005 au 08/02/2021. Les données utilisées dans tous les 
# modèles correspondent à la colonne Open

glimpse(df)

colSums(is.na(df))

#Graphique linéaire
df1 <- ts(df$Open,start = c(2004,5), end =c(2021,2) ,frequency = 24)
#df3 <-  ts(df,start = c(2004,5), end =c(2021,2) ,frequency = 24)
#plot.ts(df3)
xyplot(df1, ylab = "Prix $", main = "graphique de série chronologique pour le prix de google")
#plot_ly(x = as.Date(df$Date, format= "%Y-%m-%d"),y=~df$Open)
#Ici, les données sont sur une fréquence mensuelle (12 mois) pour faciliter le calcul.

#figure<--------------
#Le graphique linéaire montre des prix fluctuants tout au long avec une forte volatilité.

#
#Diagramme de densité et QQ

hist(df1, prob =TRUE, 12)
lines(density(df1))
qqnorm(df1)
qqline(df1)
#Le diagramme de distribution comprenant la densité et le diagramme QQ normal ci-dessous 
#montre clairement que la distribution des données n'est pas normale.
#plot_ly(x = df1, type = "histogram")
#_____________________________________________________
# fig <- df %>% plot_ly(x = ~df$Date, type="candlestick",
#                       open = ~df$Open, close = ~df$Close,
#                       high = ~df$High, low = ~df$Low)
# fig <- fig %>% layout(title = "Basic Candlestick Chart")
# fig
#____________________________________


#Descriptive Statistic
#stat.desc(df1)

###########################################3
#MODEL BASIC RANDOM WALD
#check graphique de la série chronologique
ggplot(df, aes(Date,Open,group = 1)) + geom_path(colour='blue') +
    ggtitle("Prix des actions d'ouverture de Google") + 
    geom_smooth(method = "lm", formula = y ~ x, colour='black', linetype = "dashed")

#######Arima
# fit ARIMA modèle 
arima.model <- auto.arima(df$Open, allowdrift = T) 
arima.model
######Prevision with Arima
arima.forecast <- forecast(arima.model, h=60, level= 90)
# plot forecast for model
# arima_prev <- autoplot(arima.forecast)
# arima_prev


##ACF
# check ACF plot
#ggAcf(df$Open, type='correlation')

acf(df$Open, lag.max=40, main='prix d\'ouverture', type="correlation",plot=T,col='darkgreen')


# Normalisation des données
# on normalise l'ensemble de données en utilisant la moyenne et la std. dev.

df1_stationary = rnorm(length(df1), mean=1, sd=1)
df1_trend = cumsum(rnorm(length(df1), mean=1, sd=4))+ df1/100

df1_stationary <- df1_stationary/max(df1_stationary)
df1_trend <- df1_trend/max(df1_trend)

#Fonction d'autocorrélation (ACF)

#plot.new()
#frame()
#par(mfcol=c(2,2))

plot(df1_stationary,
     type = 'l', col='blue', 
     xlab = 'time', ylab='changement de l\'indice des prix de Google',
     main = 'stationnarité')
acf(df1_stationary, lag.max=length(df1_stationary),
    xlab = 'lag #', ylab='ACF', main = '')

plot(df1_trend,
     type = 'l', col='blue', 
     xlab = 'time', ylab='changement de l\'indice des prix de Google',
     main = 'tendance')
acf(df1_trend, lag.max=length(df1_trend),
    xlab = 'lag #', ylab='ACF', main = '')

#########################
#Décomposition des séries chronologiques
###################
df1SMA8 <- SMA(df1,n=8) # lissage avec moyenne mobile 8
#plot.ts(df1SMA8)

df1comp <- decompose(df1SMA8)
#plot(df1comp, yax.flip=TRUE)


###########################################################
#####################||Ajustement saisonnier|##############
###########################################################
df1.Comp.seasonal <- sapply(df1comp$seasonal, nchar)
df1SeasonAdj <- df1 - df1.Comp.seasonal
#plot.ts(df1SeasonAdj)

########tts 
#Train/Test split
split_df1 <- ts_split(ts.obj = df1, sample.out = 12)
training <- split_df1$train
test = split_df1$test

#Model fitting and prediction
train <- StructTS(training, type = c("BSM"),
                  fixed = c(0.1, 0.001, NA, NA), 
                  optim.control = list(trace=TRUE))

# autoplot(training, series = 'Training data') + autolayer(fitted(train,h=12), series = '12-step fitted values')
# 
# checkresiduals(train)

# df2 <- ts(df$Open, frequency = 12)
# xyplot(df2, ylab = "Prix $", main = "Diagramme de séries chronologiques")

######################
#Filtre de Kalman##
####################

sm <- tsSmooth(train)
fm <- fitted(train)
#pm <- forecast(train,h=12)
plot(df1)
lines(sm[,1],col = 'blue')
lines(fm[,1],col = 'red' )
#lines(sm[,2],col = 'green' )
training.sa <- df1 - sm[,1]
training.sa1 <- sm[, 1]- fm[,2] 
lines(training.sa1, col = 'green')
lines(training.sa, col = 'black')
legend('topleft', col = c('blue', 'red','green') , lty = 1,
       legend = c("Niveau filtré", "Niveau lissé","Niveau prédict"))


#############
x <- training
miss <- sample(1:length(x), 12)
x[miss] <- NA
estim <- sm[,1] + sm[, 2]
plot(x, ylim=range(df1))
points(time(x)[miss], estim[miss], col = 'red', pch = "o")
points(time(x)[miss], df1[miss], col = 'blue', pch = "o")
legend('topleft', pch = "o", col = c(2,1), legend = c('Estimation du bruit', 'Bruit'))
##############################
# décomposition de la structure de base de la serie lissée
#plot(sm, main = 'décomposition de la structure de base')
#mtext(text = 'décomposition de la structure de base', side = 3, adj = 0, line = 1)

####################################################
#####ON PEUT METTRE ARIMA ICI#######################
####################################################

#prediction#########
# sm %>%
# forecast(h=12) %>%
# autoplot(sm) + autolayer(test)
#######################

############DLM
# forecasting using state sapce models
#build model for state space
model.build <- function(p) {
    return(
        dlmModPoly(2, dV=p[1], dW=p[2:3]) +
        dlmModSeas(12, dV=p[4])
    )
}
# 
#######Convergance du modele
model.mle <- dlmMLE(training, parm=c(0.1, 0.001, 1, 1), build=model.build)
#if(model.mle$convergence==0) print("convergé") else print("n'a pas convergé")

#####################
###  Filtre de Kalman
#####################
model.mle$par
model.fit <- model.build(model.mle$par)
model.filtered <- dlmFilter(training , model.fit)
model.smoothed <- dlmSmooth(training, model.fit)

n <- 2*12
model.forecast <- dlmForecast(model.filtered, nAhead=n)

x <- index(training)
xf <- seq(max(x), max(x)+n/12, 1/12)
aa <- model.forecast$a[,-1]*(-1)
aa <- cbind(model.forecast$a[,1], aa)
a <- drop(model.forecast$a%*%t(FF(model.fit)))
a <- c(tail(training,1), a)
df <- rbind(
    data.frame(x=x, y=as.numeric(training), series="original"),
    data.frame(x=x, y=apply(model.filtered$m[-1,1:2], 1, sum), series="filtrée"),
    data.frame(x=x, y=apply(model.smoothed$s[-1,1:2], 1, sum), series="lissée"),
    data.frame(x=xf, y=a, series="prédite")
)
g.dlm <- ggplot(subset(df, x>2004), aes(x=x, y=y, colour=series)) + geom_line()
g.dlm


####################################################
########### forecasting using arima ##########
####################################################
model <- auto.arima(training)
model.forecast1 <- forecast(model, h = 6*12)
plot(model.forecast1)

#####ERROR FILTER
filterGoog <- dlmFilter(training, model.fit )
plot(residuals(filterGoog, sd = FALSE), type = "o", ylab = "Erreur de prediction standarisée")
abline(h = 0)
