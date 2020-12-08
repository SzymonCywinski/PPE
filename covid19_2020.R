##---
##title: "COVID zakażeni/zmarli"
##author: "TP"
##date: "7 12 2020"
##output: html_document
##---
##
####knitr::opts_chunk$set(echo = TRUE)
##
library ("forecast")
library ("fpp2")
library ("dplyr")

## pokaż wersje pakietów
sessionInfo()

## załadowanie danych z pliku CSV zamiana na typ TS (szereg czasowy)
t <- read.csv("covid19_2020.csv", dec=";", sep = ';',  header=T, na.string="NA");

## skracamy szereg ; zaczynam 1go października
day0 <- "2020-09-30"
t <- t %>% filter(as.Date(date, format="%Y-%m-%d") > day0 )

str(t)

firstDay <- first(t$date)
fd <- as.Date(firstDay)
week1 <- as.numeric(strftime(fd, format = "%V"))
firstDay
week1
dow1 <- as.numeric(strftime(fd, format = "%u"))
dow1

## week1 = numer tygodnia w roku 2020
c <- ts(t$newc, start=c(week1, dow1), frequency=7)
d <- ts(t$newd, start=c(week1, dow1), frequency=7)

end(c)
#####################


ggseasonplot(d)
autoplot(decompose(d) )

hmax <- 6

tl <- head (d, length(d) -hmax)
tt <- tail (d, hmax)
cat ("Koniec zbioru uczącego", end(tl))
cat ("Początek zbioru testowego", start(tt))

### ### ### ###

m_es  <- ets(tl)
summary(m_es)
accuracy(m_es)

checkresiduals(m_es)
res_es <- m_es$residuals

m_es.fitted <- m_es$fitted
autoplot(m_es.fitted, series="teoret") +  autolayer(m_es$x, series="empir")

Box.test(res_es, type='Ljung-Box')

m_esf <- forecast(m_es, h=hmax)
autoplot(m_esf)
m_esa <- accuracy(m_esf, tt)
m_esa

############################
########
## Arima
## Oszacowanie modelu

m_aa  <- auto.arima(tl)
summary(m_aa)
accuracy(m_aa)

## Ocena reszt

checkresiduals(m_aa)
res_aa <- m_aa$residuals

m_aa.fitted <- m_aa$fitted
autoplot(m_aa.fitted, series="teoret") +  autolayer(m_aa$x, series="empir")

Box.test(res_aa, type='Ljung-Box')

m_aaf <- forecast(m_aa, h=hmax)
autoplot(m_aaf)
m_aaa <- accuracy(m_aaf, tt)
m_aaa

#############

A.table <- rbind(m_esa, m_aaa)
row.names(A.table) <- c('es', 'es/t', 'arima', 'arima/t')
A.table <- as.data.frame(A.table)
A.table <- A.table[order(A.table$RMSE),]
A.table

autoplot(d, series="empir") +  
  autolayer(m_esf$mean, series="es") +
  autolayer(m_aaf$mean, series="aa") 
  
## Bardziej szczegółowy wykres 

autoplot(tt, series="empir") +  
  autolayer(m_esf$mean, series="es") +
  autolayer(m_aaf$mean, series="aa")

## koniec

