#---
#title: ""
#author: "TP"
#date: "2020"
#output: html_document
#---
#  
#```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
#```

library("forecast")
library ("fpp2")

## wczytanie zbioru danych w postaci csv
z <- read.csv("MZM.csv", dec=".", sep = ';',  header=T, na.string="NA");
is.ts(z)

## noclegi gości zagranicznych
z <-ts(z[,"zagraniczni"], start=c(2015, 1), frequency=12)

is.ts(z)

frequency(z)
start(z)
end(z)

autoplot(z) + geom_smooth()
ggseasonplot(z)
autoplot(decompose(z) )

## statystyki zbiorcze
t.mean <- mean(z, na.rm = T)
t.max <- max(z, na.rm = T)
t.min <- min(z, na.rm = T)

hmax <- 6

zl <- head (z, length(z) -hmax)
zt <- tail (z, hmax)
cat ("Koniec zbioru uczącego", end(zl))
cat ("Początek zbioru testowego", start(zt))

## Prognozowanie za pomocą trendu liniowego
## Oszacowanie modelu

lm <- tslm(zl ~ trend + season )
summary(lm)
accuracy(lm)

##Ocena reszt

checkresiduals(lm)
res_lm <- lm$residuals

lm.fitted <- lm$fitted.values
autoplot(lm.fitted, series="teoret") +  autolayer(lm$x, series="empir")
# p > 0.05 świadczy o autokorelacji
Box.test(res_lm, type='Ljung-Box')

lmf <- forecast(lm, h=hmax)
autoplot(lmf)
mla <- accuracy(lmf, zt)
mla
