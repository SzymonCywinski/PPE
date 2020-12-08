##---
##title: "☛☛☛ TYTUŁ ☚☚☚"
##author: "☛☛☛ AUTORZY ☚☚☚"
##date: "☛☛☛ DATA ☚☚☚"
##output: html_document
##---
##
##```{r setup, include=FALSE}
##knitr::opts_chunk$set(echo = TRUE)
##```
##☛☛☛ OPIS PROGNOZOWANEGO ZJAWISKA ☚☚☚

library ("forecast")
library ("fpp2")

## pokaż wersje pakietów
sessionInfo()

## załadowanie danych z pliku CSV zamiana na typ TS (szereg czasowy)
t <- read.csv("☛☛☛NAZWA_PLIKU.CSV☚☚☚", dec=",", sep = ';',  header=T, na.string="NA");
t <-ts(t, start=c(☛☛☛OKRES, PODOKRES ☚☚☚), frequency=☛☛☛CZĘSTOTLIWOŚĆ☚☚☚)

## Zamiast ggtdisplay można użyć: 
##  plot(t)
##  autoplot(t) 
##  ggtsdisplay(t, smooth = T)
autoplot(t, facets = TRUE) + geom_smooth()

ggseasonplot(t)
autoplot(decompose(t) )

## statystyki zbiorcze
t.mean <- mean(t, na.rm = T)
t.max <- max(t, na.rm = T)
t.min <- min(t, na.rm = T)

## Założenia prognostyczne
hmax <- ☛☛☛LICZBA☚☚☚

tl <- head (t, length(t) -hmax)
tt <- tail (t, hmax)
cat ("Koniec zbioru uczącego", end(tl))
cat ("Początek zbioru testowego", start(tt))

## Prognozowanie za pomocą trendu liniowego
## Oszacowanie modelu

m_lm <- tslm(tl ~ trend + season )
summary(m_lm)
accuracy(m_lm)

##Ocena reszt

checkresiduals(m_lm)
res_lm <- m_lm$residuals

m_lm.fitted <- m_lm$fitted.values
autoplot(m_lm.fitted, series="teoret") +  autolayer(m_lm$x, series="empir")
# p > 0.05 świadczy o autokorelacji
Box.test(res_lm, type='Ljung-Box')

m_lmf <- forecast(m_lm, h=hmax)
autoplot(m_lmf)
m_mla <- accuracy(m_lmf, tt)
m_mla

## Prognozowanie za pomocą wygładzania wykładniczego
## Oszacowanie modelu

m_es  <- ets(tl)
summary(m_es)
accuracy(m_es)

## Ocena reszt

checkresiduals(m_es)
res_es <- m_es$residuals

m_es.fitted <- m_es$fitted
autoplot(m_es.fitted, series="teoret") +  autolayer(m_es$x, series="empir")

Box.test(res_es, type='Ljung-Box')

## Wyznacznie prognoz i porównanie z wartościami ze zbioru testowgo

m_esf <- forecast(m_es, h=hmax)
autoplot(m_esf)
m_esa <- accuracy(m_esf, tt)
m_esa

## Arima

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

## Porównanie prognoz
## Prognozy naiwne
m_sn  <- snaive(tl)
res_sn <- m_sn$residuals
m_sn.fitted <- m_sn$fitted
m_snf <- forecast(m_sn, h=hmax)
m_sna <- accuracy(m_snf, tt)
m_sna

m_rwf  <- rwf(tl)
res_rwd <- m_rwf$residuals
m_rwf.fitted <- m_rwf$fitted
m_rwff <- forecast(m_rwf, h=hmax)
m_rwfa <- accuracy(m_rwff, tt)
m_rwfa

##Zestawienie ocen dopasowania (uporządkowanych wg wielkości RMSE)

A.table <- rbind( m_mla,  m_esa, m_aaa, m_sna, m_rwfa)
row.names(A.table) <- c('lm', 'lm/t', 'es', 'es/t', 'arima', 'arima/t', 'sna', 'sna/t', 'rwf', 'ref/t')
A.table <- as.data.frame(A.table)
A.table <- A.table[order(A.table$RMSE),]
A.table

## Porównanie wyników prognozowania na wykresie:

autoplot(t, series="empir") +  
  autolayer(m_lmf$mean, series="ln") +
  autolayer(m_esf$mean, series="es") +
  autolayer(m_aaf$mean, series="aa") +
  autolayer(m_snf$mean, series="sn") +
  autolayer(m_rwf$mean, series="rwf")

## tylko zbiór testowy 
autoplot(tt, series="empir") +  
  autolayer(m_lmf$mean, series="ln") +
  autolayer(m_esf$mean, series="es") +
  autolayer(m_aaf$mean, series="aa")

## koniec