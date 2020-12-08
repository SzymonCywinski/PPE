library ("forecast")
sessionInfo()

t <- read.csv("mleko.csv", dec=",", sep = ';',  header=T, na.string="NA");
t <-ts(t, start=c(2010, 1), frequency=12)
end(t)
## Analizę danych rozpoczynamy od wykresu.
## 1. Czy dane są kompletne?;
## 2. Czy występuje trend i/lub sezonowość;
## 3. Czy występują obserwacje nietypowe?;
## 4. Podać podstawowe charakterystyki, tj. średnia/maksymalna/minimalna 

## Ad 1/2/3
## https://robjhyndman.com/hyndsight/forecast7-ggplot2/
#plot(t)
#autoplot(t) 
#ggtsdisplay(t, smooth = T)
autoplot(t, facets = TRUE) + geom_smooth()

ggseasonplot(t, main="wykres sezonowy")

autoplot(decompose(t) )

## Ad 4/ statystyki zbiorcze
t.mean <- mean(t, na.rm = T)
t.max <- max(t, na.rm = T)
t.min <- min(t, na.rm = T)
t.mean
t.max
t.min

## Zbiór danych dzielimy na uczący i testowy
## zamiast window lepiej użyć head/tail
## tl <- head(t, round(length(t) * 0.8))
## h <- 99
## albo tl <- head(t, length(t) - h)
## tt <- tail(t, h)
hmax <- 6

tl <- head (t, length(t) -hmax)
tt <- tail (t, hmax)
cat ("Koniec zbioru uczącego", end(tl))
cat ("Początek zbioru testowego", start(tt))

## Prognozowanie za pomocą trendu liniowego
## 1. Oszacowanie modelu fit/summary
##    Oceniamy R2 oraz RMSE/MAPE
m_lm <- tslm(tl ~ trend + season )
summary(m_lm)
accuracy(m_lm)
# 2. Ocena reszt checkresiduals/
checkresiduals(m_lm)
res_lm <- m_lm$residuals

m_lm.fitted <- m_lm$fitted.values
autoplot(m_lm.fitted, series="teoret") +  autolayer(m_lm$x, series="empir")
# p > 0.05 świadczy o autokorelacji
Box.test(res_lm, type='Ljung-Box')

# Wnioski
# Dopasowanie R2 > 90? OK
# Dopasowanie RMSE 18.95 ?
# Występuje autokorelacjs SL
m_lmf <- forecast(m_lm, h=hmax)
autoplot(m_lmf)
m_mla <- accuracy(m_lmf, tt)
m_mla

## Prognozowanie za pomocą wygładzania wykładniczego

m_es  <- ets(tl)
summary(m_es)
accuracy(m_es)
# 2. Ocena reszt
checkresiduals(m_es)
res_es <- m_es$residuals

m_es.fitted <- m_es$fitted
autoplot(m_es.fitted, series="teoret") +  autolayer(m_es$x, series="empir")

Box.test(res_es, type='Ljung-Box')


m_esf <- forecast(m_es, h=hmax)
autoplot(m_esf)
m_esa <- accuracy(m_esf, tt)
m_esa

## Arima

m_aa  <- auto.arima(tl)
summary(m_aa)
accuracy(m_aa)
# 2. Ocena reszt
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

## czy w ogóle warto się bawić w model bardziej skomplikowany od `naivnego'
## Ponieważ szereg wykazuje się trendem i sezonowścią to raczej warto ale sprawdzmy jak bardzo

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

## Brak autokorelacji składnika losowego

## Najniższa wartość RMSE / MAPE w zbiorze testowym
## Poniższe przepisujemy po prostu
A.table <- rbind( m_mla,  m_esa, m_aaa, m_sna, m_rwfa)
row.names(A.table) <- c('lm', 'lm/t', 'es', 'es/t', 'arima', 'arima/t', 'sna', 'sna/t', 'rwf', 'ref/t')
A.table <- as.data.frame(A.table)
A.table <- A.table[order(A.table$RMSE),]
A.table

## Porównujemy na wykresie
## Podobnie jak rysunek
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

##Wszystkie prognozy są systematycznie zawyżone się okazuje. Być może 
##jest to spo


## Koniec

#install.packages("fpp2")
# https://cran.r-project.org/web/packages/fpp2/index.html
library("fpp2")
autoplot(t, facets = TRUE) + geom_smooth()
