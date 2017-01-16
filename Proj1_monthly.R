## -*- coding: utf-8 -*-
##
## Plik MCOILWTICO.csv zawiera średxnie miesięczne ceny ropy w okresie 1986/1--2016/12
## żródło  https://fred.stlouisfed.org/series/MCOILWTICO
## wybieramy szereg `Not Seasonally Adjusted' tj. bez korekty sezonowej
ropa <- read.csv("MCOILWTICO.csv", header = T, sep = ",")

## zamieniamy kolumnę MCOILWTICO na szereg czasowy (MCOILWTICO to nazwa kolumny w pliku CSV;
## jeżeli w pliku CSV nie ma nagłówków kolumn należy użyć nazw V1, V2 itd... (V2 oznacza druga kolumnę)

## start=c(1986,1) oznacza że pierwsza obserwacja to styczeń 1986
f0 <- ts(ropa$MCOILWTICO, frequency=12, start=c(1986,1)) ## 

## #### #### Wykresy ### #### #### #### #### #### #### #### #### #### #### ###
## ocenić na wykresie czy występuje trend/sezonowość
plot(f0)

## wykres w kolorkach (po wczytaniu pakietu lattice) ####
library("lattice")
xyplot(f1);

## definiujemy zbiór uczący jako szereg f0 bez trzech ostatnich obserwacji
f1 <- window(f0,start=c(1986,1),end=c(2016,9))
## definiujemy zbiór testowy równy trzem ostatnim obserwacjom z f0
f1t <- window(f0,start=c(2016,10),end=c(2016,12))

## obejrzmy czy wszystko jest OK
## f1 powinien mieć 369 obserwacje a f1t 3 obserwacje
f1
f1t

## Prognozy ### #### #### #### #### #### ######################################
## średnia ruchoma
f1.forecast.m3 <- forecast (meanf (x = f1, h = 3))
summary(f1.forecast.m3)

## Metoda naiwna błądzenie-przypadkowe (random-walk) z dryfem
f1.forecast.rwf <- forecast (rwf(f1, h=3, drift=T))
summary(f1.forecast.rwf)
                                 
# dekompozycja na podstawie modelu regresji: trend liniowy + sezonowosc
# jeżeli nie ma sezonowości pomiń człon `+ season'
f1.tslm.s <- tslm(f1 ~ trend + season)
#summary(f1.tslm.s)
# prognoza
f1.forecast.lm <- forecast(f1.tslm.s, h=3)
plot(f1.forecast.lm)
summary(f1.forecast.lm)

## Algorytm Holta
f1.holt <- holt(f1, h = 3)
summary(f1.holt)

## Holt-Winters (jeżeli występuje sezonowość)
f1.hw <- hw(f1, h = 3, seasonal = "additive")
summary(f1.hw)

## Porównanie prognoz #### #### #### ####

# średnia ruchoma
accuracy(f1.forecast.m3, f1t);

# błądzenie przypadkowe
accuracy(f1.forecast.rwf, f1t);

# liniowy (z sezonowoscia jezeli wystepuje)
accuracy(f1.forecast.lm, f1t);

# metoda Holta
accuracy(f1.holt, f1t);

# metoda HW (jezeli wystepuje sezonowosc)
accuracy(f1.hw, f1t);

###################################################################
## Porównać RMSE i inne statystyki ocenić która metoda daje najlepsze
## wyniki; napisać krótkie sprawozdanie 1--2 strony
## Ewentualnie zastosować inne metody

## ### Linki do potencjalnych stron z danymu
## Bitcoin forecasting (volume/price)
## http://www.rdatamining.com/examples/time-series-forecasting
## 
## http://data.bitcoinity.org/markets/volume/6m?c=e&t=bar
## http://www.ecb.europa.eu/stats/exchange/eurofxref/html/index.en.html

## Federal Reserve Bank of St. Louis
## https://fred.stlouisfed.org/categories/32217
## Ceny ropy: https://fred.stlouisfed.org/series/MCOILWTICO
## średnie dzienne ceny ropy 1986/1--2016/12 => DCOILWTICO.csv
## średnie miesieczne ceny ropy (szereg bez sezonowosci) 1986/1--2016/12 ==> MCOILWTICO.csv
##
## Banki danych miesiecznych
## https://fred.stlouisfed.org/tags/series?t=monthly
## Przykłady
## Pozwolenia na budowe (New Private Housing Units Authorized by Building Permits) Not adjusted
## https://fred.stlouisfed.org/series/PERMITNSA
## Bezrobocie
## https://fred.stlouisfed.org/series/LNU03000000
## Przejechane kilometry (Vehicle Miles Travelled)
## https://fred.stlouisfed.org/series/TRFVOLUSM227NFWA
## Indeks produkcji cementu
## https://fred.stlouisfed.org/series/IPN32731N
## Indeks produkcji lodów
## https://fred.stlouisfed.org/series/IPN31152N

## Bank danych lokalnych (GUS)
## stat.gov.pl/bdl

## Dane finansowe (zwykle dzienne)
## Polska stooq
## https://stooq.pl/q/d/?s=eurpln

