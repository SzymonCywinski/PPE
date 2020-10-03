# ustawiamy katalog roboczy
#setwd("C:/Users/")

# wczytanie danych w formacie tekstowym
# Albo dane <-read.csv(file="pkb.csv", header=T, dec=".", sep=",")
# View(dane)
# wybranie zmiennej (kolumny)
# szereg.pkb <- dane[,3]

dane <- scan(file= "hotele.txt");

# pokaż początek szeregu
head(dane)

# liczba obserwacji
length(dane)

# utworzenie obiektu typu ts
hotele.ts <- ts(dane, start = c(2009,1), frequency=12)

# pomoc dla funkcji ts
?ts

# data początkowa/końcowa oraz częstotliwość
start(hotele.ts)

end(hotele.ts)

frequency(hotele.ts)


str(hotele)

## wykres

plot(hotele.ts)

## załadowanie biblioteki forecast

library(forecast)

## jeżeli nie ma:
install.packages("forecast", dependencies=TRUE)

### https://www.r-bloggers.com/installing-r-packages/
### install.packages("ggplot2", lib="/data/Rpackages/")
### library(ggplot2, lib.loc="/data/Rpackages/")
### .Renviron w windows nazywa się 

## wykresy sezonowe
par(mfrow = c(2, 1))
monthplot(hotele.ts, main = "wykres miesięczny")
seasonplot(hotele.ts, year.labels = TRUE, col= rainbow(5), main="wykres sezonowy")

## Wykres autokorelacji ACF
Acf(hotele.ts)

## dekompozycja addytywna
hotele.ts.dekompozycja.add <- decompose (hotele.ts, "additive")
plot (hotele.ts.dekompozycja.add)
## ?decompose

## Podział na zbiór uczący/testowy
hotele.learn <- window (hotele.ts, end = c(2012, 3))
start (hotele.learn)

end(hotele.learn)
length(hotele.learn)

## zbiór testowy
hotele.test <- window(hotele.ts, start= c(2012, 4))

start(hotele.test)
end(hotele.test)

length(hotele.test)

## Trend liniowy / metoda NK
?tsm1

model.tsm1 <- ts.lm(hotele.learn ~ trend + season )
summary(model.ts1m)


## Model ETS (wygładzanie wykładnicze / exponental smooting)
?etc

model.ets < ets(hotele.learn)
summary(model.ets)

## Obiekty w przestrzeni roboczej

ls()


## Prognozy

?forecast

horyzont <- length(hotele.test)
tslm.prog <- forecast(model.tslm, h=horyzony)

ets.prog <- forecast(model.ets, h=horyzony)

## Wykresy prognoz
## Ustawienie parametru mfrow jako wektora dwuelementowego 
## o wartościach (2, 2)
par(mfrow = c(2,2))

plot(tslm.prognozy)
grid()

plot(ets.prognozy)
grid()

## Ocena i porównanie dokładności prognoz
kryteria <- c('RMSE', "MAE", "MAPE")

accuracy(tslm.prognozy, hotele.test)[, kryteria]

accuracy(ets.prognozy, hotele.test)[, kryteria]

## koniec

