## Minimalistyczne wprowdzanie do R
##

wektor <- c(2, 4, 5)

?seq 

tez.wektor <- seq(0, 10, by=1)

length(tez.wektor)

## Więcej wektorów
##  https://www.forbes.com/billionaires/
## Wektors napisów (character vector)
milioner <- c('Jeff Bezos', 'Bill Gates', 'Bernard Arnault', 'Warren Buffett', 'Larry Ellison',
"Amancio Ortega", "Mark Zuckerberg", "Jim Walton", "Alice Walton", "Rob Walton")
## Wektor liczb
majatek <- c(113, 98,76,67.5,59,55.1,54.7,54.6,54.4, 54.1, 52.7, 52.1, 50.9, 49.1, 48.9)
wiek <- c(56, 64, 71, 89, 75, 84, 35, 71, 70, 75, 64, 80, 47, 46, 66)
## Wektor niby napisów ale tak naprawdę dat
urodzony <- c( '1964-01-01', '1956-01-01', '1949-01-01', '1931-01-01', '1945-01-01',
'1936-01-01', '1985-01-01', '1949-01-01', '1950-01-01', '1945-01-01', '1956-01-01',
'1940-01-01', '1973-01-01', '1974-01-01', '1954-01-01')

## Dwuliterowe kody ISO krajów
## Niby napisów ale czynników (factors)
kraj <- c( US, US, FR, US, US, ES, US, US, US, US, US, MX, US, US, FR);
## F&I = finance and investment
branza <- c("Technology", "Technology", "Retail", "F&I", "Technology", "Retail",
   "Technology", "Retail", "Retail", "Retail", "Technology", "Telecom", "Technology", "Technology",
   "Retail")

## ramka / data-frame w nomenklaturze R
## zbiór nazwanych wektorów


forbes <- data.frame(milioner, majatek, urodzony, wiek, kraj, branza)

## struktura
str(forbes)
## wypisz forbes
forbes
## wypisz kolumnę z forbes
## po nazwie
forbes$majatek
## po numerze
forbes[, 3]

## wypisz poczatek/koniec
head(forbes)
##
tail(forbes)
## ilu
?nrow

nrow(forbes)

## Policzmy podstawowe statystyki
## srednio majtek

?mean
?median

mean(majatek)

##summary (forbes) źle
##
summary(forbes.wiek)

min(forbes.wiek)

## ?? zadziała ??
plot(forbes$wiek)

### 
###  Importowanie arkusza z pliku csv
###  Wartości brakujące
###  Kropka dziesiętna 
forbes <- read.csv("FB2020.csv", dec=".", sep = ';',  header=T, na.string="NA");

str(forbes)

## koniec
