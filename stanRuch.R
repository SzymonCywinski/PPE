library ("dplyr")

z <- read.csv("stan_i_ruch_naturalny_ludnosci_GUS.csv", dec=".", sep = ';',  header=T, na.string="NA");

str(z)

trueMonth <- z$GUSdata %>% gsub(".* ", "", .) %>%
    as.roman() %>% as.numeric()
trueYear <- z$GUSdata %>% gsub(" .*", "", .) %>% as.numeric()

z$trueMonth <- paste0(trueYear, "-",  trueMonth, "-01")

z
