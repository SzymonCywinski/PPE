###
### today <- Sys.Date()
first.day <- as.Date(last(d$date))
### 90 days ago
first.day <- today - 90
tt<- format(today, "%Y-%m-%d")
fd<- format(first.day, "%Y-%m-%d")

## d is data frame
d <- d %>% filter(as.Date(date, format="%Y-%m-%d") > fd) %>% as.data.frame

##
# country.codes = c ('PL', 'CS')
d2 <- d %>% filter (id %in% country.codes) %>% as.data.frame

##
