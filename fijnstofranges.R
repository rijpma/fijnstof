library(data.table)
library(countrycode)

air <- data.table::fread('~/downloads/data/AirBase_v8_statistics.csv')
ctrs <- read.csv("~/dropbox/uurabo/nwcountries.csv")
iso2c <- countrycode::countrycode(ctrs$iso3c, 'iso3c', 'iso2c')
ctrs$iso2c <- countrycode::countrycode(ctrs$iso3c, 'iso3c', 'iso2c')

table(air[grep('particulate matter < 10', air$component_name, ignore.case=T), component_name])

air_pm10 <- air[grep('particulate matter < 10', air$component_name, ignore.case=T), ]
air_pm10[, ctr:=substr(station_european_code, 1, 2)]
air_pm10_nw <- air_pm10[ctr %in% iso2c, ]

air_pm10_nw[statistic_name=="maximum", max(statistic_value), by=list(ctr, statistics_year, statistic_name)]
air_pm10_nw[statistic_name=="annual mean", mean(statistic_value), by=list(ctr, statistics_year, statistic_name)]
air_pm10_nw[statistic_name=="annual mean", mean(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)]

mineur <- min(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, min(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)])
maxeur <- max(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, max(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)])
nweurg <- air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, mean(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)]

nldminpop <- 16.15e6
nldmaxpop <- 16.80e6
mineur * nldminpop
maxeur * nldmaxpop