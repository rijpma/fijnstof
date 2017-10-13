library(data.table)
library(countrycode)

air <- data.table::fread('~/downloads/data/AirBase_v8_statistics.csv')
ctrs <- read.csv("~/onedrive/uurabo/nwcountries.csv")
iso2c <- countrycode::countrycode(ctrs$iso3c, 'iso3c', 'iso2c')
ctrs$iso2c <- countrycode::countrycode(ctrs$iso3c, 'iso3c', 'iso2c')

table(air$component_name)

table(air[grep('particulate matter < 10', air$component_name, ignore.case=T), component_name])

air_pm10 <- air[grep('particulate matter < 10', air$component_name, ignore.case=T), ]
air_pm10[, ctr:=substr(station_european_code, 1, 2)]
air_pm10_nw <- air_pm10[ctr %in% iso2c, ]

air_pm10[ctr=="NL", ]

table(air_pm10$statistic_name)

air_pm10_nw[statistic_name=="maximum", max(statistic_value), by=list(ctr, statistics_year, statistic_name)]
air_pm10_nw[statistic_name=="annual mean", mean(statistic_value), by=list(ctr, statistics_year, statistic_name)]
air_pm10_nw[statistic_name=="annual mean", mean(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)]

air_pm10_eur = air_pm10_nw[statistic_name=="annual mean", list(mean = mean(statistic_value), max = max(statistic_value), min = min(statistic_value), var = var(statistic_value)), by = list(ctr, statistics_year)]
data.table::fwrite(air_pm10_eur[order(ctr, statistics_year), ], "~/downloads/data/fijnstof/fijnstof_eur.csv")

mineur <- min(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, min(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)])
maxeur <- max(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, max(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)])
nweurg <- air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, mean(statistic_value), by=list(ctr, statistics_year, statistic_name)][, range(V1)]

nldminpop <- 16.15e6
nldmaxpop <- 16.80e6
mineur * nldminpop
maxeur * nldmaxpop

mineur
maxeur

# compare with nld

nld = data.table::fread("~/downloads/data/fijnstof/fijnstof20136_corop.csv")

hist(nld$pm10, xlim=c(mineur, maxeur))

pdf("~/downloads/data/fijnstof/airbasepm10distr.pdf", width = 10, heigh = 6)
par(mfrow = c(1, 2))
hist(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, statistic_value], breaks = 100, xlab = 'pm10 station annual means', main ='NW EU')
abline(v = range(nld$pm10), col = 2)
# abline(v = range(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002 & grepl("NL", station_european_code), statistic_value]), col = 4)
hist(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002 & grepl("NL", station_european_code), statistic_value], , breaks = 50, xlab = 'pm10 station annual means', main ='NL', xlim = c(mineur, maxeur))
abline(v = range(nld$pm10), col = 2)
dev.off()

air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002 & grepl("NL", station_european_code), ][order(statistic_value)]

head(air_pm10_nw[statistic_name=="annual mean" & statistics_year > 2002, ][order(-statistic_value), list(station_european_code, statistics_year, statistic_value)], 10)

stations = data.table::fread("/Users/auke/Downloads/AirBase_v8_stations.csv")
library("maptools")
data(wrld_simpl)

pdf("~/downloads/data/fijnstof/airbasestations.pdf")
plot(wrld_simpl[wrld_simpl$ISO2 %in% iso2c, ])
points(unique(stations[air_pm10_nw, on = 'station_european_code'][, list(station_longitude_deg, station_latitude_deg)]), cex = 0.2, col = 2)
dev.off()
