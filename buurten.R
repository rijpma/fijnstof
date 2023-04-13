library("data.table")
library("rgdal")
library("raster")
library("stringi")
library("readxl")

setwd('~/data/fijnstof/conc_pm10_2000-2014')
files <- list.files()

corop = readxl::read_excel("~/data/fijnstof/Coropindeling 2013-2016.xlsx", skip = 1)
setDT(corop)
setnames(corop, names(corop), gsub("\\.\\.\\..*", "", names(corop)))
n = nrow(corop)
corop = rbindlist(list(corop[, 1:3], corop[, 4:7], corop[, 9:12], corop[, 14:17]), fill = TRUE)
corop[, year := rep(c(2013:2016), each = n)]
corop[, gmcode := paste0('GM', stringi::stri_pad_left(G, 4, '0'))]
corop[, C := paste0("CR", stringi::stri_pad_left(C, 2, '0'))]
corop$Corop[corop$year==2013] = corop[year != 2013, Corop][match( corop[year==2013, C], corop[year != 2013, C])]

# corop2017 = corop[year == 2016]
# corop2017[, year := 2017]

# corop2018 = corop[year == 2016]
# corop2018[, year := 2018]

# corop2019 = corop[year == 2016]
# corop2019[, year := 2019]

# corop2020 = corop[year == 2016]
# corop2020[, year := 2020]

# this list is pretty long now, so let's update from
# https://opendata.cbs.nl/statline/#/CBS/nl/navigatieScherm/thema?themaNr=4450

# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83287NED/table?ts=1611931018358 # 2016
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83553NED/table?ts=1611931009268 # 2017
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83859NED/table?ts=1611930998143 # 2018
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/84378NED/table?ts=1611930990317 # 2019
# https://opendata.cbs.nl/statline/#/CBS/nl/dataset/84721NED/table?ts=1611930981206 # 2020

# corop = fread("~/data/fijnstof/corop/Gebieden_in_Nederland_2016_29012021_155559.csv")
corop2017 = fread("~/data/fijnstof/corop/Gebieden_in_Nederland_2017_29012021_155547.csv")
corop2018 = fread("~/data/fijnstof/corop/Gebieden_in_Nederland_2018_29012021_155537.csv")
corop2019 = fread("~/data/fijnstof/corop/Gebieden_in_Nederland_2019_29012021_155526.csv")
corop2020 = fread("~/data/fijnstof/corop/Gebieden_in_Nederland_2020_29012021_155507.csv")

corop2017$year = 2017
corop2018$year = 2018
corop2019$year = 2019
corop2020$year = 2020
corop20172020 = rbindlist(list(corop2017, corop2018, corop2019, corop2020))

corop20172020[, `Regio's` := NULL]
setnames(corop20172020, "Codes en namen van gemeenten/Code (code)", "gmcode")
setnames(corop20172020, "Codes en namen van gemeenten/Naam (naam)", "Gemeente")
setnames(corop20172020, "Lokaliseringen van gemeenten/COROP-gebieden/Code (code)", "C")
setnames(corop20172020, "Lokaliseringen van gemeenten/COROP-gebieden/Naam (naam)", "Corop")

corop20172020 = corop20172020[, lapply(.SD, stri_trim_both)]

corop = rbindlist(list(corop, corop20172020), use.names = TRUE, fill = TRUE)

maps = infos = list()
widths=c(I = 3, I = 3, I = 3, I = 3, X = 1, A = 10, X = 1, 
    A = 10, X = 1, A = 10, X = 1, A = 22, X = 1, A = 6, 
    X = 1, I = 2, X = 1, F = 8, X = 1, F = 8, I = 3, I = 3, 
    X = 1, F = 8, X = 1, F = 8)

for (file in files){
    infos[[file]] = read.fwf(file, widths=widths, nrow=1)    
}
maps[["2000"]] = read.table("conc_pm10_2000.aps", skip = 1)
maps[["2001"]] = read.table("conc_pm10_2001.aps", skip = 1)
maps[["2002"]] = read.fwf("conc_pm10_2002.aps", skip = 1,
    widths = rep(6, infos[["conc_pm10_2002.aps"]]$V21))
maps[["2003"]] = read.fwf("conc_pm10_2003.aps", skip = 1,
    widths = rep(6, infos[["conc_pm10_2003.aps"]]$V21))
maps[["2004"]] = read.fwf("conc_pm10_2004.aps", skip = 1,
    widths = rep(6, infos[["conc_pm10_2004.aps"]]$V21))
maps[["2005"]] = read.table("conc_pm10_2005.aps", skip = 1)
maps[["2006"]] = read.table("conc_pm10_2006.aps", skip = 1)
maps[["2007"]] = read.table("conc_pm10_2007.aps", skip = 1)
maps[["2008"]] = read.table("conc_pm10_2008.aps", skip = 1)
maps[["2009"]] = read.table("conc_pm10_2009.aps", skip = 1)
maps[["2010"]] = read.table("conc_pm10_2010.aps", skip = 1)
maps[["2011"]] = read.table("conc_pm10_2011.aps", skip = 1)
maps[["2012"]] = read.table("conc_pm10_2012.aps", skip = 1)
maps[["2013"]] = read.table("conc_pm10_2013.aps", skip = 1)
maps[["2014"]] = read.table("conc_pm10_2014.aps", skip = 1)
maps[["2015"]] = read.table("conc_pm10_2015.aps", skip = 1)
maps[["2016"]] = read.table("conc_pm10_2016.aps", skip = 1)
maps[["2017"]] = read.table("conc_pm10_2017.aps", skip = 1)
maps[["2018"]] = read.table("conc_pm10_2018.aps", skip = 1)
maps[["2019"]] = read.table("conc_pm10_2019.aps", skip = 1)
maps[["2020"]] = read.table("conc_pm10_RR2020_2002.aps", skip = 1)

names(infos) = stringi::stri_extract_first_regex(names(infos), "\\d{4}")

popmaps = list()
# popmaps[["2003"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2003-buurt-kaart/gem2003")
# popmaps[["2004"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2004-buurtkaart-data/gem_2004_gen")
popmaps[["2013"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2013/wijk_2013.shp", stringsAsFactors = FALSE)
popmaps[["2014"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2014/wijk_2014.shp", stringsAsFactors = FALSE)
popmaps[["2015"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2015/wijk_2015.shp", stringsAsFactors = FALSE)
popmaps[["2016"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2016/wijk_2016.shp", stringsAsFactors = FALSE)
popmaps[["2017"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2017/wijk_2017.shp", stringsAsFactors = FALSE)
popmaps[["2018"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2018/wijk_2018.shp", stringsAsFactors = FALSE)
popmaps[["2019"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2019/wijk_2019_v2.shp", stringsAsFactors = FALSE)
popmaps[["2020"]] = rgdal::readOGR("~/data/fijnstof/buurtkaarten/2020/wijk_2020_v1.shp", stringsAsFactors = FALSE)

nl = rgdal::readOGR("~/data/fijnstof/nl/nl.shp")

# this loop can take a while because of extract()
# pdf("~/data/fijnstof/2020update.pdf")
munics = list()
for (year in names(popmaps)){
    mat = maps[[year]]
    info = infos[[year]]
    mat[mat==-999] = NA
    mat = as.matrix(mat)
    bbox = c(xmn = info$V18, xmx = info$V18 + info$V21*info$V24, 
        ymn = info$V20 - info$V22*info$V26, ymx = info$V20)
    bbox = bbox*1000
    r = raster::raster(mat, xmn=bbox['xmn'], xmx=bbox['xmx'], ymn=bbox['ymn'], ymx=bbox['ymx'])
    # plot(r)
    # lines(popmaps[[year]])
    popmaps[[year]]@data$pm10m = raster::extract(r, popmaps[[year]], fun = mean, na.rm=T)
    popmaps[[year]]@data$pm10s = raster::extract(r, popmaps[[year]], fun = sum, na.rm=T)
    cat(year, '\n')
}
# dev.off()

tabs = lapply(popmaps, data.table::as.data.table)
tabs[['2013']]$year = 2013
tabs[['2014']]$year = 2014
tabs[['2015']]$year = 2015
tabs[['2016']]$year = 2016
tabs[['2017']]$year = 2017
tabs[['2018']]$year = 2018
tabs[['2019']]$year = 2019
tabs[['2020']]$year = 2020

pop = data.table::rbindlist(tabs, fill = TRUE)

pop[AANT_INW < 0, AANT_INW := NA]
pop[AANT_VROUW < 0, AANT_VROUW := NA]
pop[AANT_MAN < 0, AANT_MAN := NA]

pop[, GM_CODE := as.character(GM_CODE)]
pop[, code := as.numeric(stringi::stri_extract_all_regex(GM_CODE, "\\d+"))]

pop[year == 2017, list(AANT_INW, AANT_VROUW, AANT_MAN, GM_CODE, code)]
pop[year == 2018, list(AANT_INW, AANT_VROUW, AANT_MAN, GM_CODE, code)]
pop[year == 2019, list(AANT_INW, AANT_VROUW, AANT_MAN, GM_CODE, code)]
pop[year == 2020, list(AANT_INW, AANT_VROUW, AANT_MAN, GM_CODE, code)]

pop[, sum(as.numeric(AANT_INW), na.rm = TRUE), by = year]

corop$year = as.numeric(corop$year)
pop = merge(pop, corop, by.x =c("GM_CODE", "year"), by.y = c("gmcode", "year"), all.x = T)
unique(pop[is.na(Corop) & !is.na(GM_NAAM), list(GM_NAAM, Corop, year)][order(year)])

pop[, AANT_INW := as.numeric(AANT_INW)]
pop[, AANT_VROUW := as.numeric(AANT_VROUW)]
pop[, AANT_MAN := as.numeric(AANT_MAN)]
out = pop[!is.na(AANT_INW), list(pm10 = sum(pm10m * AANT_INW) / sum(AANT_INW),
                                pm10_fem = sum(pm10m * AANT_VROUW) / sum(AANT_VROUW),
                                pm10_mal = sum(pm10m * AANT_MAN) / sum(AANT_MAN)), 
    by = list(year, Corop, C)]
out2 = pop[!is.na(AANT_INW), list(pm10 = mean(pm10m)), by = list(year, Corop, C)]
plot(out$pm10, out2$pm10, xlab = "weighted", ylab = "unweighted")

# pm10 totals?
sapply(maps, function(x) sum(x[x >= 0]))

hist(pop$pm10m)
m13 = unlist(maps[['2013']])
m14 = unlist(maps[['2014']])
m15 = unlist(maps[['2015']])
m16 = unlist(maps[['2016']])
m17 = unlist(maps[['2017']])
m18 = unlist(maps[['2018']])
m19 = unlist(maps[['2019']])
m20 = unlist(maps[['2020']])

pdf("../fijnstofdistr.pdf", width = 10, height = 8)
par(mfrow = c(2, 4))
hist(m13[m13 > 0], breaks = 100, main = "2013, unweighted neigbourhood means")
hist(m14[m14 > 0], breaks = 100, main = "2014, unweighted neigbourhood means")
hist(m15[m15 > 0], breaks = 100, main = "2015, unweighted neigbourhood means")
hist(m16[m16 > 0], breaks = 100, main = "2016, unweighted neigbourhood means")
hist(m17[m17 > 0], breaks = 100, main = "2017, unweighted neigbourhood means")
hist(m18[m18 > 0], breaks = 100, main = "2018, unweighted neigbourhood means")
hist(m19[m19 > 0], breaks = 100, main = "2019, unweighted neigbourhood means")
hist(m20[m20 > 0], breaks = 100, main = "2020, unweighted neigbourhood means")

par(mfrow = c(2, 4))
hist(pop[year == 2013, pm10m], breaks = 100, main = "2013, GCN means")
hist(pop[year == 2014, pm10m], breaks = 100, main = "2014, GCN means")
hist(pop[year == 2015, pm10m], breaks = 100, main = "2015, GCN means")
hist(pop[year == 2016, pm10m], breaks = 100, main = "2016, GCN means")
hist(pop[year == 2017, pm10m], breaks = 100, main = "2017, GCN means")
hist(pop[year == 2018, pm10m], breaks = 100, main = "2018, GCN means")
hist(pop[year == 2019, pm10m], breaks = 100, main = "2019, GCN means")
hist(pop[year == 2020, pm10m], breaks = 100, main = "2020, GCN means")
dev.off()

data.table::fwrite(out[order(C, year)], "~/data/fijnstof/fijnstof201320_corop_tmf.csv")

# same thing, by gemeente
out = pop[!is.na(AANT_INW), 
    list(pm10 = sum(pm10m * AANT_INW) / sum(AANT_INW),
         pm10_fem = sum(pm10m * AANT_VROUW) / sum(AANT_VROUW),
         pm10_mal = sum(pm10m * AANT_MAN) / sum(AANT_MAN)), 
    by = list(year, GM_CODE, GM_NAAM)]
data.table::fwrite(out, "~/downloads/data/fijnstof/fijnstof201320_gem_tmf.csv")
pdf("~/downloads/data/fijnstof/fijnstof201320_gem.pdf")
for (gem in unique(out$GM_CODE[order(out$pm10)])){
    plot(pm10 ~ year, data = out, col = "lightgray",
        main = out[GM_CODE == gem, unique(GM_NAAM)])
    lines(pm10 ~ year, data = out[GM_CODE == gem],
        type = 'b', col = 2)
}
dev.off()

library("sf")
for (i in 2016:2020){
    png(paste0("~/data/fijnstof/corops", i, ".png"), width = 800, height = 1080)
    toplot = sf::st_as_sf(popmaps[[as.character(i)]])
    toplot[!is.na(toplot$GM_CODE) & toplot$WK_NAAM == "Wijk 00 Grootegast", ]
    toplot = merge(toplot, corop[year == i], by.x = "GM_CODE", by.y = "gmcode", all.x = TRUE)
    plot(toplot[, "Corop"], main = i)
    dev.off()
    cat(i)
}


# same thing, by province
# gem13 = readxl::read_excel("/Users/auke/Downloads/2013-gemeenten-alfabetisch-per-provincie.xls")
gem14 = readxl::read_excel("~/Downloads/2014-gemeenten-alfabetisch-per-provincie.xls")
gem15 = readxl::read_excel("~/Downloads/Gemeenten alfabetisch per provincie 2015.xls")
# gem16 = readxl::read_excel("~/Downloads/gemeenten-alfabetisch-2016-3.xls")
gem16 = readxl::read_excel("~/Downloads/gemeenten-alfabetisch-2016.xls")
gem17 = readxl::read_excel("~/Downloads/Gemeenten alfabetisch 2017.xls")
gem18 = readxl::read_excel("~/Downloads/Gemeenten alfabetisch 2018.xls")
gem19 = readxl::read_excel("~/Downloads/Gemeenten alfabetisch 2019.xls")
gem20 = readxl::read_excel("~/Downloads/Gemeenten alfabetisch 2020.xlsx")
# gem13$GM_CODE = paste0("GM", gem13$Gemcode)
gem14$GM_CODE = paste0("GM", gem14$prov_Gemcode)
gem15$GM_CODE = paste0("GM", gem15$Gemeentecode)
gem16$GM_CODE = paste0("GM", gem16$Gemeentecode)
gem17$GM_CODE = paste0("GM", gem17$Gemeentecode)
gem18$GM_CODE = paste0("GM", gem18$Gemeentecode)
gem19$GM_CODE = paste0("GM", gem19$Gemeentecode)
gem20$GM_CODE = paste0("GM", gem20$Gemeentecode)

# gem13$year = 2013
gem14$year = 2014
gem15$year = 2015
gem16$year = 2016
gem17$year = 2017
gem18$year = 2018
gem19$year = 2019
gem20$year = 2020

# gem13$prov = gem13$provcodel
gem14$prov = gem14$provcodel
gem15$prov = gem15$Provincienaam
gem16$prov = gem16$Provincienaam
gem17$prov = gem17$Provincienaam
gem18$prov = gem18$Provincienaam
gem19$prov = gem19$Provincienaam
gem20$prov = gem20$Provincienaam

gem = data.table::rbindlist(list(
    # gem13, 
    gem14, gem15, gem16, gem17, gem18, gem19, gem20),
    fill = TRUE)

gem[order(year), .(GM_CODE, year)]

pop = merge(pop, gem, by = c("GM_CODE", "year"), all.x = TRUE)
pop[is.na(prov), list(GM_NAAM, prov)]
pop = pop[!is.na(prov), ]

out = pop[!is.na(AANT_INW), list(pm10 = sum(pm10m * AANT_INW) / sum(AANT_INW)), by = list(year, prov)]
data.table::fwrite(out, "~/data/fijnstof/fijnstof201420_provincie.csv")

# quick check: 2020 here is indeed supposed to be worse than 2019
ggplot(out, aes(year, pm10, col = prov)) + geom_line()
toplot = rbindlist(
    list(
        `2013` = as.data.table(m13),
        `2014` = as.data.table(m14),
        `2015` = as.data.table(m15),
        `2016` = as.data.table(m16),
        `2017` = as.data.table(m17),
        `2018` = as.data.table(m18),
        `2019` = as.data.table(m19),
        `2020` = as.data.table(m20)), use.names = FALSE, idcol = "year")
toplot[m13 < 0, m13 := NA]
ggplot(toplot, aes(m13, col = year)) + geom_density(bw = 1)


out = pop[!is.na(AANT_INW), list(pm10 = sum(pm10m * AANT_INW) / sum(AANT_INW)), by = list(year)]
data.table::fwrite(out, "~/data/fijnstof/fijnstof201420_nationaal.csv")


