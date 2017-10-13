library("data.table")
library("maptools")
library("raster")
library("stringi")
library("readxl")

setwd('~/downloads/data/fijnstof/conc_pm10_2000-2014')
files <- list.files()

corop = readxl::read_excel("/Users/auke/Downloads/data/fijnstof/Coropindeling 2013-2016.xlsx", skip = 1)
setDT(corop)
setnames(corop, names(corop), gsub("_.*", "", names(corop)))
n = nrow(corop)
corop = rbindlist(list(corop[, 1:3], corop[, 4:7], corop[, 9:12], corop[, 14:17]), fill = TRUE)
corop[, year := rep(c(2013:2016), each = n)]
corop[, gmcode := paste0('GM', stringi::stri_pad_left(G, 4, '0'))]
corop$Corop[corop$year==2013] = corop[year != 2013, Corop][match( corop[year==2013, C], corop[year != 2013, C])]

maps <- infos <- list()
widths=c(I=3, I=3, I=3, I=3, X=1, A=10, X=1, A=10, X=1, A=10, X=1, A=22, X=1, A=6, X=1, I=2, X=1, F=8, X=1, F=8, I=3, I=3, X=1, F=8, X=1, F=8)
for (file in files){
    infos[[file]] <- read.fwf(file, widths=widths, nrow=1)    
}
maps[["2000"]] <- read.table("conc_pm10_2000.aps", skip=1)
maps[["2001"]] <- read.table("conc_pm10_2001.aps", skip=1)
maps[["2002"]] <- read.fwf("conc_pm10_2002.aps", widths=rep(6, infos[["conc_pm10_2002.aps"]]$V21), skip=1)
maps[["2003"]] <- read.fwf("conc_pm10_2003.aps", widths=rep(6, infos[["conc_pm10_2003.aps"]]$V21), skip=1)
maps[["2004"]] <- read.fwf("conc_pm10_2004.aps", widths=rep(6, infos[["conc_pm10_2004.aps"]]$V21), skip=1)
maps[["2005"]] <- read.table("conc_pm10_2005.aps", skip=1)
maps[["2006"]] <- read.table("conc_pm10_2006.aps", skip=1)
maps[["2007"]] <- read.table("conc_pm10_2007.aps", skip=1)
maps[["2008"]] <- read.table("conc_pm10_2008.aps", skip=1)
maps[["2009"]] <- read.table("conc_pm10_2009.aps", skip=1)
maps[["2010"]] <- read.table("conc_pm10_2010.aps", skip=1)
maps[["2011"]] <- read.table("conc_pm10_2011.aps", skip=1)
maps[["2012"]] <- read.table("conc_pm10_2012.aps", skip=1)
maps[["2013"]] <- read.table("conc_pm10_2013.aps", skip=1)
maps[["2014"]] <- read.table("conc_pm10_2014.aps", skip=1)
maps[["2015"]] <- read.table("conc_pm10_2015.aps", skip=1)
maps[["2016"]] <- read.table("conc_pm10_2016.aps", skip=1)

names(infos) = gsub(".*_|\\.aps", "", names(infos))

popmaps = list()
# popmaps[["2003"]] = maptools::readShapeSpatial("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2003-buurt-kaart/gem2003.shp")
# popmaps[["2004"]] = maptools::readShapeSpatial("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2004-buurtkaart-data/gem_2004_gen.shp")
popmaps[["2013"]] = maptools::readShapeSpatial("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2013/wijk_2013.shp")
popmaps[["2014"]] = maptools::readShapeSpatial("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2014/wijk_2014.shp")
popmaps[["2015"]] = maptools::readShapeSpatial("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2015/wijk_2015.shp")
popmaps[["2016"]] = maptools::readShapeSpatial("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2016/wijk_2016.shp")

munics = list()
for (year in names(popmaps)){
    mat <- maps[[year]]
    info <- infos[[year]]
    mat[mat==-999] <- NA
    mat <- as.matrix(mat)
    topleft = c(xmn=info$V18, xmx=info$V18 + info$V21*info$V24, ymn=info$V20 - info$V22*info$V26, ymx=info$V20)
    topleft <- topleft*1000
    r <- raster::raster(mat, xmn=topleft['xmn'], xmx=topleft['xmx'], ymn=topleft['ymn'], ymx=topleft['ymx'])
    popmaps[[year]]@data$pm10m = raster::extract(r, popmaps[[year]], fun = mean, na.rm=T)
    popmaps[[year]]@data$pm10s = raster::extract(r, popmaps[[year]], fun = sum, na.rm=T)
    # sum? 
    cat(year, '\n')
}

tabs = lapply(popmaps, data.table::as.data.table)
tabs[['2013']]$year = 2013
tabs[['2014']]$year = 2014
tabs[['2015']]$year = 2015
tabs[['2016']]$year = 2016
pop = data.table::rbindlist(tabs, fill = TRUE)
pop[AANT_INW < 0, AANT_INW := NA]
pop[, GM_CODE := as.character(GM_CODE)]

pop = merge(pop, corop, by.x =c("GM_CODE", "year"), by.y = c("gmcode", "year"), all.x = T)
pop[is.na(Corop), list(GM_NAAM, Corop, year)]
pop = pop[!is.na(Corop), ]

# check values

out = pop[!is.na(AANT_INW), list(pm10 = sum(pm10m * AANT_INW) / sum(AANT_INW)), by = list(year, Corop, C)]
out2 = pop[!is.na(AANT_INW), list(pm10 = mean(pm10m)), by = list(year, Corop, C)]
plot(out$pm10, out2$pm10)

hist(pop$pm10m)
m13 = unlist(maps[['2013']])
m14 = unlist(maps[['2014']])
m15 = unlist(maps[['2015']])
m16 = unlist(maps[['2016']])

pdf("../fijnstofdistr.pdf", width = 10, height = 10)
par(mfrow = c(2, 2))
hist(m13[m13 > 0], breaks = 100, main = "2013, unweighted neigbourhood means")
hist(m14[m14 > 0], breaks = 100, main = "2014, unweighted neigbourhood means")
hist(m15[m15 > 0], breaks = 100, main = "2015, unweighted neigbourhood means")
hist(m16[m16 > 0], breaks = 100, main = "2016, unweighted neigbourhood means")

hist(pop[year == 2013, pm10m], breaks = 100, main = "2013, GCN means")
hist(pop[year == 2014, pm10m], breaks = 100, main = "2014, GCN means")
hist(pop[year == 2015, pm10m], breaks = 100, main = "2015, GCN means")
hist(pop[year == 2016, pm10m], breaks = 100, main = "2016, GCN means")

dev.off()

data.table::fwrite(out, "~/downloads/data/fijnstof/fijnstof20136_corop.csv")


gem13 = readxl::read_excel("/Users/auke/Downloads/2013-gemeenten-alfabetisch-per-provincie.xls")
gem14 = readxl::read_excel("/Users/auke/Downloads/2014-gemeenten-alfabetisch-per-provincie.xls")
gem15 = readxl::read_excel("/Users/auke/Downloads/Gemeenten alfabetisch per provincie 2015.xls")
gem16 = readxl::read_excel("/Users/auke/Downloads/gemeenten-alfabetisch-2016-3.xls")
gem13$GM_CODE = paste0("GM", gem13$Gemcode)
gem14$GM_CODE = paste0("GM", gem14$prov_Gemcode)
gem15$GM_CODE = paste0("GM", gem15$Gemeentecode)
gem16$GM_CODE = paste0("GM", gem16$Gemeentecode)
gem13$year = 2013
gem14$year = 2014
gem15$year = 2015
gem16$year = 2016

gem13$prov = gem13$provcodel
gem14$prov = gem14$provcodel
gem15$prov = gem15$Provincienaam
gem16$prov = gem16$Provincienaam

gem = data.table::rbindlist(list(gem13, gem14, gem15, gem16))

pop = merge(pop, gem, by = c("GM_CODE", "year"), all.x=T)
pop[is.na(prov), list(GM_NAAM, prov)]
pop = pop[!is.na(prov), ]

out = pop[!is.na(AANT_INW), list(pm10 = sum(pm10m * AANT_INW) / sum(AANT_INW)), by = list(year, prov)]
data.table::fwrite(out, "~/downloads/data/fijnstof/fijnstof20136_provincie.csv")



# pop$prov = gem15$Provincienaam[match(pop$GM_CODE, paste0("GM", gem15$Gemeentecode))]

# pop[, list(GM_NAAM, prov)]


# tabs[['2014']]$AANT_INW[tabs[['2014']]$AANT_INW < 0] <- NA
# tabs[['2015']]$AANT_INW[tabs[['2015']]$AANT_INW < 0] <- NA
# tabs[['2016']]$AANT_INW[tabs[['2016']]$AANT_INW < 0] <- NA



# lapply(tabs, function(x) head(x$AANT_INW))
# lapply(tabs, function(x) head(x$AANT_INW))
# tabs[['2015']]

# pop15 = popmaps[["2015"]]
# pop15@data$AANT_INW[pop15@data$AANT_INW < 0] = NA
# library("readxl")

# pop15@data = data.frame(pop15@data, gem[match(pop15@data$GM_CODE, paste0("GM", gem$Gemeentecode)), ])

# aggregate(cbind(pm10, AANT_INW) ~ GM_CODE, data=pop15@data, mean)

# p15 = as.data.table(pop15@data)
# p15[!is.na(AANT_INW) & AANT_INW > 0, pm10m * AANT_INW]
# p15[!is.na(AANT_INW) & AANT_INW > 0, sum(pm10m * AANT_INW) / sum(AANT_INW), by = Provincienaam]
# p15[!is.na(AANT_INW) & AANT_INW > 0, sum(pm10m * AANT_INW) / sum(AANT_INW)]



# p15[!is.na(AANT_INW) & AANT_INW > 0, sum(pm10m / AANT_INW), by = Provincienaam][order(V1)]
# p15[!is.na(AANT_INW) & AANT_INW > 0, pm10s / AANT_INW]

# p15[!is.na(AANT_INW) & AANT_INW > 0, mean(pm10s / AANT_INW), by = Provincienaam][order(V1), ]
# p15[!is.na(AANT_INW) & AANT_INW > 0, sum(pm10s) / sum(AANT_INW), by = Provincienaam][order(V1), ]

# plot(p15[!is.na(AANT_INW), sum(pm10s) / sum(AANT_INW), by = Provincienaam][order(V1), V1])
# plot(p15[!is.na(AANT_INW), sum(pm10m * AANT_INW) / sum(AANT_INW), by = Provincienaam][order(V1), V1])

# p15[, lapply(.SD, mean, na.rm=T), by = GM_CODE, .SDcols = c("pm10", "AANT_INW")]
# duplicated(p15[, list(GM_CODE, AANT_INW)])
# duplicated(p15[, list(GM_CODE)])
# p15[duplicated(GM_CODE), list(GM_CODE, AANT_INW)]
# plot(pop15[pop15$GM_CODE == "GM0007", ])

# out = p15[!is.na(AANT_INW), list(pm10 = sum(pm10 * AANT_INW)/sum(AANT_INW)), by = Provincienaam][order(pm10)]
# data.table::fwrite(out, "~/downloads/data/fijnstof/fijnstof2015_provincie.csv")

# pop15$pm10 * pop15*

# aggregate(cbind(pm10, AANTINW) ~ GM_2003, data=popmaps[["2003"]]@data, mean)
# aggregate(cbind(pm10, AANT_INW) ~ GM_CODE, data=popmaps[["2004"]]@data, mean)



# file = "conc_pm10_2003.aps"
#     mat <- maps[[file]]
#     info <- infos[[file]]
#     mat[mat==-999] <- NA
#     mat <- as.matrix(mat)
#     topleft = c(xmn=info$V18, xmx=info$V18 + info$V21*info$V24, ymn=info$V20 - info$V22*info$V26, ymx=info$V20)
#     topleft <- topleft*1000
#     r <- raster::raster(mat, xmn=topleft['xmn'], xmx=topleft['xmx'], ymn=topleft['ymn'], ymx=topleft['ymx'])

# g2013@data$pm10 = raster::extract(r, g2013, fun = mean, na.rm=T)
# x = aggregate(cbind(pm10, AANTINW) ~ GM_2003, data = g2013@data, mean)
# weighted.mean(x$V1, x$AANTINW)

# sum(g2013@data$pm10 * g2013@data$AANTINW) / sum(g2013@data$AANTINW)

# g2013@data$pm10xinw = g2013@data$pm10 *  g2013@data$AANTINW

# sum(g2013$pm10xinw, na.rm=T) / sum(g2013$AANTINW, na.rm=T)

# head(g2013@data)