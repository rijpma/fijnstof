library("data.table")
library("rgdal")
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

names(infos) = gsub(".*_|\\.aps", "", names(infos))

popmaps = list()
# popmaps[["2003"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2003-buurt-kaart/gem2003")
# popmaps[["2004"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2004-buurtkaart-data/gem_2004_gen")
popmaps[["2013"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2013/wijk_2013.shp", stringsAsFactors = FALSE)
popmaps[["2014"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2014/wijk_2014.shp", stringsAsFactors = FALSE)
popmaps[["2015"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2015/wijk_2015.shp", stringsAsFactors = FALSE)
popmaps[["2016"]] = rgdal::readOGR("/Users/auke/Downloads/data/fijnstof/buurtkaarten/2016/wijk_2016.shp", stringsAsFactors = FALSE)

# this loop can take a while because of extract()
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
    popmaps[[year]]@data$pm10m = raster::extract(r, popmaps[[year]], fun = mean, na.rm=T)
    popmaps[[year]]@data$pm10s = raster::extract(r, popmaps[[year]], fun = sum, na.rm=T)
    cat(year, '\n')
}

tabs = lapply(popmaps, data.table::as.data.table)
tabs[['2013']]$year = 2013
tabs[['2014']]$year = 2014
tabs[['2015']]$year = 2015
tabs[['2016']]$year = 2016
pop = data.table::rbindlist(tabs, fill = TRUE)

pop[AANT_INW < 0, AANT_INW := NA]
pop[AANT_VROUW < 0, AANT_VROUW := NA]
pop[AANT_MAN < 0, AANT_MAN := NA]

pop[, GM_CODE := as.character(GM_CODE)]
pop[, code := as.numeric(stringi::stri_extract_all_regex(GM_CODE, "\\d+"))]

pop = merge(pop, corop, by.x =c("GM_CODE", "year"), by.y = c("gmcode", "year"), all.x = T)
pop[is.na(Corop), list(GM_NAAM, Corop, year)]

pop[, AANT_INW := as.numeric(AANT_INW)]
pop[, AANT_VROUW := as.numeric(AANT_VROUW)]
pop[, AANT_MAN := as.numeric(AANT_MAN)]
out = pop[!is.na(AANT_INW), list(pm10 = sum(pm10m * AANT_INW) / sum(AANT_INW),
                                pm10_fem = sum(pm10m * AANT_VROUW) / sum(AANT_VROUW),
                                pm10_mal = sum(pm10m * AANT_MAN) / sum(AANT_MAN)), by = list(year, Corop, C)]
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

data.table::fwrite(out, "~/downloads/data/fijnstof/fijnstof20136_corop_tmf.csv")

# same thing, by province
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
