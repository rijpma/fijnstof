setwd('~/downloads/data/fijnstof/')
source('https://raw.githubusercontent.com/rijpma/opgaafrollen/master/rolfunctions.r')
options(stringsAsFactors=FALSE)

library(sp)
library(maptools)
library(deldir)
library(raster)
library(rgdal)
library(jsonlite)

path_cbsvier <- '2014-cbs-vierkant-100m/CBSvierkant100m201410.shp'
rdriehoek <- CRS("+init=epsg:28992")
wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs ")

nms <- read.table('pm10.csv', nrows=1, sep=',')
pm10 <- read.csv('pm10.csv', header=T)
names(pm10) <- c('date', paste0('', nms[-1]))
pm10$year <- as.numeric(sapply(pm10$date, function(x) strsplit(x, '/')[[1]][3]))
pm10$year <- ifelse(pm10$year < 90, pm10$year + 2000, pm10$year + 1900)
pm10 <- pm10[pm10$year >= 2000, ]

stations <- grepr('^addMarker', readLines('http://www.lml.rivm.nl/meetnet/'))
ms2 <- data.frame(nr=sapply(stations, function(s) gregexprr('NL[0-9]+', s)[1]), row.names=NULL)
ms2$lat <- as.numeric(sapply(stations, function(s) gregexprr('[0-9]+[.][0-9]+', s)[1]))
ms2$long <- as.numeric(sapply(stations, function(s) gregexprr('[0-9]+[.][0-9]+', s)[2]))
ms2$nr <- substr(ms2$nr, 5, 7)

stations <- jsonlite::fromJSON('http://www.luchtkwaliteitmetingen.nl/map/list_stations?component=PM10')
crds <- do.call(rbind, stations$station$station$geo$coordinates)
ms3 <- data.frame(lat=crds[, 1], long=crds[, 2], nr=stations$station$station$id)
colnames(pm10) %in% ms3$nr # odd

ms2 <- ms2[ms2$nr %in% names(pm10), ]
ms2 <- ms2[order(ms2$nr), ]
day <- pm10$date
yrs <- pm10$year

# De wettelijke norm is een jaargemiddelde van 40 (μg/m3). 
# Daarnaast mag het daggemiddelde jaarlijks maximaal 35 keer hoger zijn dan 50 (μg/m3).
y_cpl <- aggregate.data.frame(pm10[, grepl('\\d+', names(pm10))],
    by=list(year=yrs), function(x) sum(!is.na(x)) >= 200) # at least 200 obs
y_ave <- aggregate.data.frame(pm10[, grepl('\\d+', names(pm10))], 
    by=list(year=yrs), mean, na.rm=T)
y_nxc <- aggregate.data.frame(pm10[, grepl('\\d+', names(pm10))], 
    by=list(year=yrs), function(x) sum(x > 50, na.rm=T))
# careful, NA does not pass in sum, but corrected by adding to y_ave
year <- y_ave$year
pm10_y <- y_ave > 40 | y_nxc > 35
pm10_y[is.na(y_cpl)] <- NA

pm10_y <- pm10_y[, colnames(pm10_y) %in% ms2$nr]
pm10_y <- pm10_y[, order(colnames(pm10_y))]
y_ave <- y_ave[, colnames(y_ave) %in% ms2$nr]

stations <- SpatialPoints(cbind(ms2$long, ms2$lat), proj4string=wgs)
stations_rd <- spTransform(stations, CRSobj=rdriehoek)

nl <- readShapeSpatial('nl/nl.shp', proj4string=wgs)
nl_rd <- spTransform(nl, CRSobj=rdriehoek)

pdf('metingen.pdf')
par(mfrow=c(1, 1))
plot(nl_rd)
text(coordinates(stations_rd)[, 1], coordinates(stations_rd)[, 2], ms2$nr, col=2, cex=0.8)
par(mfrow=c(3, 3), mar=c(4, 4, 1, 1))
for (i in 2:ncol(y_ave)){
    plot(x=range(year), y=range(y_ave[, -1], na.rm=T), 
        type='n', bty='l', xlab='year', ylab='ugram/m3')
    title(main=ms2$nr[i], line=-1)
    lines(year, y_ave[, i], type='b', col=2)
}
dev.off()

nlbox <- bbox(nl_rd)
vnlist <- vector(mode="list", length(nrow(pm10_y)))
for (i in 1:nrow(pm10_y)){
    idx <- which(!is.na(pm10_y[i, ]))
    vn <- deldir(coordinates(stations_rd[idx])[, 1], 
                 coordinates(stations_rd[idx])[, 2], 
                 rw=c(nlbox))
    tiles <- tile.list(vn)
    polys <- vector(mode="list", length=length(tiles))
    for (j in 1:length(tiles)){
        polycrds <- cbind(tiles[[j]]$x, tiles[[j]]$y)
        polys[[j]] <- Polygons(list(Polygon(polycrds)), ID=ms2$nr[idx][j])
    }
    vnlist[[i]] <- SpatialPolygons(polys, proj4str=CRS("+init=epsg:28992"))
}

pdf('stationsdekking.pdf')
for (vn in vnlist){
    plot(nl_rd)
    plot(vn, add=T, border=2)
    title(main=)
}
dev.off()

# big file!
# http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/publicaties/geografische-data/archief/2014/2013-kaart-vierkanten-art.htm
pop <- readShapeSpatial(path_cbsvier)
# pop <- pop[coordinates(pop)[,1] > 100000 & coordinates(pop)[,1] < 110000 &
#            coordinates(pop)[,2] > 450000 & coordinates(pop)[,2] < 460000, ]
pop@data[pop@data < 0] <- NA
ppop <- SpatialPixelsDataFrame(coordinates(pop), data=pop@data[grep('^INW', names(pop@data))], proj4string=rdriehoek)

inwvrbs <- grepr('^INW', names(pop@data))
fill <- data.frame(year=year, nstat=NA, nexposed=NA)
for (i in 1:nrow(pm10_y)){
    idx <- names(vnlist[[i]])
    fill$nstat[i] <- length(idx)
    rpop <- raster(ppop[inwvrbs[i]])
    fill$nexposed[i] <- sum(extract(rpop, vnlist[[i]], fun=sum, na.rm=T) * pm10_y[i, idx], na.rm=T)
    cat(i, '-')
}

write.csv(fill, 'fijnstofexpsr.csv')

# # distance weighted from all stations
# # 3s for 2284
# (((nrow(pop) / nrow(pop)) * 3) * nrow(pm10)) / 60 / 60
# # 1830 hours...
# distmat <- 
# for (i in 1:nrow(pop)){
#     point <- coordinates(pop[i, ])
#     dists <- spDistsN1(pts=stations_rd, pt=point, longlat=FALSE)
#     cat(i, '\n')
# }

# # assuming stable stations
# rowSums(sqrt(dists) * pm10[2:3, ], na.rm=T) / rowSums(sqrt(dists[!is.na(pm10[2:3, ])]))
# sum(sqrt(dists) * pm10[2, ], na.rm=T) / sum(sqrt(dists[!is.na(pm10[2, ])]))