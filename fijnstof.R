setwd('~/downloads/data/fijnstof/')
options(stringsAsFactors=FALSE)

library(sp)
library(maptools)
library(deldir)
library(raster)
library(rgdal)
library(jsonlite)

grepr <- function(pattern, x, ...){
    idx <- grep(pattern, x, ...)
    return(x[idx])
}
gregexprr <- function(pattern, string){
    # return all string matches of a regular expression
    # todo: check whether/how it work on multiple strings at once

    rgx <- gregexpr(pattern, string)
    out <- substring(string, rgx[[1]], rgx[[1]] + attr(rgx[[1]], 'match.length') - 1)
    return(out)
}

path_cbsvier <- '2014-cbs-vierkant-100m/CBSvierkant100m201410.shp'
rdriehoek <- CRS("+init=epsg:28992")
wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs ")

nl <- readShapeSpatial('nl/nl.shp', proj4string=wgs)
nl_rd <- spTransform(nl, CRSobj=rdriehoek)

# using GCN maps
#---------------

setwd('~/downloads/data/fijnstof/conc_pm10_2000-2014')
files <- list.files()

maps <- infos <- list()
widths=c(I=3, I=3, I=3, I=3, X=1, A=10, X=1, A=10, X=1, A=10, X=1, A=22, X=1, A=6, X=1, I=2, X=1, F=8, X=1, F=8, I=3, I=3, X=1, F=8, X=1, F=8)
for (file in files){
    infos[[file]] <- read.fwf(file, widths=widths, nrow=1)    
}
maps[["conc_pm10_2000.aps"]] <- read.table("conc_pm10_2000.aps", skip=1)
maps[["conc_pm10_2001.aps"]] <- read.table("conc_pm10_2001.aps", skip=1)
maps[["conc_pm10_2002.aps"]] <- read.fwf("conc_pm10_2002.aps", widths=rep(6, infos[["conc_pm10_2002.aps"]]$V21), skip=1)
maps[["conc_pm10_2003.aps"]] <- read.fwf("conc_pm10_2003.aps", widths=rep(6, infos[["conc_pm10_2003.aps"]]$V21), skip=1)
maps[["conc_pm10_2004.aps"]] <- read.fwf("conc_pm10_2004.aps", widths=rep(6, infos[["conc_pm10_2004.aps"]]$V21), skip=1)
maps[["conc_pm10_2005.aps"]] <- read.table("conc_pm10_2005.aps", skip=1)
maps[["conc_pm10_2006.aps"]] <- read.table("conc_pm10_2006.aps", skip=1)
maps[["conc_pm10_2007.aps"]] <- read.table("conc_pm10_2007.aps", skip=1)
maps[["conc_pm10_2008.aps"]] <- read.table("conc_pm10_2008.aps", skip=1)
maps[["conc_pm10_2009.aps"]] <- read.table("conc_pm10_2009.aps", skip=1)
maps[["conc_pm10_2010.aps"]] <- read.table("conc_pm10_2010.aps", skip=1)
maps[["conc_pm10_2011.aps"]] <- read.table("conc_pm10_2011.aps", skip=1)
maps[["conc_pm10_2012.aps"]] <- read.table("conc_pm10_2012.aps", skip=1)
maps[["conc_pm10_2013.aps"]] <- read.table("conc_pm10_2013.aps", skip=1)
maps[["conc_pm10_2014.aps"]] <- read.table("conc_pm10_2014.aps", skip=1)

range(sapply(maps, function(x) range(x[x > -1])))

setwd('~/downloads/data/fijnstof/')

range(unlist(lapply(maps, function(x) range(x[x >= 0]))))
polydfs <- list()
cuts <- seq(from=10, to=70, length.out=10)
pdf('fijnstofgrids.pdf')
for (file in files){
    mat <- maps[[file]]
    info <- infos[[file]]
    mat[mat==-999] <- NA
    mat <- as.matrix(mat)
    topleft = c(xmn=info$V18, xmx=info$V18 + info$V21*info$V24, ymn=info$V20 - info$V22*info$V26, ymx=info$V20)
    topleft <- topleft*1000
    r <- raster::raster(mat, xmn=topleft['xmn'], xmx=topleft['xmx'], ymn=topleft['ymn'], ymx=topleft['ymx'])
    r5km <- raster::aggregate(r, fact=5, fun=mean)
    polys5km <- raster::rasterToPolygons(r5km)
    polys5km$cut <- cut(polys5km$layer, cuts)
    labels <- RColorBrewer::brewer.pal(9, 'RdPu')[which(levels(polys5km$cut) %in% unique(polys5km$cut))]
    plot(polys5km, col=as.character(factor(polys5km$cut, labels=labels)), lwd=0.1)
    plot(nl_rd, add=T, lwd=0.5)
    legend('topleft', legend=unique(polys5km$cut), fill=labels)
    polydfs[[file]] <- polys5km
    title(main=file)
    abline(v=102552.030389, h=496472.150836)
} 
dev.off()


# big file!
# http://www.cbs.nl/nl-NL/menu/themas/dossiers/nederland-regionaal/publicaties/geografische-data/archief/2014/2013-kaart-vierkanten-art.htm
pop <- readShapeSpatial(path_cbsvier)
# pop <- pop[coordinates(pop)[,1] > 100000 & coordinates(pop)[,1] < 110000 &
#            coordinates(pop)[,2] > 450000 & coordinates(pop)[,2] < 460000, ]
pop@data[pop@data < 0] <- NA
ppop <- SpatialPixelsDataFrame(coordinates(pop), data=pop@data[grep('^INW', names(pop@data))], proj4string=rdriehoek)

polyears <- gsub('.*_|[.].*', '', names(polydfs))
inwvrbs <- grepr('^INW', names(pop@data))
fill <- data.frame(year=as.numeric(polyears), nstat=NA, exposurexpersons=NA)
for (i in 1:length(polydfs)){
    polys <- polydfs[[i]]
    rpop <- raster::raster(ppop[i])
    fill$exposurexpersons[i] <- sum(raster::extract(rpop, polys, fun=sum, na.rm=T) * polys@data, na.rm=T)

    cat(i, '-')
}

pdf('poptest.pdf')
plot(rpop)
plot(nl_rd, add=T, lwd=0.5)
axis(1)
axis(2)
plot(polys)
plot(nl_rd, add=T, lwd=0.5)
axis(1)
axis(2)
dev.off()

write.csv(fill, 'fijnstofexpsr.csv')
pdf('fijnstofexpsr.pdf')
plot(exposurexpersons ~ year, data=fill, type='b', bty='l', col=2, ylab='Persons x average exposure ug/m3')
dev.off()

# alternative: directly from measurement stations
#------------------------------------------------

nms <- read.table('pm10.csv', nrows=1, sep=',')
pm10 <- read.csv('pm10.csv', header=T)
names(pm10) <- c('date', nms[-1])
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
colnames(pm10) %in% as.character(ms3$nr) # odd

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


inwvrbs <- grepr('^INW', names(pop@data))
fill <- data.frame(year=as.numeric(polyears), nstat=NA, exposurexpersons=NA)
for (i in 1:length(vnlist)){
    idx <- names(vnlist[[i]])
    fill$nstat[i] <- length(idx)
    rpop <- raster(ppop[inwvrbs[i]])
    fill$nexposed[i] <- sum(extract(rpop, vnlist[[i]], fun=sum, na.rm=T) * pm10_y[i, idx], na.rm=T)
    cat(i, '-')
}


pdf('stationsdekking.pdf')
for (vn in vnlist){
    plot(nl_rd)
    plot(vn, add=T, border=2)
    title(main=)
}
dev.off()