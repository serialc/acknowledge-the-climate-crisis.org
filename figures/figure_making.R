library(curl)

###### SEA LEVEL rise #######################

webpage <- "https://www.epa.gov/climate-indicators/climate-change-indicators-sea-level"
data_src <- "https://www.epa.gov/system/files/other-files/2022-07/sea-level_fig-1.csv"
alt_webpage <- "https://climate.nasa.gov/vsoverlay-sea-level/"
# need to log in to retrieve
alt_src <- "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/MERGED_TP_J1_OSTM_OST_GMSL_ASCII_V51/GMSL_TPJAOS_5.1.txt"

dfn = paste0("sources/sea_level_", Sys.Date(), '.csv')
if ( !file.exists(dfn) ) {
  curl_download(data_src, destfile = dfn)
}
sealvl <- read.csv(file = dfn, skip = 6)
sealvl2 <- read.table('sources/GMSL_TPJAOS_5.1.txt', comment.char = "H", header = FALSE, sep="")

head(sealvl)
head(sealvl2)

colnames(sealvl) <- c("year", "csiro", "c_lower", "c_higher", "noaa")
colnames(sealvl2) <- c("altim_type", "file_cycle", "year_decimal", "obs_count", "obs_wcount", "gmsl", "stdev", "smth", "gmsl_adj", "gmsl_stdev", "smth2", "smth3", "smth4")
# not using sealvl2

sealvl$csiro_mm <- sealvl$csiro * 25.4 # inch to mm
sealvl$noaa_mm <- sealvl$noaa * 25.4
sealvl$c_lower_mm  <- sealvl$c_lower * 25.4
sealvl$c_higher_mm <- sealvl$c_higher * 25.4

svg(filename='outputs/mslr.svg', width=7, height=4)
par(mar=c(5,5,2,1))
plot(sealvl$year, sealvl$csiro_mm, type='l', lwd=2, xlab="Time", ylab="Sea level change (mm)", lty=0, bty='l', axes=F,
     xlim=c(min(sealvl$year), max(sealvl$year)), ylim=c(min(sealvl$c_lower_mm, na.rm=TRUE), max(sealvl$noaa_mm, na.rm = TRUE)), xaxs="i", yaxs="i",
     main=paste("Sea level rise since", min(sealvl$year)))
abline(v=seq(1880, 2020, by=20), col='grey')
abline(h=seq(0, 250, by=50), col='grey')

polyvals <- c(sealvl$c_higher_mm, rev(sealvl$c_lower_mm))
# need to exclude NA's otherwise can't make polygon
polygon(c(sealvl$year, rev(sealvl$year))[!is.na(polyvals)], c(sealvl$c_higher_mm, rev(sealvl$c_lower_mm))[!is.na(polyvals)], col=adjustcolor('steelblue2', 0.3), lty=0)
lines(sealvl$year, sealvl$csiro_mm, lwd=2, col="steelblue")
lines(sealvl$year, sealvl$noaa_mm, lwd=2, col="black")
axis(1, at=seq(min(sealvl$year), max(sealvl$year), by=20), labels = seq(min(sealvl$year), max(sealvl$year), by=20), lty = 0)
axis(2, at=seq(0, 250, by=50), labels=seq(0, 250, by=50), lty=0, las=1)
par(srt=35)
text(1962, 140, labels="Tide gauge data", pos=4, col="steelblue2")
text(1994, 150, labels="Satellite data", pos=4, col="black")
dev.off()

# Cite as:
#CSIRO (Commonwealth Scientific and Industrial Research Organisation). 2017 update to data originally published in: Church, J.A., and N.J. White. 2011. Sea-level rise from the late 19th to the early 21st century. Surv. Geophys. 32:585–602. Accessed April 2024. www.cmar.csiro.au/sealevel/sl_data_cmar.html.
#NOAA (National Oceanic and Atmospheric Administration). 2022. Laboratory for Satellite Altimetry: Sea level rise. Accessed April 2024. www.star.nesdis.noaa.gov/sod/lsa/SeaLevelRise/LSA_SLR_timeseries_global.php.

###### SEA TEMPERATURE #######################
library(rjson)
webpage <- "https://www.epa.gov/climate-indicators/climate-change-indicators-sea-surface-temperature"
data_src <- "https://www.epa.gov/sites/default/files/2021-04/sea-surface-temp_fig-1.csv"
webpage2 <- "https://climatereanalyzer.org/clim/sst_daily/"
data_src2 <- "https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_world2_sst_day.json"

dfn = paste0("sources/sea_temp_", Sys.Date(), '.csv')
if ( !file.exists(dfn) ) {
  curl_download(data_src, destfile = dfn)
}
seatmp <- read.csv(file = dfn, skip = 6)
head(seatmp)

dfn2 = paste0("sources/sea_temp_alt_", Sys.Date(), '.json')
if ( !file.exists(dfn2) ) {
  curl_download(data_src2, destfile = dfn2)
}
seatmp2 <- fromJSON(file = dfn2)
head(seatmp2)

# need to clean this up
# list contains years of data as well as the mean and std deviations
seatmp2_names <- sapply(seatmp2, "[[", 1)
seatmp2_notyears <- is.na(as.integer(seatmp2_names))
seatmp2_years <- !seatmp2_notyears
seatmp2_notyears_indicies <- which(seatmp2_notyears == TRUE)

seatmp2_values <- sapply(seatmp2, function(x) {
  #x <- seatmp2[[1]]
  # convert NULL to NA
  x$data[sapply(x$data, is.null)] <- NA
  as.numeric(unlist(x$data))
})
seatmp2_df <- data.frame(seatmp2_values)
colnames(seatmp2_df) <- seatmp2_names
head(seatmp2_df)

# convert from F to C
seatmp$tempC <- seatmp$Annual.anomaly * 5/9
seatmp$upconfC <- seatmp$Upper.95..confidence.interval * 5/9
seatmp$lowconfC <- seatmp$Lower.95..confidence.interval * 5/9

svg(filename='outputs/mslt.svg', width=7, height=4)
par(mar=c(6,5,2,1))
plot(seatmp$Year, seatmp$tempC, type='l', lwd=2, xlab="Time", ylab=expression("Sea temperature change ("*~degree*"C)"), lty=0, bty='l', axes=F,
     xlim=c(min(seatmp$Year), c(max(seatmp$Year))), ylim=c(min(seatmp$tempC), 0.6), xaxs="i", yaxs="i",
     main="Global sea surface temperature since 1880")
# straighten text
polygon(c(1971,2000,2000,1971), c(1,1,-1,-1), col="#eeeeee", lty=0)
abline(v=seq(1880, 2020, by=20), col='grey')
abline(h=seq(-0.6, 0.6, by=0.3), col='grey')
abline(h=0, col="darkslategrey")
polygon(c(seatmp$Year, rev(seatmp$Year)), c(seatmp$upconfC, rev(seatmp$lowconfC)), col=adjustcolor('steelblue2', 0.3), lty=0)
lines(seatmp$Year, seatmp$tempC, lwd=2, col="darkslategrey")
par(srt=0)
text(1970, -0.45, label="Baseline is\n1971 to 2000\naverage", pos=4)

axis(1, at=seq(1880, 2020, by=20), labels = seq(1880, 2020, by=20), lty = 0)
axis(2, at=seq(-0.6, 0.6, by=0.3), labels=seq(-0.6, 0.6, by=0.3), lty=0, las=1)
dev.off()

# Colorbrewer2 base on #showyourstripes (https://en.m.wikipedia.org/wiki/File:20180522_Color_palette_for_warming_stripes_-_ColorBrewer_9-class_single_hue.svg)
cool <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
hot  <- c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')

coolhot <- c(rev(cool[2:9]), '#eeeeee', hot[2:9])
rainbowcols <- rainbow(sum(seatmp2_years))

library(classInt)
library(TeachingDemos)
seatmp2_year_means <- colMeans(seatmp2_df[,1:sum(seatmp2_years)], na.rm=TRUE)
seatmp2_classed <- classify_intervals(seatmp2_year_means, n = length(coolhot), style="jenks")
as.integer(seatmp2_classed)

svg(filename='outputs/mslt2.svg', width=7, height=4)
par(mar=c(2.2, 4.2, 2, 1))
plot(1:nrow(seatmp2_df), seatmp2_df[,'1981'], type='n', ylim=c(min(seatmp2_df, na.rm=TRUE), max(seatmp2_df, na.rm=TRUE)),
     xlab="", xaxt="n", ylab=expression("Ocean surface temperature ("*~degree*"C)"),
     main="Global sea surface temperature since 1981")
polygon( c(1:nrow(seatmp2_df), nrow(seatmp2_df):1), c(seatmp2_df$`plus 2σ`, rev(seatmp2_df$`minus 2σ`)), lty=0, col="#ddeeff")

for( i in 1:sum(seatmp2_years)) {
  year <- seatmp2_names[seatmp2_years][i]
  mval = mean(seatmp2_df[,year], na.rm = TRUE)
  #lines(1:nrow(seatmp2_df), seatmp2_df[,year], col=coolhot[as.integer(seatmp2_classed[i])])
  #lines(1:nrow(seatmp2_df), seatmp2_df[,year], col=rainbowcols[i], lwd=2)
  lines(1:nrow(seatmp2_df), seatmp2_df[,year], col="#999999", r=1)
}
lines(1:nrow(seatmp2_df), seatmp2_df$`1982-2011 mean`, lwd=2, lty=2, col="darkslategrey")

# text
for( i in 1:sum(seatmp2_years)) {
  year <- seatmp2_names[seatmp2_years][i]
  mval = mean(seatmp2_df[,year], na.rm = TRUE)
  #shadowtext(365/sum(seatmp2_years) * i, mval, label=year, cex = 1.2, col=coolhot[as.integer(seatmp2_classed[i])], r=0.01)
  #text(365/sum(seatmp2_years) * i, mval, label=year)
  shadowtext(365/sum(seatmp2_years) * i, mval, label=year, r=0.05, col=coolhot[as.integer(seatmp2_classed[i])])
}
axis(1,at = seq(1, 366, length.out=12), labels = month.name)
dev.off()


# Cite as:
# NOAA National Centers for Environmental information, Climate at a Glance: Global Time Series, published April 2024, retrieved on April 19, 2024 from https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series 
# Data source: NOAA OISST v2.1 and Climate Reanalyzer (2024). Daily Sea Surface Temperature. Climate Change Institute, University of Maine. Retrieved on April 19, 2024 from https://climatereanalyzer.org/clim/sst_daily/

###### CO2 levels #######################

webpage <- "https://gml.noaa.gov/ccgg/trends/"
data_src <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
dfn = paste0("sources/co2_", Sys.Date(), '.csv')
if ( !file.exists(dfn) ) {
  curl_download(data_src, destfile = dfn)
}
cotwo <- read.table(data_src, sep="", stringsAsFactors = F, header = F, comment.char = '#')
head(cotwo)
colnames(cotwo) <- c("year", "month", "dec_year", "ppm", "deseasoned", "days", "stddev", "uncertainty")
cotwo[cotwo == -9.99] <- NA
cotwo[cotwo == -0.99] <- NA

## Weekly data
data_src <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_weekly_mlo.txt"
dfn = paste0("sources/co2_wkly_", Sys.Date(), '.csv')
if ( !file.exists(dfn) ) {
  curl_download(data_src, destfile = dfn)
}
cotwo_wk <- read.table(data_src, sep="", stringsAsFactors = F, header = F, comment.char = '#')
head(cotwo_wk)

colnames(cotwo_wk) <- c("year", "month", "day", "dec_year", "ppm", "days", "c1yr", "c10yr", "s1800")
cotwo_wk[cotwo_wk == -999.99] <- NA

svg(filename='outputs/co2.svg', width=7, height=4)
par(mar=c(5,5,2,1.5))
plot(cotwo$dec_year, cotwo$ppm, type='l', lwd=2, xlab="Time", ylab=expression("CO"[2]*" ppm"), lty=0, bty='l', axes=F,
     xlim=c(min(cotwo$dec_year), max(cotwo$dec_year) + 1), ylim=c(min(cotwo$ppm, na.rm=T), max(cotwo$ppm, na.rm=T)), xaxs="i", yaxs="i",
     main=expression("CO"[2]*" levels since 1958"))
abline(v=seq(1960, 2020, by=10), col='grey')
abline(h=seq(320, 460, by=20), col='grey')

par(srt=90)
text(labels="Limits to Growth Report", x = 1972, y=340, pos = 4, offset = 0.7 )
abline(v=1972, col="slategrey") 
text(labels="Kyoto Protocol", x = 1992, y=370, pos = 4, offset = 0.7 )
abline(v=1992, col="slategrey")
text(labels="Paris Agreement", x = 2015.95, y=340, pos = 4, offset = -0.7 )
abline(v=2015.95, col="slategrey")
par(srt=0)

lines(cotwo$dec_year, cotwo$ppm, lwd=2, col="steelblue2")
lines(cotwo$dec_year, cotwo$deseasoned, lwd=2, col="darkslategrey")
axis(1, at=seq(1960, 2020, by=10), labels = seq(1960, max(cotwo_wk$year), by=10), lty = 0)
axis(2, at=seq(320, 460, by=20), labels=seq(320, 460, by=20), lty=0, las=1)
dev.off()

# Cite as:
# 

###### Global Temp #######################

webpage <- "https://climate.nasa.gov/vital-signs/global-temperature/"
data_src <- "https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt"
dfn = paste0("sources/global_temp_", Sys.Date(), '.txt')
if ( !file.exists(dfn) ) {
  curl_download(data_src, destfile = dfn)
}

gtemp <- read.table(dfn, sep="", skip = 5, stringsAsFactors = F, header = F)
head(gtemp)
colnames(gtemp) <- c("year", "raw", "smth")

svg(filename='outputs/global_temp.svg', width=7, height=4)
par(mar=c(6,5,2,1))
plot(gtemp$year, gtemp$raw, type='l', lwd=2, xlab="Time", ylab=expression("Temperature anomaly ("^degree*"C)"), lty=0, bty='l', axes=F,
     xlim=c(min(gtemp$year), max(gtemp$year)+1), ylim=c(min(gtemp$raw, na.rm=T), max(gtemp$raw, na.rm=T)), xaxs="i", yaxs="i",
     main="Global temperature change")
mtext("Baseline uses 1951 to 1980 average temperature", 1, 5, cex = 0.9, adj = 0)
yearseq <- seq(1880, 2020, by=20)
valseq <-  seq(-0.4, 1, by=0.2)
abline(v=yearseq, col='grey')
abline(h=valseq, col='grey')
abline(h=0, col="darkslategrey")
lines(gtemp$year, gtemp$raw, lwd=2, col="steelblue2")
lines(gtemp$year, gtemp$smth, lwd=2, col="darkslategrey")
axis(1, at=yearseq, labels = yearseq, lty = 0)
axis(2, at=valseq, labels=valseq, lty=0, las=1)
dev.off()

###### Arctic Sea Ice Minimum #######################

webpage <- "https://nsidc.org/arcticseaicenews/charctic-interactive-sea-ice-graph/"
# https://nsidc.org/api/seaiceservice/extent/north/filled_averaged_data/2022?index=doy
data_src <- "https://nsidc.org/api/seaiceservice/extent/north/filled_averaged_data/"

# save data
dfn = paste0("sources/seaice_", Sys.Date(), '.RData')
if ( !file.exists(dfn) ) {
  data_years <- 1979:2024
  sea_ice <- lapply(data_years, function(x) {
    #x <- c(1979:2024)[1]
    jx <- fromJSON(file=paste0(data_src, x, "?index=doy"))
    jxu <- unlist(jx)
    jxu[jxu == -1] <- NA
    jxu
  })
  names(sea_ice) <- data_years
  save(sea_ice, file = dfn)
}

load(file = dfn) # sea_ice

sim <- sapply(sea_ice, min, na.rm=TRUE)
vsim <- sim[1:length(sim)-1]
years <- as.integer(names(vsim))

svg(filename='outputs/asim.svg', width=7, height=4)
par(mar=c(5,5,2,1))
plot(years, vsim, type='l', lwd=2,
     xlab="Year", ylab=expression("Area (million km"^2*")"), lty=0, bty='l', axes=F,
     xlim=c(min(years), max(years)), ylim=c(min(vsim, na.rm=T), max(vsim, na.rm=T)), xaxs="i", yaxs="i",
     main="Arctic Sea ice minimum")
yearseq <- seq(1980, 2020, by=10)
valseq <-  seq(3, 8, by=1)
abline(v=yearseq, col='grey')
abline(h=valseq, col='grey')
#lines(asim$year, asim$ice_extent, lwd=6, col="steelblue2")
lines(years, vsim, lwd=2, col="darkslategrey")
axis(1, at=yearseq, labels = yearseq, lty = 0)
axis(2, at=valseq, labels=valseq, lty=0, las=1)
dev.off()

### Second figure

# Toss out the last day of leap years and transform to df
sidf <- data.frame(sapply(sea_ice, function(x) { x[1:365] }))
head(sidf)
colnames(sidf) <- gsub(pattern = "X", replacement = "", colnames(sidf))

plot(FALSE, xlim=c(1,365), ylim=c(min(sidf, na.rm=TRUE), max(sidf, na.rm=TRUE)), xlab="", ylab="", xaxt="n", yaxt="n" )
for ( i in 1:ncol(sidf)) {
  lines(1:365, sidf[,i], col='#aaaaaa')
}
# what's the baseline period we're using?
lines(1:365, rowMeans(sidf, na.rm=TRUE), col="darkslategrey", lwd=2)
