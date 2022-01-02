###### SEA LEVEL rise #######################
sealvl <- read.table("sources/GMSL_TPJAOS_4.2_199209_201907.txt", sep="", skip = 50, stringsAsFactors = F)
head(sealvl)
colnames(sealvl) <- c("altim_type", "file_cycle", "decim_year", "obs", "weight_obs", "GMSL", "stdvGMSL", "smthGMSL", "GMSL_GIA", "stdvGMSL_GIA", "smthGMSL_GIA", "smthGMSL_GIA_asrem")

plotval <- sealvl$smthGMSL_GIA_asrem - min(sealvl$smthGMSL_GIA_asrem)

svg(filename='outputs/mslr.svg', width=7, height=4)
par(mar=c(5,5,2,1))
plot(sealvl$decim_year, plotval, type='l', lwd=2, xlab="Time", ylab="Sea level change (mm)", lty=0, bty='l', axes=F,
     xlim=c(min(sealvl$decim_year) - 0.3, 2020), ylim=c(0, max(plotval) + 5), xaxs="i", yaxs="i",
     main="Sea level rise since 1993")
abline(v=seq(1995, 2020, by=5), col='grey')
abline(h=seq(0, 80, by=20), col='grey')
polygon(c(sealvl$decim_year, rev(sealvl$decim_year)), c(plotval + 4, rev(plotval - 4)), col=adjustcolor('steelblue2', 0.3), lty=0)
lines(sealvl$decim_year, plotval, lwd=2, col="darkslategrey")
axis(1, at=seq(1995, 2020, by=5), labels = seq(1995, 2020, by=5), lty = 0)
axis(2, at=seq(0, 80, by=20), labels=seq(0, 80, by=20), lty=0, las=1)
dev.off()


###### SEA TEMPERATURE #######################
# cite: Data source: NOAA, 2016
seatmp <- read.table("sources/sea-surface-temp_epa_gov.csv", sep=",", skip = 6, stringsAsFactors = F, header = T)
head(seatmp)
seatmp$tempC <- seatmp$Annual.anomaly * 5/9
seatmp$upconfC <- seatmp$Upper.95..confidence.interval * 5/9
seatmp$lowconfC <- seatmp$Lower.95..confidence.interval * 5/9

svg(filename='outputs/mslt.svg', width=7, height=4)
par(mar=c(6,5,2,1))
plot(seatmp$Year, seatmp$tempC, type='l', lwd=2, xlab="Time", ylab=expression("Sea temperature change ("^degree*"C)"), lty=0, bty='l', axes=F,
     xlim=c(min(seatmp$Year), 2020), ylim=c(min(seatmp$tempC), 0.6), xaxs="i", yaxs="i",
     main="Global sea surface temperature since 1880")
mtext("Baseline uses 1971 to 2000 average temperature", 1, 5, cex = 0.9, adj = 0)
abline(v=seq(1880, 2020, by=20), col='grey')
abline(h=seq(-0.6, 0.6, by=0.3), col='grey')
abline(h=0, col="darkslategrey")
polygon(c(seatmp$Year, rev(seatmp$Year)), c(seatmp$upconfC, rev(seatmp$lowconfC)), col=adjustcolor('steelblue2', 0.3), lty=0)
lines(seatmp$Year, seatmp$tempC, lwd=2, col="darkslategrey")
axis(1, at=seq(1880, 2020, by=20), labels = seq(1880, 2020, by=20), lty = 0)
axis(2, at=seq(-0.6, 0.6, by=0.3), labels=seq(-0.6, 0.6, by=0.3), lty=0, las=1)
dev.off()

###### CO2 levels #######################
#read.table("ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt")
#read.table("ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_weekly_mlo.txt")
cotwo <- read.table("sources/co2_mm_mlo.txt", sep="", skip = 72, stringsAsFactors = F, header = F)
head(cotwo)
colnames(cotwo) <- c("year", "month", "dec_year", "avg", "ppm", "trendco", "days")
cotwo[cotwo == -99.99] <- NA

cotwo <- read.table("sources/co2_weekly_mlo.txt", sep="", skip = 49, stringsAsFactors = F, header = F)
head(cotwo)
colnames(cotwo) <- c("year", "month", "day", "dec_year", "ppm", "days", "c1yr", "c10yr", "s1800")
cotwo[cotwo == -999.99] <- NA

png(filename='outputs/co2.png', width=800, height=400)
svg(filename='outputs/co2.svg', width=7, height=4)
par(mar=c(5,5,2,1.5))
plot(cotwo$dec_year, cotwo$ppm, type='l', lwd=2, xlab="Time", ylab=expression("CO"[2]*" ppm"), lty=0, bty='l', axes=F,
     xlim=c(min(cotwo$dec_year), 2020), ylim=c(min(cotwo$ppm, na.rm=T), max(cotwo$ppm, na.rm=T)), xaxs="i", yaxs="i",
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
lines(cotwo$dec_year, cotwo$trendco, lwd=2, col="darkslategrey")
axis(1, at=seq(1960, 2020, by=10), labels = seq(1960, 2020, by=10), lty = 0)
axis(2, at=seq(320, 460, by=20), labels=seq(320, 460, by=20), lty=0, las=1)
dev.off()

###### Global Temp #######################
# https://climate.nasa.gov/vital-signs/global-temperature/

gtemp <- read.table("sources/global_temp.txt", sep="", skip = 5, stringsAsFactors = F, header = F)
head(gtemp)
colnames(gtemp) <- c("year", "raw", "smth")

svg(filename='outputs/global_temp.svg', width=7, height=4)
par(mar=c(6,5,2,1))
plot(gtemp$year, gtemp$raw, type='l', lwd=2, xlab="Time", ylab=expression("Temperature anomaly ("^degree*"C)"), lty=0, bty='l', axes=F,
     xlim=c(min(gtemp$year), 2020), ylim=c(min(gtemp$raw, na.rm=T), max(gtemp$raw, na.rm=T)), xaxs="i", yaxs="i",
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
# https://climate.nasa.gov/vital-signs/arctic-sea-ice/
# remove spaces from file headings

asim <- read.table("sources/arctic_sea_ice_minimum.txt", sep="", skip = 3, stringsAsFactors = F, header = T)
head(asim)
colnames(asim) <- c("year", "month", "day", "ice_extent", "area_year", "area_month", "area_day", "ice_area")

svg(filename='outputs/asim.svg', width=7, height=4)
par(mar=c(5,5,2,1))
plot(asim$year, asim$ice_extent, type='l', lwd=2, xlab="Time", ylab=expression("Area (million km"^2*")"), lty=0, bty='l', axes=F,
     xlim=c(min(asim$year), 2020), ylim=c(min(asim$ice_extent, na.rm=T), max(asim$ice_extent, na.rm=T)), xaxs="i", yaxs="i",
     main="Arctic Sea ice minimum")
yearseq <- seq(1980, 2020, by=10)
valseq <-  seq(0, 7*10^6, by=10^6)
abline(v=yearseq, col='grey')
abline(h=valseq, col='grey')
#lines(asim$year, asim$ice_extent, lwd=6, col="steelblue2")
lines(asim$year, asim$ice_extent, lwd=2, col="darkslategrey")
axis(1, at=yearseq, labels = yearseq, lty = 0)
axis(2, at=valseq, labels=seq(0, 7, 1), lty=0, las=1)
dev.off()
