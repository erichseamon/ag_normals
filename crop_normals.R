library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep("Washington", counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% "Idaho")
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Idaho", "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", "Washington", "/month_positive/", sep=""))

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = "Idaho")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_1989, xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)

par(mar=c(5,7,5,5), mgp = c(5, 1, 0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))

#barplot of one year, average by month

xrangeyear <- subset(xrange, county == "Latah" & commodity == "WHEAT" & damagecause == "Drought" & year == "2001")
xrangeyeara <- aggregate(xrangeyear$loss, list(xrangeyear$monthcode), FUN = "sum")
colnames(xrangeyeara) <- c("monthcode", "loss")

monthlist <- data.frame(c(1:12))
colnames(monthlist) <- c("monthcode")
xrangeyeara <- data.frame(xrangeyeara)
xrangeyear2 <- join(monthlist, xrangeyeara, by = "monthcode")
xrangeyear2 <- data.frame(xrangeyear2)
#colnames(xrangeyear2)[4] <- "NAME"
xrangeyear3 <- t(cbind(xrangeyear2$monthcode, xrangeyear2$loss))

#barplot(xrangeyear2$loss)

#barplot of normals for 1989-2015 for county/commodity/damage cause 


xrange2 <- aggregate(xrange$loss, list(xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "mean")
colnames(xrange2) <- c("county", "commodity", "damagecause", "monthcode", "loss")

xrange3 <- subset(xrange2, county == "Latah" & commodity == "WHEAT" & damagecause == "Drought")
xrange3 <- subset(xrange3, monthcode != 0)
xrange3_month <- c(1:12)
xrange3_month <- data.frame(xrange3_month)
colnames(xrange3_month) <- c("monthcode")
xrange3 <- join(xrange3_month, xrange3, by = "monthcode" )

#lines(xrange3$loss)

xrange4 <- cbind(c(1:12), xrangeyear2$loss, xrange3$loss)
colnames(xrange4) <- c("monthcode", "single_year", "loss")
xrange4 <- data.frame(xrange4)

xrange5 <- cbind(xrange4$monthcode, xrange4$loss)
xrange6 <- t(xrange5)

#--full range for all years, sum of each month

xrange2full <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "sum")
colnames(xrange2full) <- c("year", "county", "commodity", "damagecause", "monthcode", "loss")

xrange3full <- subset(xrange2full, county == "Latah" & commodity == "WHEAT" & damagecause == "Drought")
xrange3full[order(xrange3full$year, xrange3full$monthcode),]
xrange_yearlist <- as.data.frame(rep(1989:2015, each = 12))
xrange_monthlist <- as.data.frame(rep(1:12, 27))
xrange_yearmonthlist <- cbind(xrange_yearlist, xrange_monthlist)
colnames(xrange_yearmonthlist) <- c("year", "monthcode")

xrange3fullfinal <- join(xrange_yearmonthlist, xrange3full, by=c("year","monthcode"))

xrange3fullfinal2 <- cbind(xrange3fullfinal, as.data.frame(rep(xrange6[2,], 27)))
#xrange3fullfinal2 <- subset(xrange3fullfinal2, county != "<NA>")
xrange3fullfinal2[is.na(xrange3fullfinal2)] <- 0

colnames(xrange3fullfinal2) <- c("year", "monthcode", "county", "commodity", "damagecause", "loss", "normal")
xrange3fullfinal2$anomaly <- xrange3fullfinal2$loss - xrange3fullfinal2$normal


#---barplot of normal vs single year

xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
barplot(xrange7[2:3,], col=c("darkblue", "red"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = "Monthly Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=2)

legend("topright", 
       legend = c(paste("Crop Loss for", " 2015", sep=""), "Crop Loss Monthly Average 1989-2015"), 
       fill = c("darkblue", "red"))

xrange7[is.na(xrange7)] <- 0
anomaly <- xrange7[2,] - xrange7[3,]

an1 <- anomaly[c(1:12)]>=0
an2 <- anomaly[c(1:12)]<=0

if ( all(an1, na.rm=TRUE) == TRUE ) {
  
  ylimz <- max(anomaly, na.rm=TRUE)
  ylimzz <- c(-ylimz, ylimz)
} else {
  
  ylimzz <- c(0, max(anomaly, na.rm=TRUE))
  
  
}

if ( all(an2, na.rm=TRUE) == TRUE ) {
  
  ylimz <- min(anomaly, na.rm=TRUE)
  ylimzz <- c(ylimz, abs(ylimz))
} else {
  
  ylimzz <- c(-max(anomaly, na.rm=TRUE), max(anomaly, na.rm=TRUE))
}

anomaly2 <- t(xrange3fullfinal2$anomaly)
an3 <- anomaly2[c(1:324)]>=0
an4 <- anomaly2[c(1:324)]<=0

if ( all(an3, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- max(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(-ylimz1, ylimz1)
} else {
  
  ylimzz1 <- c(0, max(anomaly2, na.rm=TRUE))
  
  
}

if ( all(an4, na.rm=TRUE) == TRUE ) {
  
  ylimz1 <- min(anomaly2, na.rm=TRUE)
  ylimzz1 <- c(ylimz1, abs(ylimz1))
} else {
  
  ylimzz1 <- c(-max(anomaly2, na.rm=TRUE), max(anomaly2, na.rm=TRUE))
}



barplot(anomaly, col=ifelse(anomaly>0,"red","blue"), ylim=ylimzz, names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=2)

legend("topright", 
       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), 
       fill = c("darkblue", "red"))

barplot(xrange3fullfinal2$anomaly, col=ifelse(xrange3fullfinal2$anomaly>0,"red","blue"), ylim=ylimzz1,  border="white", font.axis=2, beside=T, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=2)

#legend("bottomright", 
#       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), 
#       fill = c("darkblue", "red"))

#plot of one year (sum) for entire state for damagecause/commodity

xrange2a <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause), FUN = "sum")
colnames(xrange2a) <- c("year", "county", "commodity", "damagecause", "loss")

xrangemonth <- subset(xrange2a, commodity == "WHEAT" & damagecause == "Drought" & year == "2001")
colnames(xrangemonth)[2] <- "NAME"

m <- merge(counties, xrangemonth, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1
ma <- m

par(mar=c(3,3,3,3))

plot(ma,col = tt(length(ma))[vec])

#plot for normals for month, state, county.commodity.damage cause, 1989 - 2015 mean

xrangenormals <- aggregate(xrange$loss, list(xrange$commodity, xrange$damagecause, xrange$county), FUN = "sum")
colnames(xrangenormals) <- c("commodity", "damagecause", "county", "loss")
xrangenormals_cause <- subset(xrangenormals, damagecause == "Drought" & commodity == "WHEAT")
colnames(xrangenormals_cause)[3] <- "NAME"

m <- merge(counties, xrangenormals_cause, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1

par(mar=c(3,3,3,3))

plot(m,col = tt(length(m))[vec])







