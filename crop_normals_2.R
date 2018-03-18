library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep("Washington", counties@data$STATE_NAME),]
counties <- subset(counties, STATE_NAME %in% "Washington")
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", "Washington", sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", "Washington", "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", "Washington", "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", "Washington", "/month_positive/", sep=""))
system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", "Washington", "/month_positive/", sep=""))

#setwd(monthdir2)

temp = list.files(pattern = "\\.WHEAT.csv")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

par(mar=c(5,9,5,5), mgp = c(5, 1, 0))
layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE))

#barplot of one year, average by month

xrangeyear <- subset(xrange, county == "Whitman" & commodity == "WHEAT" & damagecause == "Drought" & year == "2009")
monthlist <- data.frame(c(1:12))
colnames(monthlist) <- c("monthcode")
xrangeyear <- data.frame(xrangeyear)
xrangeyear2 <- join(monthlist, xrangeyear, by = "monthcode")
xrangeyear2 <- data.frame(xrangeyear2)
#colnames(xrangeyear2)[4] <- "NAME"
xrangeyear3 <- t(cbind(xrangeyear2$monthcode, xrangeyear2$loss))

#barplot(xrangeyear2$loss)

#barplot of normals for 1989-2015 for county/commodity/damage cause 


xrange2 <- aggregate(xrange$loss, list(xrange$county, xrange$commodity, xrange$damagecause, xrange$monthcode), FUN = "mean")
colnames(xrange2) <- c("county", "commodity", "damagecause", "monthcode", "loss")

xrange3 <- subset(xrange2, county == "Whitman" & commodity == "WHEAT" & damagecause == "Drought")
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

xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
barplot(xrange7[2:3,], col=c("darkblue", "red"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = "Monthly Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=2)

legend("topright", 
       legend = c(paste("Crop Loss for", " 2015", sep=""), "Crop Loss Monthly Average 1989-2015"), 
       fill = c("darkblue", "red"))

#plot of one year (sum) for entire state for damagecause/commodity

xrange2a <- aggregate(xrange$loss, list(xrange$year, xrange$county, xrange$commodity, xrange$damagecause), FUN = "sum")
colnames(xrange2a) <- c("year", "county", "commodity", "damagecause", "loss")

xrangemonth <- subset(xrange2a, commodity == "WHEAT" & damagecause == "Drought" & year == "2015")
colnames(xrangemonth)[2] <- "NAME"

m <- merge(counties, xrangemonth, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1
ma <- m

par(mar=c(2,4,4,4))
plot(ma,col = tt(length(ma))[vec], main = "Washington Crop Loss Totals - 2015 \n WHEAT, Drought Claims")

#plot for normals for month, state, county.commodity.damage cause, 1989 - 2015 mean

xrangenormals <- aggregate(xrange$loss, list(xrange$commodity, xrange$damagecause, xrange$county), FUN = "sum")
colnames(xrangenormals) <- c("commodity", "damagecause", "county", "loss")
xrangenormals_cause <- subset(xrangenormals, damagecause == "Drought")
colnames(xrangenormals_cause)[3] <- "NAME"

m <- merge(counties, xrangenormals_cause, by='NAME')
m$loss[is.na(m$loss)] <- 0

replace(m$loss, m$loss!=0, rank(m$loss[m$loss!=0]))

tt <- colorRampPalette(c("white",  "light blue", "dark blue"), space = "Lab")
vec <- rank(m$loss)
vec[vec==vec[1]]<-1


par(mar=c(2,4,4,4))
plot(m,col = tt(length(m))[vec], main = "Washington Crop Loss Totals - 1989-2015 \n WHEAT, Drought Claims")


