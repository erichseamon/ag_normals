var_year <- 2015
var_damage <- "Drought"
var_commodity <- "WHEAT"
var_state <- "Washington"
#var_county <- "Whitman"

library(plyr)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(var_state, counties@data$STATE_NAME),]

counties <- subset(counties, STATE_NAME %in% var_state)
monthdir <- paste("/dmine/data/USDA/agmesh-scenarios/", var_state, sep="")

monthdir2 <- paste("/dmine/data/USDA/agmesh-scenarios/", var_state, "/month_1989_2015",sep="")

#uniquez <- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", input$state, "/month", sep=""))
maskraster <- raster(paste("/dmine/data/USDA/agmesh-scenarios/", var_state, "/netcdf/pdsi_apr_", "2001", ".nc", sep=""))
#setwd(monthdir)
#system("find month -type f -size +75c -exec cp -nv {} month_positive/ \\;")

setwd(paste("/dmine/data/USDA/agmesh-scenarios/", var_state, "/month_positive/", sep=""))
#system("mv *AdjustedGrossRevenue.csv ../commodity_csv_agr_month/")
uniquez <<- list.files(paste("/dmine/data/USDA/agmesh-scenarios/", var_state, "/month_positive/", sep=""))

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = var_state)
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

par(mar=c(8,9,7,5), mgp = c(7, 1, 0))
layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow=TRUE))

#barplot of one year, average by month

countieslist <- as.data.frame(counties$NAME)
countieslist$anomaly <- NA

clist <- matrix(NA, nrow = nrow(countieslist), ncol = 2)

jj=1
xrangeyearallcounties <- subset(xrange, commodity == var_commodity & damagecause == var_damage & year == var_year)

for (ii in unique(xrangeyearallcounties$county)) {
  


xrangeyear <- subset(xrange, county == ii & commodity == var_commodity & damagecause == var_damage & year == var_year)
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

xrange3 <- subset(xrange2, county == ii & commodity == var_commodity & damagecause == var_damage)
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

xrange3full <- subset(xrange2full, county == ii & commodity == var_commodity & damagecause == var_damage)
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






xrange7 <- rbind(xrangeyear3, xrange6[2,])
options(scipen = 999)
#barplot(xrange7[2:3,], cex.main = 2, cex.axis = 2, cex.lab = 2, col=c("red", "darkblue"), names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=T, main = "Monthly Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", las = 2, font.lab=2, cex.names = 2)

#legend("topright", 
#       legend = c(paste("Crop Loss for", " 2015", sep=""), "Crop Loss Monthly Average 1989-2015"), 
#       fill = c("red", "darkblue"), cex = 2)


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


#anomaly <- xrange7[2,] - xrange7[3,]
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




#barplot(anomaly, col=ifelse(anomaly>0,"red","darkblue"), cex.names = 2, cex.axis = 2, cex.main = 2, ylim = ylimzz, cex.lab = 2, names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), border="white", font.axis=2, beside=3, main = "Monthly Anomalies: Crop Loss Comparison: Crop Loss Normals (1989-2015) vs. 2015 \n State of Washingtion, WHEAT, Drought Claims", legend=rownames(xrange7), ylab = "Crop loss ($)", xlab="Month", , las = 2, font.lab=3)

#legend("topright", 
#       legend = c(paste("Negative Anomaly: Normals larger than", " 2015", sep=""), paste("Positive Anomaly: ", "2015", " larger than Monthly \nAverage 1989-2015", sep="")), cex = 2, 
 #      fill = c("darkblue", "red"))

clist[jj,] <- c(ii, sum(anomaly))
jj = jj + 1


}
colnames(clist) <- c("NAME", "anomaly")


countiesanomaly <- merge(counties, clist, by = "NAME", all=T)

countiesanomaly$anomaly <- as.numeric(as.character(countiesanomaly$anomaly))



cc0 <- as.data.frame(subset(countiesanomaly, anomaly != "NA" & anomaly != 0))

cc0 <- cc0[order(cc0$anomaly),]
cc1 <- subset(countiesanomaly, anomaly < 0)
cc2 <- subset(countiesanomaly, anomaly > 0)

cc1a <- nrow(cc1)
cc2a <- nrow(cc2)

if (cc2a == 0) {
  
  rc2 = colorRampPalette(colors = c("darkblue", "lightblue"), space="Lab")(cc1a)
  rampcols = as.data.frame(c(rc2))
  
} else if (cc1a == 0) {
  
  rc1 = colorRampPalette(colors = c("lightpink", "darkred"), space="Lab")(cc2a)
  rampcols = as.data.frame(c(rc1))
  
  } else {
    
    rc1 = colorRampPalette(colors = c("darkblue", "lightblue"), space="Lab")(cc1a)
    rc2 = colorRampPalette(colors = c("lightpink", "darkred"), space="Lab")(cc2a)
    rampcols = as.data.frame(c(rc1, rc2))
    
  }



colnames(rampcols) <- c("color")
cc0$color <- rampcols$color
cc0a <- cc0[, -c(2:6)]

cc0b <- merge(countiesanomaly, cc0a, by = "NAME")

pal <- colorNumeric(palette = c("blue", "red"),
                    domain = countiesanomaly$anomaly, na.color="lightgray")
exte <- as.vector(extent(countiesanomaly))


label <- paste(sep = "<br/>", cc0b$NAME, round(cc0b$anomaly, 0))
markers <- data.frame(label)

lev <- levels(cc0b$color)
lev2 <- c(lev, "#FFFFFF")
levels(cc0b$color) <- lev2

naIndex <- which(is.na(cc0b$color))
cc0b[naIndex, "color"] <- "#FFFFFF"

leaflet(data = cc0b) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(fillColor = cc0b$color, fillOpacity = 0.5, color = "black", weight = 1, popup = markers$label)





