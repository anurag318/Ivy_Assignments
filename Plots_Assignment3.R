#EXPORT each plot to a png/pdf programmatically and through UI
jpeg('rplot.jpg')
plot(Y,X)
dev.off()
getwd()
dev.copy(pdf,'myplot.pdf')
dev.off()
library(scatterplot3d)
#Scatterplot3d
scatterplot3d(wind,rain,area, main="3D Scatterplot")
dev.copy(png,'myplot1.png')
dev.off()



# Scatterplot matrix of DMC,DC,wind,rain,temp

pairs(~DMC+DC+wind+rain+temp,data=df, 
      main="Scatterplot matrix of DMC,DC,wind,rain,temp")
dev.copy(pdf,"Matrix_Scatterplot.pdf")
dev.off()
# 3D Scatterplot of wind,rain,area

scatterplot3d(wind,rain,area,main = "3D Scatterplot of wind,rain,area")

# Interactive 3D Scatterplot of wind,rain,area

library(rgl)
plot3d(wind,rain,area,main"Interactive 3D Scatterplot of wind,rain,area")
scatterplot3d(wind,rain,area,color = "red",main = "3D Plot")

# Boxplot of X and Y

boxplot(X~Y,data=df, main="Boxplot", 
        xlab="X", ylab="Y")

# Simple bar plot of temp, wind, rain [horizontal and vertical]

barplot(temp, main="Temperature Distribution", xlab="Temperature")

barplot(temp, main="Temperature Distribution",horiz = TRUE, xlab="Temperature")

count <- table(wind) 
barplot(count,main="Wind Distribution",xlab="Wind")

barplot(wind,main="Wind Distribution",horiz=TRUE,xlab="Wind")



barplot(rain,main="rain Distribution",xlab="Rain")

barplot(rain,main="rain Distribution",horiz=TRUE,xlab="Rain")


# Grouped bar plot of X and Y

counts <- table(X,Y)
barplot(counts, main="Bar Plot of X and Y",
        xlab="X", col=c("blue","green"),
        legend = rownames(counts), beside=TRUE)
dev.copy(pdf,"Barplot.pdf")
dev.off()

# Histogram of probability distribution of X, Y, wind, temp, area along with line density
hist(X, 
     main="Histogram of probability distribution of X", 
     xlab="X", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(X))

hist(Y, 
     main="Histogram of probability distribution of Y", 
     xlab="Y", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(Y))

hist(wind, 
     main="Histogram of probability distribution of wind", 
     xlab="wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(wind))

hist(temp, 
     main="Histogram of probability distribution of temp", 
     xlab="wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(temp))

hist(area, 
     main="Histogram of probability distribution of area", 
     xlab="area", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(area))

# Histogram of frequency distribution of X, Y, wind, temp, area


# Pie Chart of area, wind, rain, temp by month

df$month

df_pivot <- summarise(group_by(df,month),area=sum(area))
slices <- df_pivot[["area"]]
pie(slices, labels=df[["month"]], main="Pie Chart of area")

df_pivot <- summarize(group_by(df,month),wind=sum(wind))
slices <- df_pivot[["wind"]] 
pie(slices, labels=df[["month"]], main="Pie Chart of wind ")

df_pivot <- summarize(group_by(df,month),rain=sum(rain))
slices <- df_pivot[["rain"]] 
pie(slices, labels=df[["month"]], main="Pie Chart of rain")

df_pivot <- summarize(group_by(df,month),temp=sum(temp))
slices <- df_pivot[["temp"]] 
pie(slices, labels=df[["month"]], main="Pie Chart of temp")

# Pie Chart of area, wind, rain, temp by day

df_pivot <- summarize(group_by(df,day),area=sum(area))
slices <- df_pivot[["area"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of area")

df_pivot <- summarize(group_by(df,day),wind=sum(wind))
slices <- df_pivot[["wind"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of wind ")

df_pivot <- summarize(group_by(df,day),rain=sum(rain))
slices <- df_pivot[["rain"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of rain")

df_pivot <- summarize(group_by(df,day),temp=sum(temp))
slices <- df_pivot[["temp"]] 
pie(slices, labels=df[["day"]], main="Pie Chart of temp")


# Map Plot of sourceAirportID
airports <- read.csv("C:/Users/hp/Desktop/Ivy_R/Class 3/airports.dat")
airports
colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")

routes <- read.csv("C:/Users/hp/Desktop/Ivy_R/Class 3/routes.dat")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")

attach(airports)
attach(routes)

map <- get_map(location = 'World', zoom = 5)
airportA <- merge(airports, routes, by.x = "ID", by.y = "sourceAirportID")
mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(sourceAirportID)), data = airportA, alpha = .5)
mapPoints

dev.copy(pdf,"Map.pdf")
dev.off()
# Map Plot of destinationAirportID

#### ASSIGNMENT ####
# EXPORT each plot to a png/pdf programmatically and through UI
