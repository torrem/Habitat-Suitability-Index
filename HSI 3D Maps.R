 # Scallop 3D Map

#Figure out how to make year variable work!


library(rgl)
library(raster)
library(rgdal) 
library(gstat) 
library(colorRamps) 
library(classInt)
library(sp)
library(ggplot2)
library(maptools)
library(reshape)


setwd("C:/Users/Mike/Documents/Scallop/Data/Scallop HSI")


## Choose Parameters
lifeStage = "Adult" # pick Adult or Juv

year=2005 # 1978 - 2013

#crop = c(-71.2, -66.6, 41.2, 45.6)# # Total Extent
#crop = c()# MDI
crop = c(-69, -68, 43.6, 44.6)# Penobscot Bay Region
#crop = c(-71, -70, 41.7, 42.7)# NH/MA Coast
#crop = c(-67.2, -66.8, 44.8, 45.2)# Cobckook Bay (Zone 3) 1997

#----Load FVCOM Data----
#Build FVCOM data in separate file "LIS-FVCOM". 
#Use LIS-FVCOM-Fall for Fall Adult and Juvenile
#Use LIS-FVCOM-SPring for Spring Adult and Juvenile

#   0       -     431
#jan 1978       Dec 2013

FVCOM = read.csv("FVCOM data.csv", header = T)

space=rep(0,(nrow(FVCOM)))

FVCOM = cbind(space,FVCOM)

#yr <- 1978:2013

#pull out temp and salinity data from correct year
ts1978=FVCOM[,c(2:8,9:20, 441:452)]
ts1979=FVCOM[,c(2:8,21:32, 453:464)]
ts1980=FVCOM[,c(2:8,33:44, 465:476) ]
ts1981=FVCOM[,c(2:8,45:56, 477:488)]
ts1982=FVCOM[,c(2:8,57:68, 489:500)]
ts1983=FVCOM[,c(2:8,69:80, 501:512 )]
ts1984=FVCOM[,c(2:8,81:92, 513:524)]
ts1985=FVCOM[,c(2:8,93:104, 525:536)]
ts1986=FVCOM[,c(2:8,105:116, 537:548)]
ts1987=FVCOM[,c(2:8,117:128, 549:560)]
ts1988=FVCOM[,c(2:8,129:140, 561:572)]
ts1989=FVCOM[,c(2:8,141:152, 573:584)]
ts1990=FVCOM[,c(2:8,153:164, 585:596)]
ts1991=FVCOM[,c(2:8,165:176, 597:608)]
ts1992=FVCOM[,c(2:8,177:188, 609:620)]
ts1993=FVCOM[,c(2:8,189:200, 621:632)]
ts1994=FVCOM[,c(2:8,201:212, 633:644)]
ts1995=FVCOM[,c(2:8,213:224, 645:656)]
ts1996=FVCOM[,c(2:8,225:236, 657:668)]
ts1997=FVCOM[,c(2:8,237:248, 669:680)]
ts1998=FVCOM[,c(2:8,249:260, 681:692)]
ts1999=FVCOM[,c(2:8,261:272, 693:704)]
ts2000=FVCOM[,c(2:8,273:284, 705:716)]
ts2001=FVCOM[,c(2:8,285:296, 717:728)]
ts2002=FVCOM[,c(2:8,297:308, 729:740)]
ts2003=FVCOM[,c(2:8,309:320, 741:752)]
ts2004=FVCOM[,c(2:8,321:332, 753:764)]
ts2005=FVCOM[,c(2:8,333:344, 765:776)]
ts2006=FVCOM[,c(2:8,345:356, 777:788)]
ts2007=FVCOM[,c(2:8,357:368, 789:800)]
ts2008=FVCOM[,c(2:8,369:380, 801:812)]
ts2009=FVCOM[,c(2:8,381:392, 813:824)]
ts2010=FVCOM[,c(2:8,393:404, 825:836)]
ts2011=FVCOM[,c(2:8,405:416, 837:848)]
ts2012=FVCOM[,c(2:8,417:428, 849:860)]
ts2013=FVCOM[,c(2:8,429:440, 861:872)]

#add year column
ts1978$year = 1978
ts1979$year = 1979
ts1980$year = 1980
ts1981$year = 1981
ts1982$year = 1982
ts1983$year = 1983
ts1984$year = 1984
ts1985$year = 1985
ts1986$year = 1986
ts1987$year = 1987
ts1988$year = 1988
ts1989$year = 1989
ts1990$year = 1990
ts1991$year = 1991
ts1992$year = 1992
ts1993$year = 1993
ts1994$year = 1994
ts1995$year = 1995
ts1996$year = 1996
ts1997$year = 1997
ts1998$year = 1998
ts1999$year = 1999
ts2000$year = 2000
ts2001$year = 2001
ts2002$year = 2002
ts2003$year = 2003
ts2004$year = 2004
ts2005$year = 2005
ts2006$year = 2006
ts2007$year = 2007
ts2008$year = 2008
ts2009$year = 2009
ts2010$year = 2010
ts2011$year = 2011
ts2012$year = 2012
ts2013$year = 2013


#Calculate mean fall (oct & nov) temp

x = list(ts1978=ts1978, ts1979=ts1979, ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
         ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
         ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
         ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
         ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
         ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
         ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,ts2013=ts2013)

#year=1978

for (i in (1:length(x))){
  
  y=x[[i]]
  
  t=y[,c(8:19)]
  s=y[,c(20:31)]
  
  mt = apply(t, 1, max)
  ms = apply(s, 1, min)
  
  y=cbind((y[,c(32,1:7)]),mt,ms)
  row.names(y)<-NULL
  
  names(y)[9:10]=c("temperature_b","salinity_b")
  
  x[[i]]=y
}


#----Calculate Suitability Indexes----

## Need to take environmental variables (temp, salinity, depth, substrate)
## and calculate suitability indexes as well as HSI values FOR ALL LIFESTAGES

scallop = read.csv("scallop data.csv", header = T)

f = x
d=scallop

#d = d[!is.na(d$min_salinity),]

names(d)[names(d) == 'abundance'] <- 'total abudance'

##Temperature SI
temp = d[,"max_temperature"]
temp = as.numeric(as.character(temp))
temperature_int = (classIntervals(temp, 10, style = "fisher"))
temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
temperature_bins = (cut(temp, breaks = temperature_int$brks))
d$temperature_bins = cut(temp, breaks = temperature_int$brks)
temperature_adult = aggregate(d$adult_abundance ~ temperature_bins, data = d, FUN = "mean")
temperature_juv = aggregate(d$juv_abundance ~ temperature_bins, data = d, FUN = "mean")
temperature = merge(temperature_adult, temperature_juv, by = "temperature_bins", all = TRUE)

colnames(temperature)[1] = "temperature_bins"
colnames(temperature)[2] = "abundance_adult"
colnames(temperature)[3] = "abundance_juv"
temperature = temperature[order(temperature$temperature_bins),]

temperature$SI_temperature_adult = ((temperature$abundance_adult - min(temperature$abundance_adult))/
                                      (max(temperature$abundance_adult) - min(temperature$abundance_adult)))
temperature$SI_temperature_juv = ((temperature$abundance_juv - min(temperature$abundance_juv))/
                                    (max(temperature$abundance_juv) - min(temperature$abundance_juv)))
temperature = temperature[, c(1,4,5)]

d = merge(d, temperature, by = "temperature_bins", all = TRUE)

#FVCOM temp
for (i in (1:length(x))){
  
  y=x[[i]]
  
  temp_fv = y[,"temperature_b"]
  y$temperature_bins = cut(temp_fv, breaks = temperature_int$brks)
  y = merge(y, temperature, by = "temperature_bins", all = TRUE)
  
  x[[i]]=y
}


## Salinity SI
salt = d[,"min_salinity"]
salt_int = (classIntervals(salt, 10, style = "fisher"))
salt_int[[2]][1] =salt_int[[2]][1]-0.1
salt_bins = cut(salt, breaks = salt_int$brks)
d$salinity_bins = cut(salt, breaks = salt_int$brks)

salinity_adult = aggregate(d$adult_abundance ~ salinity_bins, data = d, FUN = "mean")
salinity_juv = aggregate(d$juv_abundance ~ salinity_bins, data = d, FUN = "mean")
salinity = merge(salinity_adult, salinity_juv, by = "salinity_bins", all = TRUE)

colnames(salinity)[1] = "salinity_bins"
colnames(salinity)[2] = "abundance_adult"
colnames(salinity)[3] = "abundance_juv"
salinity = salinity[order(salinity$salinity_bins),]

salinity$SI_salinity_adult = ((salinity$abundance_adult - min(salinity$abundance_adult))/
                                (max(salinity$abundance_adult) - min(salinity$abundance_adult)))
salinity$SI_salinity_juv = ((salinity$abundance_juv - min(salinity$abundance_juv))/
                              (max(salinity$abundance_juv) - min(salinity$abundance_juv)))
salinity = salinity[, c(1,4,5)]

d = merge(d, salinity, by = "salinity_bins", all = TRUE)

#FVCOM sal
for (i in (1:length(x))){
  
  y=x[[i]]
  
  sal_fv = y[,"salinity_b"]
  y$salinity_bins = cut(sal_fv, breaks = salt_int$brks)
  y = merge(y, salinity, by = "salinity_bins", all = TRUE)
  
  x[[i]]=y
}


##Depth SI 

dep = d[,"depth"]
dep = as.numeric(as.character(dep))
depth_int = (classIntervals(dep, 10, style = "fisher"))
depth_int[[2]][1] = depth_int[[2]][1]-0.1
depth_bins = (cut(dep, breaks = depth_int$brks))
d$depth_bins = cut(dep, breaks = depth_int$brks)

depth_adult = aggregate(d$adult_abundance ~ depth_bins, data = d, FUN = "mean")
depth_juv = aggregate(d$juv_abundance ~ depth_bins, data = d, FUN = "mean")
depth = merge(depth_adult, depth_juv, by = "depth_bins", all = TRUE)

colnames(depth)[1] = "depth_bins"
colnames(depth)[2] = "abundance_adult"
colnames(depth)[3] = "abundance_juv"
depth = depth[order(depth$depth_bins),]

depth = depth[order(depth$depth_bins),]

depth$SI_depth_adult = ((depth$abundance_adult - min(depth$abundance_adult))/
                          (max(depth$abundance_adult) - min(depth$abundance_adult)))
depth$SI_depth_juv = ((depth$abundance_juv - min(depth$abundance_juv))/
                        (max(depth$abundance_juv) - min(depth$abundance_juv)))
depth = depth[, c(1,4,5)]

d = merge(d, depth, by = "depth_bins", all = TRUE)

#FVCOM depth
for (i in (1:length(x))){
  
  y=x[[i]]
  
  dep_fv = y[,"depth"]
  y$depth_bins = cut(dep_fv, breaks = depth_int$brks)
  y = merge(y, depth, by = "depth_bins", all = TRUE)
  
  x[[i]]=y
}


##Sediment SI
sediment_adult = aggregate(d$adult_abundance ~ sediment, data = d, FUN="mean")
sediment_juv = aggregate(d$juv_abundance ~ sediment, data = d, FUN="mean")
sed = merge(sediment_adult, sediment_juv, by = "sediment", all = TRUE)

colnames(sed)[1] = "sediment"
colnames(sed)[2] = "abundance_adult"
colnames(sed)[3] = "abundance_juv"

sed$SI_sediment_adult = ((sed$abundance_adult - min(sed$abundance_adult))/
                           (max(sed$abundance_adult) - min(sed$abundance_adult)))
sed$SI_sediment_juv = ((sed$abundance_juv - min(sed$abundance_juv))/
                         (max(sed$abundance_juv) - min(sed$abundance_juv)))

sed = sed[, c(1,4,5)]

d = merge(d, sed, by = "sediment", all = TRUE)

#FVCOM depth
for (i in (1:length(x))){
  
  y=x[[i]]
  
  sed_fv = y[,"sediment"]
  y = merge(y, sed, by = "sediment", all = TRUE)
  
  x[[i]]=y
}


#----------- Calculate HSI----

d$HSI_adult = round((1/4)*(d$SI_temperature_adult + d$SI_depth_adult + d$SI_sediment_adult + d$SI_salinity_adult),2)
d$HSI_juv = round((1/4)*(d$SI_temperature_juv + d$SI_depth_juv + d$SI_sediment_juv + d$SI_salinity_juv),2)



for (i in (1:length(x))){
  
  y=x[[i]]
  
  y$HSI_adult = round((1/4)*(y$SI_temperature_adult + y$SI_depth_adult + y$SI_sediment_adult + y$SI_salinity_adult),2)
  y$HSI_juv = round((1/4)*(y$SI_temperature_juv + y$SI_depth_juv + y$SI_sediment_juv + y$SI_salinity_juv),2)
  
  y = y[!is.na(y$HSI_adult) & !is.na(y$HSI_juv),]
  
  x[[i]]=y
}

rm(list=setdiff(ls(), c("x", "lifeStage", "year", "crop")))


#----Plot 2D HSI Maps----
COAST = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/GIS/US_Coastlines/US_Coast.shp")


FV_HSI = x

for (i in 1:length(FV_HSI)){
  t = FV_HSI[[i]]
  if (lifeStage == "Adult"){t = t[,c(5,7:9,22)]} #22 corresponds to adult HSI
  if (lifeStage == "Juv"){t = t[,c(5,7:9,23)]} #23 corresponds to juv HSI
  FV_HSI[[i]] = t
}

a = list("sp.polygons", COAST)

##Produce 2D HSI map for selected Year
    
yr = 1978:2013
d = as.data.frame(FV_HSI[which(year == yr)[[1]]])
colnames(d) <- c("year", "station_id", "latitude", "longitude", "HSI")

coordinates(d) = ~longitude + latitude

v = variogram(HSI ~ 1, d) 

v.g = vgm(nugget = 0.01, psill = 0.03, range = 0.9, model = "Gau") 

v.f = fit.variogram(object = v, model = v.g)  

#Plot variogram, check the model fitting. 
plot(v, model = v.f)

g = gstat(formula = HSI~1, model = v.f, data = d, maxdist = 0.1)  

xrange = range(d$longitude)
yrange = range(d$latitude)

grid= expand.grid(lon = seq(from = xrange[1], to = xrange[2], by = 0.01), 
                  lat = seq(from = yrange[1], to = yrange[2], by = 0.01))

gridded(grid) = ~lon + lat
gridded(grid) = TRUE

p = predict(g, newdata = grid) 

grid = expand.grid(nx=480, ny=480)

col = matlab.like(10)


spplot(p, sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "white"),
       col.regions = col,
       at = (0:10)/10, 
       zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
       xlim = head(crop, n=2), ylim = tail(crop, n=2))
    

    



#--------------------plot 3D HSI Map----
GOMras <- raster("C:/Users/Mike/Documents/Scallop/Data/GIS/Depth/ne_atl_crm_v1.asc")
 
dep <- crop(GOMras, crop)


#plot(dep)


dep <- as.matrix(dep)


pcrp <- p
pcdf = data.frame(pcrp@coords,pcrp@data) 
hh = rasterFromXYZ(pcdf[,c(1:2, 3)]) 
hh <- crop(hh, crop)

##since hh is a different resolution than the elevation matrix 
##need to resample hh matrix onto same grid as elevation matrix
resampleFactor <- (dim(hh)[1]/dim(dep)[1])
inputRaster = hh
inCols <- ncol(inputRaster)
inRows <- nrow(inputRaster)
resampledRaster <- raster(ncol=(inCols / resampleFactor), nrow=(inRows / resampleFactor))
extent(resampledRaster) <- extent(inputRaster)
resampledRaster <- resample(inputRaster,resampledRaster,method='bilinear')

hh <-resampledRaster

hh <- as.matrix(hh)
dim(hh)

##----surface plot----------------------_
z<-dep 
mult=z
mult[z >= 0] <- 1 
mult[z < 0] <- 2 #multiplyer to add emphasis to depth
z <- z  * mult

invz <--z #takes the inverse of z to display map correctly
x<-10*(1:nrow(z))
y<-10*(1:ncol(z))
zlim<-range(z)
zlen<-zlim[2]-zlim[1]+1

##Assign color
col=rep("#7F7F7F", length(dep))



HSI_Vec = as.vector(hh)
HSI_Vec[is.na(HSI_Vec)] <- 8000 #chaging to smaller kirging radius created a few gaps under land surfaces.. change to 0
Ele_Vec = as.vector(dep)

#Need to change this block to vectorized commands to handle larger raster size!!
pb <- txtProgressBar(min = 0, max = length(HSI_Vec), style = 3)
system.time(
for (i in 1:length(HSI_Vec)){
  if (0 <= HSI_Vec[i] & HSI_Vec[i] < 0.1) {col[i]=matlab.like(10)[1]}
  if (0.1 < HSI_Vec[i] & HSI_Vec[i] < 0.2) {col[i]=matlab.like(10)[2]}
  if (0.2 < HSI_Vec[i] & HSI_Vec[i] < 0.3) {col[i]=matlab.like(10)[3]}
  if (0.3 < HSI_Vec[i] & HSI_Vec[i] < 0.4) {col[i]=matlab.like(10)[4]}
  if (0.4 < HSI_Vec[i] & HSI_Vec[i] < 0.5) {col[i]=matlab.like(10)[5]}
  if (0.5 < HSI_Vec[i] & HSI_Vec[i] < 0.6) {col[i]=matlab.like(10)[6]}
  if (0.6 < HSI_Vec[i] & HSI_Vec[i] < 0.7) {col[i]=matlab.like(10)[7]}
  if (0.7 < HSI_Vec[i] & HSI_Vec[i] < 0.8) {col[i]=matlab.like(10)[8]}
  if (0.8 < HSI_Vec[i] & HSI_Vec[i] < 0.9) {col[i]=matlab.like(10)[9]}
  if (0.9 < HSI_Vec[i] & HSI_Vec[i] <= 1) {col[i]=matlab.like(10)[10]}
  if(HSI_Vec[i] == 8000) {col[i="#7F7F7F"]}
  # update progress bar
  setTxtProgressBar(pb, i)
 # print(i)
})
close(pb)


for (i in 1:length(HSI_Vec)){
  if (Ele_Vec[i] >=1) {col[i]="#006400"}
  }

open3d()
rgl.surface(x,y,invz,color=col,alpha=1,back="lines")
#terrain3d(x,y,invz,color=col,alpha=0.5,back="lines")
#box3d()







#--------------------------END---------------------------------

# #Terrain Map
# z<-dep 
# mult=z
# mult[z >= 0] <- 1 
# mult[z < 0] <- 3 #multiplyer to add emphasis to depth
# z <- z  * mult
# 
# invz <--z #takes the inverse of z to display map correctly
# x<-10*(1:nrow(z))
# y<-10*(1:ncol(z))
# zlim<-range(z)
# zlen<-zlim[2]-zlim[1]+1
# 
# ##Assign color
# colorlut<-terrain.colors(zlen, alpha=1)
# col<-colorlut[z-zlim[1]+1]
# 
# open3d()
# rgl.surface(x,y,invz,color=col,alpha=1,back="lines")
# box3d()
# 
# ##Water matrix
# water = dep
# water[water < 800] <- -2
# 
# #add water
# zwat<-water
# xwat<-10*(1:nrow(zwat))
# ywat<-10*(1:ncol(zwat))
# rgl.surface(xwat,ywat,zwat,color="blue",alpha=0.4,back="lines")
# 
# #add the contour map in different color
# colorlut <- heat.colors(zlen,alpha=1)
# col<-colorlut[z-zlim[1]+1]
# rgl.surface(x,y,matrix(1,nrow(z),ncol(z)),color=col,back="fill")














#----------------------------------------
# data(meuse.grid)
# rasterFromXYZ(meuse.grid[,c(1:2, 5)]) 
# 
# m = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], data = meuse.grid) 
# m@data <- data.frame(name="A",m@data) 
# 
# b <- brick(m)
# b[["ffreq"]] 












