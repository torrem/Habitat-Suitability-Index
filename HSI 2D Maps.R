#HSI 2D Plotting


library(rgdal) 
library(gstat) 
library(colorRamps) 
library(classInt)
library(sp)
library(ggplot2)
library(maptools)
library(reshape)
library(grid)
require(plyr)
library(stats)


word.tif = function(filename="Word_Figure_%03d.tif", zoom=3, width=20, height=20, pointsize=10, ...) {
  if (!grepl("[.]ti[f]+$", filename, ignore.case=TRUE))
    filename = paste0(filename,".tif")
  tiff(filename=filename, compression="lzw", res=600*zoom,
       width=width, height=height, units='cm', pointsize=pointsize, ...)
}

#font to Times
windowsFonts(Times=windowsFont("TT Times New Roman"))

setwd("C:/Users/Mike/Documents/Scallop/Data/Scallop HSI")

## Choose Parameters
#lifeStage = "Juv" # pick Adult or Juv





#----Load FVCOM Data----
#Build FVCOM data in separate file "LIS-FVCOM". 
#Use LIS-FVCOM-Fall for Fall Adult and Juvenile
#Use LIS-FVCOM-SPring for Spring Adult and Juvenile

#   0       -     431
#jan 1978       Dec 2013

FVCOM = read.csv("FVCOM data16.csv", header = T)

FVCOM = FVCOM[!is.na(FVCOM$depth) & !is.na(FVCOM$sediment), ]



#----Split FVCOM between inshore & offshore & zones----
InStat = read.csv("FVCOM Inshore station list.csv", header = T)
zone1 = read.csv("zone 1 stations.csv", header = T)
zone2 = read.csv("zone 2 stations.csv", header = T)
zone3 = read.csv("zone 3 stations.csv", header = T)
for (i in (1:nrow(FVCOM))){
  
  fv=FVCOM$stationID[[i]]
  is=InStat$stationID
  z1=zone1$stationID
  z2=zone2$stationID
  z3=zone3$stationID
  
  
  m = match(fv, is, nomatch = 0)
  
  if (m > 0){
    FVCOM$INOFF[[i]] = "IN"
  }  
  
  if (m==0){
    FVCOM$INOFF[[i]] = "OFF"
    FVCOM$Zone[[i]] = "OFF"
    
  }
  
  m = match(fv, z1, nomatch = 0 )
  if (m > 0){
    FVCOM$Zone[[i]] = 1
  }
  
  m = match(fv, z2, nomatch = 0 )
  if (m > 0){
    FVCOM$Zone[[i]] = 2
  }
  
  m = match(fv, z3, nomatch = 0 )
  if (m > 0){
    FVCOM$Zone[[i]] = 3
  }
}

FVIN = subset(FVCOM, INOFF=="IN")
FVOFF = subset(FVCOM, INOFF=="OFF")







#######################################INSHORE#################################

#----Split FVCOM by year----

#space=rep(0,(nrow(FVCOM)))

#FVCOM = cbind(space,FVCOM)

#yr <- 1978:2013

#pull out temp, salinity, u, and v data from correct year
ts1978=FVIN[,c(2:7, 1737, 8:19, 440:451, 872:883, 1304:1315)]
ts1979=FVIN[,c(2:7, 1737, 20:31, 452:463, 884:895, 1316:1327)]
ts1980=FVIN[,c(2:7, 1737, 32:43, 464:475, 896:907, 1328:1339)]
ts1981=FVIN[,c(2:7, 1737, 44:55, 476:487, 908:919, 1340:1351)]
ts1982=FVIN[,c(2:7, 1737, 56:67, 488:499, 920:931, 1352:1363)]
ts1983=FVIN[,c(2:7, 1737, 68:79, 500:511, 932:943, 1364:1375)]
ts1984=FVIN[,c(2:7, 1737, 80:91, 512:523, 944:955, 1376:1387)]
ts1985=FVIN[,c(2:7, 1737, 92:103, 524:535, 956:967, 1388:1399)]
ts1986=FVIN[,c(2:7, 1737, 104:115, 536:547, 968:979, 1400:1411)]
ts1987=FVIN[,c(2:7, 1737, 116:127, 548:559, 980:991, 1412:1423)]
ts1988=FVIN[,c(2:7, 1737, 128:139, 560:571, 992:1003, 1424:1435)]
ts1989=FVIN[,c(2:7, 1737, 140:151, 572:583, 1004:1015, 1436:1447)]
ts1990=FVIN[,c(2:7, 1737, 152:163, 584:595, 1016:1027, 1448:1459)]
ts1991=FVIN[,c(2:7, 1737, 164:175, 596:607, 1028:1039, 1460:1471)]
ts1992=FVIN[,c(2:7, 1737, 176:187, 608:619, 1040:1051, 1472:1483)]
ts1993=FVIN[,c(2:7, 1737, 188:199, 620:631, 1052:1063, 1484:1495)]
ts1994=FVIN[,c(2:7, 1737, 200:211, 632:643, 1064:1075, 1496:1507)]
ts1995=FVIN[,c(2:7, 1737, 212:223, 644:655, 1076:1087, 1508:1519)]
ts1996=FVIN[,c(2:7, 1737, 224:235, 656:667, 1088:1099, 1520:1531)]
ts1997=FVIN[,c(2:7, 1737, 236:247, 668:679, 1100:1111, 1532:1543)]
ts1998=FVIN[,c(2:7, 1737, 248:259, 680:691, 1112:1123, 1544:1555)]
ts1999=FVIN[,c(2:7, 1737, 260:271, 692:703, 1124:1135, 1556:1567)]
ts2000=FVIN[,c(2:7, 1737, 272:283, 704:715, 1136:1147, 1568:1579)]
ts2001=FVIN[,c(2:7, 1737, 284:295, 716:727, 1148:1159, 1580:1591)]
ts2002=FVIN[,c(2:7, 1737, 296:307, 728:739, 1160:1171, 1592:1603)]
ts2003=FVIN[,c(2:7, 1737, 308:319, 740:751, 1172:1183, 1604:1615)]
ts2004=FVIN[,c(2:7, 1737, 320:331, 752:763, 1184:1195, 1616:1627)]
ts2005=FVIN[,c(2:7, 1737, 332:343, 764:775, 1196:1207, 1628:1639)]
ts2006=FVIN[,c(2:7, 1737, 344:355, 776:787, 1208:1219, 1640:1651)]
ts2007=FVIN[,c(2:7, 1737, 356:367, 788:799, 1220:1231, 1652:1663)]
ts2008=FVIN[,c(2:7, 1737, 368:379, 800:811, 1232:1243, 1664:1675)]
ts2009=FVIN[,c(2:7, 1737, 380:391, 812:823, 1244:1255, 1676:1687)]
ts2010=FVIN[,c(2:7, 1737, 392:403, 824:835, 1256:1267, 1688:1699)]
ts2011=FVIN[,c(2:7, 1737, 404:415, 836:847, 1268:1279, 1700:1711)]
ts2012=FVIN[,c(2:7, 1737, 416:427, 848:859, 1280:1291, 1712:1723)]
ts2013=FVIN[,c(2:7, 1737, 428:439, 860:871, 1292:1303, 1724:1735)]

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


#Calculate max yearly temperature, min salinity, & mean current speed

xIN = list(ts1978=ts1978, ts1979=ts1979, ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
         ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
         ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
         ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
         ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
         ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
         ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,ts2013=ts2013)

#year=1978

for (i in (1:length(xIN))){
  
  y=xIN[[i]]
  
  t=y[,c(8:19)]
  s=y[,c(20:31)]
  u=as.matrix(y[,c(32:43)])
  v=as.matrix(y[,c(44:55)])
  
  cs=as.data.frame(sqrt((u^2) + (v^2)))
  
  mt = apply(t, 1, mean)
  ms = apply(s, 1, mean)
  mcs = apply(cs, 1, mean)
  
  y=cbind((y[,c(56,1:7)]),mt,ms,mcs)
  row.names(y)<-NULL
  
  names(y)[9:11]=c("temperature_b","salinity_b","Current_b")
  
  xIN[[i]]=y
}




#----Calculate INSHORE suitability indexes for each variable----

scallop = read.csv("scallop data16mean.csv", header = T)
d = scallop
d$abundance = d$adult_abundance
d = subset(d, !(cruise %in% c('CRNGM09','CRNGM12')))# Exclude NGOM data


##----flow2d SI
flow = d[,"maxFlowMag2d"]
flow = as.numeric(as.character(flow))
flow2d_int = (classIntervals(flow, 7, style = "fisher"))
flow2d_int[[2]][1] = flow2d_int[[2]][1]-0.1
flow2d_bins = (cut(flow, breaks = flow2d_int$brks))
flow2d_all = aggregate(d$abundance ~ flow2d_bins, data = d, FUN = "mean")
flow2d_adult = aggregate(d$adult_abundance ~ flow2d_bins, data = d, FUN = "mean")
flow2d_count = aggregate(d$abundance ~ flow2d_bins, data = d, FUN = "length")
flow2d_juv = aggregate(d$juv_abundance ~ flow2d_bins, data = d, FUN = "mean")
flow2d = merge(flow2d_adult, flow2d_juv, by = "flow2d_bins", all = TRUE)
flow2d = merge(flow2d_all, flow2d, by = "flow2d_bins", all = TRUE)
flow2d = merge(flow2d_count, flow2d, by = "flow2d_bins", all = TRUE)

colnames(flow2d)[1] = "flow2d_bins"
colnames(flow2d)[2] = "count"
colnames(flow2d)[3] = "total_abundance"
colnames(flow2d)[4] = "abundance_adult"
colnames(flow2d)[5] = "abundance_juv"
flow2d = flow2d[order(flow2d$flow2d_bins),]

d$flow2d_bins = cut(flow, breaks = flow2d_int$brks)

flow2d$SI_flow2d_all = ((flow2d$total_abundance - min(flow2d$total_abundance))/
                          (max(flow2d$total_abundance) - min(flow2d$total_abundance)))
flow2d$SI_flow2d_adult = ((flow2d$abundance_adult - min(flow2d$abundance_adult))/
                            (max(flow2d$abundance_adult) - min(flow2d$abundance_adult)))
flow2d$SI_flow2d_juv = ((flow2d$abundance_juv - min(flow2d$abundance_juv))/
                          (max(flow2d$abundance_juv) - min(flow2d$abundance_juv)))
flow2d = flow2d[, c(1,2,6,7,8)]

flow2d_bins = as.character(flow2d_bins)
a = lapply(strsplit(as.character(flow2d$flow2d_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
flow2d_axis = aa
flow2d_axis[1] = 0

d = merge(d, flow2d, by = "flow2d_bins", all = TRUE)


#FVCOM flow
for (i in (1:length(xIN))){
  
  y=xIN[[i]]
  
  flow_fv = y[,"Current_b"]
  y$flow2d_bins = cut(flow_fv, breaks = flow2d_int$brks)
  y = merge(y, flow2d, by = "flow2d_bins", all = TRUE)
  
  xIN[[i]]=y
}




##Temperature SI
temp = d[,"max_temperature"]
temp = as.numeric(as.character(temp))
temperature_int = (classIntervals(temp, 7, style = "fisher"))
temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
temperature_bins = (cut(temp, breaks = temperature_int$brks))
d$temperature_bins = cut(temp, breaks = temperature_int$brks)
temperature_all = aggregate(d$abundance ~ temperature_bins, data = d, FUN = "mean")
temperature_adult = aggregate(d$adult_abundance ~ temperature_bins, data = d, FUN = "mean")
temperature_juv = aggregate(d$juv_abundance ~ temperature_bins, data = d, FUN = "mean")
temperature_count = aggregate(d$abundance ~ temperature_bins, data = d, FUN = "length")
temperature = merge(temperature_adult, temperature_juv, by = "temperature_bins", all = TRUE)
temperature = merge(temperature_all, temperature, by = "temperature_bins", all = TRUE)
temperature = merge(temperature_count, temperature, by = "temperature_bins", all = TRUE)

d$temperature_bins = cut(temp, breaks = temperature_int$brks)

colnames(temperature)[1] = "temperature_bins"
colnames(temperature)[2] = "count"
colnames(temperature)[3] = "abundance_all"
colnames(temperature)[4] = "abundance_adult"
colnames(temperature)[5] = "abundance_juv"
temperature = temperature[order(temperature$temperature_bins),]

temperature$SI_temperature_all = ((temperature$abundance_all - min(temperature$abundance_all))/
                                  (max(temperature$abundance_all) - min(temperature$abundance_all)))
temperature$SI_temperature_adult = ((temperature$abundance_adult - min(temperature$abundance_adult))/
                                      (max(temperature$abundance_adult) - min(temperature$abundance_adult)))
temperature$SI_temperature_juv = ((temperature$abundance_juv - min(temperature$abundance_juv))/
                                    (max(temperature$abundance_juv) - min(temperature$abundance_juv)))
temperature = temperature[, c(1,2,6,7,8)]

temperature_bins = as.character(temperature_bins)
a = lapply(strsplit(as.character(temperature$temperature_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
temperature_axis = aa

d = merge(d, temperature, by = "temperature_bins", all = TRUE)


#FVCOM temp
for (i in (1:length(xIN))){
  
  y=xIN[[i]]
  
  temp_fv = y[,"temperature_b"]
  y$temperature_bins = cut(temp_fv, breaks = temperature_int$brks)
  y = merge(y, temperature, by = "temperature_bins", all = TRUE)
  
  xIN[[i]]=y
}



## Salinity SI
salt = d[,"min_salinity"]
salt_int = (classIntervals(salt, 10, style = "fisher"))
salt_int[[2]][1] =salt_int[[2]][1]-0.1
salt_bins = cut(salt, breaks = salt_int$brks)
d$salinity_bins = cut(salt, breaks = salt_int$brks)

salinity_all = aggregate(d$abundance ~ salinity_bins, data = d, FUN = "mean")
salinity_adult = aggregate(d$adult_abundance ~ salinity_bins, data = d, FUN = "mean")
salinity_juv = aggregate(d$juv_abundance ~ salinity_bins, data = d, FUN = "mean")
salinity_count = aggregate(d$abundance ~ salinity_bins, data = d, FUN = "length")
salinity = merge(salinity_adult, salinity_juv, by = "salinity_bins", all = TRUE)
salinity = merge(salinity_all, salinity, by = "salinity_bins", all = TRUE)
salinity = merge(salinity_count, salinity, by = "salinity_bins", all = TRUE)

d$salinity_bins = cut(salt, breaks = salt_int$brks)

colnames(salinity)[1] = "salinity_bins"
colnames(salinity)[2] = "count"
colnames(salinity)[3] = "abundance_all"
colnames(salinity)[4] = "abundance_adult"
colnames(salinity)[5] = "abundance_juv"
salinity = salinity[order(salinity$salinity_bins),]

salinity$SI_salinity_all = ((salinity$abundance_all - min(salinity$abundance_all))/
                              (max(salinity$abundance_all) - min(salinity$abundance_all)))
salinity$SI_salinity_adult = ((salinity$abundance_adult - min(salinity$abundance_adult))/
                                (max(salinity$abundance_adult) - min(salinity$abundance_adult)))
salinity$SI_salinity_juv = ((salinity$abundance_juv - min(salinity$abundance_juv))/
                              (max(salinity$abundance_juv) - min(salinity$abundance_juv)))
salinity = salinity[, c(1,2,6,7,8)]

salt_bins = as.character(salt_bins)
a = lapply(strsplit(as.character(salinity$salinity_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
salinity_axis = aa

d = merge(d, salinity, by = "salinity_bins", all = TRUE)


#FVCOM salinity
for (i in (1:length(xIN))){
  
  y=xIN[[i]]
  
  salt_fv = y[,"salinity_b"]
  y$salinity_bins = cut(salt_fv, breaks = salt_int$brks)
  y = merge(y, salinity, by = "salinity_bins", all = TRUE)
  
  xIN[[i]]=y
}


##Depth SI 

dep = d[,"depth"]
dep = as.numeric(as.character(dep))
depth_int = (classIntervals(dep, 8, style = "fisher"))
depth_int[[2]][1] = depth_int[[2]][1]-0.1
depth_bins = (cut(dep, breaks = depth_int$brks))
d$depth_bins = cut(dep, breaks = depth_int$brks)

depth_all = aggregate(d$abundance ~ depth_bins, data = d, FUN = "mean")
depth_adult = aggregate(d$adult_abundance ~ depth_bins, data = d, FUN = "mean")
depth_juv = aggregate(d$juv_abundance ~ depth_bins, data = d, FUN = "mean")
depth_count = aggregate(d$juv_abundance ~ depth_bins, data = d, FUN = "length")
depth = merge(depth_adult, depth_juv, by = "depth_bins", all = TRUE)
depth = merge(depth_all, depth, by = "depth_bins", all = TRUE)
depth = merge(depth_count, depth, by = "depth_bins", all = TRUE)

d$depth_bins = cut(dep, breaks = depth_int$brks)

colnames(depth)[1] = "depth_bins"
colnames(depth)[2] = "count"
colnames(depth)[3] = "abundance_all"
colnames(depth)[4] = "abundance_adult"
colnames(depth)[5] = "abundance_juv"
depth = depth[order(depth$depth_bins),]


depth$SI_depth_all = ((depth$abundance_all - min(depth$abundance_all))/
                        (max(depth$abundance_all) - min(depth$abundance_all)))
depth$SI_depth_adult = ((depth$abundance_adult - min(depth$abundance_adult))/
                          (max(depth$abundance_adult) - min(depth$abundance_adult)))
depth$SI_depth_juv = ((depth$abundance_juv - min(depth$abundance_juv))/
                        (max(depth$abundance_juv) - min(depth$abundance_juv)))

depth = depth[, c(1,2,6,7,8)]

depth_bins = as.character(depth_bins)
a = lapply(strsplit(as.character(depth$depth_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
depth_axis = aa

d = merge(d, depth, by = "depth_bins", all = TRUE)



#FVCOM Depth
for (i in (1:length(xIN))){
  
  y=xIN[[i]]
  
  dep_fv = y[,"temperature_b"]
  y$depth_bins = cut(dep_fv, breaks = depth_int$brks)
  y = merge(y, depth, by = "depth_bins", all = TRUE)
  
  xIN[[i]]=y
}



##Sediment SI
sediment_all = aggregate(d$abundance ~ sediment, data = d, FUN="mean")
sediment_adult = aggregate(d$adult_abundance ~ sediment, data = d, FUN="mean")
sediment_juv = aggregate(d$juv_abundance ~ sediment, data = d, FUN="mean")
sediment_count = aggregate(d$juv_abundance ~ sediment, data = d, FUN="length")
sed = merge(sediment_adult, sediment_juv, by = "sediment", all = TRUE)
sed = merge(sediment_all, sed, by = "sediment", all = TRUE)
sed = merge(sediment_count, sed, by = "sediment", all = TRUE)

colnames(sed)[1] = "sediment"
colnames(sed)[2] = "count"
colnames(sed)[3] = "abundance_all"
colnames(sed)[4] = "abundance_adult"
colnames(sed)[5] = "abundance_juv"

sed$SI_sediment_all = ((sed$abundance_all - min(sed$abundance_all))/
                         (max(sed$abundance_all) - min(sed$abundance_all)))
sed$SI_sediment_adult = ((sed$abundance_adult - min(sed$abundance_adult))/
                           (max(sed$abundance_adult) - min(sed$abundance_adult)))
sed$SI_sediment_juv = ((sed$abundance_juv - min(sed$abundance_juv))/
                         (max(sed$abundance_juv) - min(sed$abundance_juv)))

sed = sed[, c(1,2,6,7,8)]

d = merge(d, sed, by = "sediment", all = TRUE)

#FVCOM depth
for (i in (1:length(xIN))){
  
  y=xIN[[i]]
  
  sed_fv = y[,"sediment"]
  y = merge(y, sed, by = "sediment", all = TRUE)
  
  xIN[[i]]=y
}



#----------- INSHORE Calculate HSI----


#Scallop tows HSI
#d$HSI = round((1/5)*(d$SI_temperature_all + d$SI_depth_all + d$SI_sediment_all + d$SI_salinity_all + d$SI_flow2d_all),2)

  d$HSI = round((    (0.2818*d$SI_temperature_all) + 
                     (0.0839*d$SI_depth_all) + 
                     (0.1486*d$SI_sediment_all) + 
                     (0.3230*d$SI_salinity_all) + 
                     (0.1625*d$SI_flow2d_all)  ),2)




#FVCOM HSI
for (i in (1:length(xIN))){
  
  y=xIN[[i]]
  
   y$HSI = round((    (0.2818*y$SI_temperature_adult) + 
                        (0.0839*y$SI_depth_adult) + 
                        (0.1486*y$SI_sediment_adult) + 
                        (0.3230*y$SI_salinity_adult) + 
                        (0.1625*y$SI_flow2d_all)  ),2)
  
  
  y = y[!is.na(y$HSI),]
  
  xIN[[i]]=y
}

rm(list=setdiff(ls(), c("xIN", "FVOFF", "scallop")))





#######################################OFFSHORE#################################


#----Split FVCOM by year----

#space=rep(0,(nrow(FVCOM)))

#FVCOM = cbind(space,FVCOM)

#yr <- 1978:2013

#pull out temp, salinity, u, and v data from correct year
ts1978=FVOFF[,c(2:7, 1737, 8:19, 440:451, 872:883, 1304:1315)]
ts1979=FVOFF[,c(2:7, 1737, 20:31, 452:463, 884:895, 1316:1327)]
ts1980=FVOFF[,c(2:7, 1737, 32:43, 464:475, 896:907, 1328:1339)]
ts1981=FVOFF[,c(2:7, 1737, 44:55, 476:487, 908:919, 1340:1351)]
ts1982=FVOFF[,c(2:7, 1737, 56:67, 488:499, 920:931, 1352:1363)]
ts1983=FVOFF[,c(2:7, 1737, 68:79, 500:511, 932:943, 1364:1375)]
ts1984=FVOFF[,c(2:7, 1737, 80:91, 512:523, 944:955, 1376:1387)]
ts1985=FVOFF[,c(2:7, 1737, 92:103, 524:535, 956:967, 1388:1399)]
ts1986=FVOFF[,c(2:7, 1737, 104:115, 536:547, 968:979, 1400:1411)]
ts1987=FVOFF[,c(2:7, 1737, 116:127, 548:559, 980:991, 1412:1423)]
ts1988=FVOFF[,c(2:7, 1737, 128:139, 560:571, 992:1003, 1424:1435)]
ts1989=FVOFF[,c(2:7, 1737, 140:151, 572:583, 1004:1015, 1436:1447)]
ts1990=FVOFF[,c(2:7, 1737, 152:163, 584:595, 1016:1027, 1448:1459)]
ts1991=FVOFF[,c(2:7, 1737, 164:175, 596:607, 1028:1039, 1460:1471)]
ts1992=FVOFF[,c(2:7, 1737, 176:187, 608:619, 1040:1051, 1472:1483)]
ts1993=FVOFF[,c(2:7, 1737, 188:199, 620:631, 1052:1063, 1484:1495)]
ts1994=FVOFF[,c(2:7, 1737, 200:211, 632:643, 1064:1075, 1496:1507)]
ts1995=FVOFF[,c(2:7, 1737, 212:223, 644:655, 1076:1087, 1508:1519)]
ts1996=FVOFF[,c(2:7, 1737, 224:235, 656:667, 1088:1099, 1520:1531)]
ts1997=FVOFF[,c(2:7, 1737, 236:247, 668:679, 1100:1111, 1532:1543)]
ts1998=FVOFF[,c(2:7, 1737, 248:259, 680:691, 1112:1123, 1544:1555)]
ts1999=FVOFF[,c(2:7, 1737, 260:271, 692:703, 1124:1135, 1556:1567)]
ts2000=FVOFF[,c(2:7, 1737, 272:283, 704:715, 1136:1147, 1568:1579)]
ts2001=FVOFF[,c(2:7, 1737, 284:295, 716:727, 1148:1159, 1580:1591)]
ts2002=FVOFF[,c(2:7, 1737, 296:307, 728:739, 1160:1171, 1592:1603)]
ts2003=FVOFF[,c(2:7, 1737, 308:319, 740:751, 1172:1183, 1604:1615)]
ts2004=FVOFF[,c(2:7, 1737, 320:331, 752:763, 1184:1195, 1616:1627)]
ts2005=FVOFF[,c(2:7, 1737, 332:343, 764:775, 1196:1207, 1628:1639)]
ts2006=FVOFF[,c(2:7, 1737, 344:355, 776:787, 1208:1219, 1640:1651)]
ts2007=FVOFF[,c(2:7, 1737, 356:367, 788:799, 1220:1231, 1652:1663)]
ts2008=FVOFF[,c(2:7, 1737, 368:379, 800:811, 1232:1243, 1664:1675)]
ts2009=FVOFF[,c(2:7, 1737, 380:391, 812:823, 1244:1255, 1676:1687)]
ts2010=FVOFF[,c(2:7, 1737, 392:403, 824:835, 1256:1267, 1688:1699)]
ts2011=FVOFF[,c(2:7, 1737, 404:415, 836:847, 1268:1279, 1700:1711)]
ts2012=FVOFF[,c(2:7, 1737, 416:427, 848:859, 1280:1291, 1712:1723)]
ts2013=FVOFF[,c(2:7, 1737, 428:439, 860:871, 1292:1303, 1724:1735)]

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


#Calculate max yearly temperature, min salinity, & mean current speed

xOFF = list(ts1978=ts1978, ts1979=ts1979, ts1980=ts1980, ts1981=ts1981, ts1982=ts1982,
           ts1983=ts1983, ts1984=ts1984, ts1985=ts1985, ts1986=ts1986, ts1987=ts1987,
           ts1988=ts1988, ts1989=ts1989, ts1990=ts1990, ts1991=ts1991, ts1992=ts1992,
           ts1993=ts1993, ts1994=ts1994, ts1995=ts1995, ts1996=ts1996, ts1997=ts1997,
           ts1998=ts1998, ts1999=ts1999, ts2000=ts2000, ts2001=ts2001, ts2002=ts2002,
           ts2003=ts2003, ts2004=ts2004, ts2005=ts2005, ts2006=ts2006, ts2007=ts2007,
           ts2008=ts2008, ts2009=ts2009, ts2010=ts2010, ts2011=ts2011, ts2012=ts2012,ts2013=ts2013)

#year=1978

for (i in (1:length(xOFF))){
  
  y=xOFF[[i]]
  
  t=y[,c(8:19)]
  s=y[,c(20:31)]
  u=as.matrix(y[,c(32:43)])
  v=as.matrix(y[,c(44:55)])
  
  cs=as.data.frame(sqrt((u^2) + (v^2)))
  
  mt = apply(t, 1, mean)
  ms = apply(s, 1, mean)
  mcs = apply(cs, 1, mean)
  
  y=cbind((y[,c(56,1:7)]),mt,ms,mcs)
  row.names(y)<-NULL
  
  names(y)[9:11]=c("temperature_b","salinity_b","Current_b")
  
  xOFF[[i]]=y
}




#----Calculate OFFSHORE suitability indexes for each variable----

#scallop = read.csv("scallop data16.csv", header = T)
d = scallop
d$abundance = d$adult_abundance
d = subset(d, cruise %in% c('CRNGM09','CRNGM12'))# Grab only NGOM data


##----flow2d SI
flow = d[,"maxFlowMag2d"]
flow = as.numeric(as.character(flow))
flow2d_int = (classIntervals(flow, 8, style = "fisher"))
flow2d_int[[2]][1] = flow2d_int[[2]][1]-0.1
flow2d_bins = (cut(flow, breaks = flow2d_int$brks))
flow2d_all = aggregate(d$abundance ~ flow2d_bins, data = d, FUN = "mean")
flow2d_adult = aggregate(d$adult_abundance ~ flow2d_bins, data = d, FUN = "mean")
flow2d_count = aggregate(d$abundance ~ flow2d_bins, data = d, FUN = "length")
flow2d_juv = aggregate(d$juv_abundance ~ flow2d_bins, data = d, FUN = "mean")
flow2d = merge(flow2d_adult, flow2d_juv, by = "flow2d_bins", all = TRUE)
flow2d = merge(flow2d_all, flow2d, by = "flow2d_bins", all = TRUE)
flow2d = merge(flow2d_count, flow2d, by = "flow2d_bins", all = TRUE)

colnames(flow2d)[1] = "flow2d_bins"
colnames(flow2d)[2] = "count"
colnames(flow2d)[3] = "total_abundance"
colnames(flow2d)[4] = "abundance_adult"
colnames(flow2d)[5] = "abundance_juv"
flow2d = flow2d[order(flow2d$flow2d_bins),]

d$flow2d_bins = cut(flow, breaks = flow2d_int$brks)

flow2d$SI_flow2d_all = ((flow2d$total_abundance - min(flow2d$total_abundance))/
                          (max(flow2d$total_abundance) - min(flow2d$total_abundance)))
flow2d$SI_flow2d_adult = ((flow2d$abundance_adult - min(flow2d$abundance_adult))/
                            (max(flow2d$abundance_adult) - min(flow2d$abundance_adult)))
flow2d$SI_flow2d_juv = ((flow2d$abundance_juv - min(flow2d$abundance_juv))/
                          (max(flow2d$abundance_juv) - min(flow2d$abundance_juv)))
flow2d = flow2d[, c(1,2,6,7,8)]

flow2d_bins = as.character(flow2d_bins)
a = lapply(strsplit(as.character(flow2d$flow2d_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
flow2d_axis = aa
flow2d_axis[1] = 0

d = merge(d, flow2d, by = "flow2d_bins", all = TRUE)


#FVCOM flow
for (i in (1:length(xOFF))){
  
  y=xOFF[[i]]
  
  flow_fv = y[,"Current_b"]
  y$flow2d_bins = cut(flow_fv, breaks = flow2d_int$brks)
  y = merge(y, flow2d, by = "flow2d_bins", all = TRUE)
  
  xOFF[[i]]=y
}




##Temperature SI
temp = d[,"max_temperature"]
temp = as.numeric(as.character(temp))
temperature_int = (classIntervals(temp, 7, style = "fisher"))
temperature_int[[2]][1] = temperature_int[[2]][1]-0.1
temperature_bins = (cut(temp, breaks = temperature_int$brks))
d$temperature_bins = cut(temp, breaks = temperature_int$brks)
temperature_all = aggregate(d$abundance ~ temperature_bins, data = d, FUN = "mean")
temperature_adult = aggregate(d$adult_abundance ~ temperature_bins, data = d, FUN = "mean")
temperature_juv = aggregate(d$juv_abundance ~ temperature_bins, data = d, FUN = "mean")
temperature_count = aggregate(d$abundance ~ temperature_bins, data = d, FUN = "length")
temperature = merge(temperature_adult, temperature_juv, by = "temperature_bins", all = TRUE)
temperature = merge(temperature_all, temperature, by = "temperature_bins", all = TRUE)
temperature = merge(temperature_count, temperature, by = "temperature_bins", all = TRUE)

d$temperature_bins = cut(temp, breaks = temperature_int$brks)

colnames(temperature)[1] = "temperature_bins"
colnames(temperature)[2] = "count"
colnames(temperature)[3] = "abundance_all"
colnames(temperature)[4] = "abundance_adult"
colnames(temperature)[5] = "abundance_juv"
temperature = temperature[order(temperature$temperature_bins),]

temperature$SI_temperature_all = ((temperature$abundance_all - min(temperature$abundance_all))/
                                    (max(temperature$abundance_all) - min(temperature$abundance_all)))
temperature$SI_temperature_adult = ((temperature$abundance_adult - min(temperature$abundance_adult))/
                                      (max(temperature$abundance_adult) - min(temperature$abundance_adult)))
temperature$SI_temperature_juv = ((temperature$abundance_juv - min(temperature$abundance_juv))/
                                    (max(temperature$abundance_juv) - min(temperature$abundance_juv)))
temperature = temperature[, c(1,2,6,7,8)]

temperature_bins = as.character(temperature_bins)
a = lapply(strsplit(as.character(temperature$temperature_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
temperature_axis = aa

d = merge(d, temperature, by = "temperature_bins", all = TRUE)


#FVCOM temp
for (i in (1:length(xOFF))){
  
  y=xOFF[[i]]
  
  temp_fv = y[,"temperature_b"]
  y$temperature_bins = cut(temp_fv, breaks = temperature_int$brks)
  y = merge(y, temperature, by = "temperature_bins", all = TRUE)
  
  xOFF[[i]]=y
}



## Salinity SI
salt = d[,"min_salinity"]
salt_int = (classIntervals(salt, 10, style = "fisher"))
salt_int[[2]][1] =salt_int[[2]][1]-0.1
salt_bins = cut(salt, breaks = salt_int$brks)
d$salinity_bins = cut(salt, breaks = salt_int$brks)

salinity_all = aggregate(d$abundance ~ salinity_bins, data = d, FUN = "mean")
salinity_adult = aggregate(d$adult_abundance ~ salinity_bins, data = d, FUN = "mean")
salinity_juv = aggregate(d$juv_abundance ~ salinity_bins, data = d, FUN = "mean")
salinity_count = aggregate(d$abundance ~ salinity_bins, data = d, FUN = "length")
salinity = merge(salinity_adult, salinity_juv, by = "salinity_bins", all = TRUE)
salinity = merge(salinity_all, salinity, by = "salinity_bins", all = TRUE)
salinity = merge(salinity_count, salinity, by = "salinity_bins", all = TRUE)

d$salinity_bins = cut(salt, breaks = salt_int$brks)

colnames(salinity)[1] = "salinity_bins"
colnames(salinity)[2] = "count"
colnames(salinity)[3] = "abundance_all"
colnames(salinity)[4] = "abundance_adult"
colnames(salinity)[5] = "abundance_juv"
salinity = salinity[order(salinity$salinity_bins),]

salinity$SI_salinity_all = ((salinity$abundance_all - min(salinity$abundance_all))/
                              (max(salinity$abundance_all) - min(salinity$abundance_all)))
salinity$SI_salinity_adult = ((salinity$abundance_adult - min(salinity$abundance_adult))/
                                (max(salinity$abundance_adult) - min(salinity$abundance_adult)))
salinity$SI_salinity_juv = ((salinity$abundance_juv - min(salinity$abundance_juv))/
                              (max(salinity$abundance_juv) - min(salinity$abundance_juv)))
salinity = salinity[, c(1,2,6,7,8)]

salt_bins = as.character(salt_bins)
a = lapply(strsplit(as.character(salinity$salinity_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
salinity_axis = aa

d = merge(d, salinity, by = "salinity_bins", all = TRUE)


#FVCOM salinity
for (i in (1:length(xOFF))){
  
  y=xOFF[[i]]
  
  salt_fv = y[,"salinity_b"]
  y$salinity_bins = cut(salt_fv, breaks = salt_int$brks)
  y = merge(y, salinity, by = "salinity_bins", all = TRUE)
  
  xOFF[[i]]=y
}


##Depth SI 

dep = d[,"depth"]
dep = as.numeric(as.character(dep))
depth_int = (classIntervals(dep, 8, style = "fisher"))
depth_int[[2]][1] = depth_int[[2]][1]-0.1
depth_bins = (cut(dep, breaks = depth_int$brks))
d$depth_bins = cut(dep, breaks = depth_int$brks)

depth_all = aggregate(d$abundance ~ depth_bins, data = d, FUN = "mean")
depth_adult = aggregate(d$adult_abundance ~ depth_bins, data = d, FUN = "mean")
depth_juv = aggregate(d$juv_abundance ~ depth_bins, data = d, FUN = "mean")
depth_count = aggregate(d$juv_abundance ~ depth_bins, data = d, FUN = "length")
depth = merge(depth_adult, depth_juv, by = "depth_bins", all = TRUE)
depth = merge(depth_all, depth, by = "depth_bins", all = TRUE)
depth = merge(depth_count, depth, by = "depth_bins", all = TRUE)

d$depth_bins = cut(dep, breaks = depth_int$brks)

colnames(depth)[1] = "depth_bins"
colnames(depth)[2] = "count"
colnames(depth)[3] = "abundance_all"
colnames(depth)[4] = "abundance_adult"
colnames(depth)[5] = "abundance_juv"
depth = depth[order(depth$depth_bins),]


depth$SI_depth_all = ((depth$abundance_all - min(depth$abundance_all))/
                        (max(depth$abundance_all) - min(depth$abundance_all)))
depth$SI_depth_adult = ((depth$abundance_adult - min(depth$abundance_adult))/
                          (max(depth$abundance_adult) - min(depth$abundance_adult)))
depth$SI_depth_juv = ((depth$abundance_juv - min(depth$abundance_juv))/
                        (max(depth$abundance_juv) - min(depth$abundance_juv)))

depth = depth[, c(1,2,6,7,8)]

depth_bins = as.character(depth_bins)
a = lapply(strsplit(as.character(depth$depth_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
depth_axis = aa

d = merge(d, depth, by = "depth_bins", all = TRUE)



#FVCOM Depth
for (i in (1:length(xOFF))){
  
  y=xOFF[[i]]
  
  dep_fv = y[,"temperature_b"]
  y$depth_bins = cut(dep_fv, breaks = depth_int$brks)
  y = merge(y, depth, by = "depth_bins", all = TRUE)
  
  xOFF[[i]]=y
}



##Sediment SI
sediment_all = aggregate(d$abundance ~ sediment, data = d, FUN="mean")
sediment_adult = aggregate(d$adult_abundance ~ sediment, data = d, FUN="mean")
sediment_juv = aggregate(d$juv_abundance ~ sediment, data = d, FUN="mean")
sediment_count = aggregate(d$juv_abundance ~ sediment, data = d, FUN="length")
sed = merge(sediment_adult, sediment_juv, by = "sediment", all = TRUE)
sed = merge(sediment_all, sed, by = "sediment", all = TRUE)
sed = merge(sediment_count, sed, by = "sediment", all = TRUE)

colnames(sed)[1] = "sediment"
colnames(sed)[2] = "count"
colnames(sed)[3] = "abundance_all"
colnames(sed)[4] = "abundance_adult"
colnames(sed)[5] = "abundance_juv"

sed$SI_sediment_all = ((sed$abundance_all - min(sed$abundance_all))/
                         (max(sed$abundance_all) - min(sed$abundance_all)))
sed$SI_sediment_adult = ((sed$abundance_adult - min(sed$abundance_adult))/
                           (max(sed$abundance_adult) - min(sed$abundance_adult)))
sed$SI_sediment_juv = ((sed$abundance_juv - min(sed$abundance_juv))/
                         (max(sed$abundance_juv) - min(sed$abundance_juv)))

sed = sed[, c(1,2,6,7,8)]

d = merge(d, sed, by = "sediment", all = TRUE)

#FVCOM depth
for (i in (1:length(xOFF))){
  
  y=xOFF[[i]]
  
  sed_fv = y[,"sediment"]
  y = merge(y, sed, by = "sediment", all = TRUE)
  
  xOFF[[i]]=y
}



#----------- Offshore Calculate HSI----


#Scallop tows HSI
#d$HSI = round((1/5)*(d$SI_temperature_all + d$SI_depth_all + d$SI_sediment_all + d$SI_salinity_all + d$SI_flow2d_all),2)

d$HSI = round((  (0.2947*d$SI_temperature_all) +
                 (0.1583*d$SI_depth_all) +
                 (0.0724*d$SI_sediment_all) +
                 (0.3432*d$SI_salinity_all) +
                 (0.1312*d$SI_flow2d_all)  ),2)



#FVCOM HSI
for (i in (1:length(xOFF))){
  
  y=xOFF[[i]]
  
  #y$HSI= round((1/5)*(y$SI_temperature_adult + y$SI_depth_adult + y$SI_sediment_adult + y$SI_salinity_adult + y$SI_flow2d_all),2)
  
   y$HSI = round((  (0.2947*y$SI_temperature_adult) + 
                     (0.1583*y$SI_depth_adult) + 
                     (0.0724*y$SI_sediment_adult) + 
                     (0.3432*y$SI_salinity_adult) + 
                     (0.1312*y$SI_flow2d_all)  ),2)
  
  y = y[!is.na(y$HSI),]
  
  xOFF[[i]]=y
}

rm(list=setdiff(ls(), c("xIN", "xOFF")))





#---Combine SCALLOP and FVCOM into a single file----

x = list()
for (i in 1:length(xIN)){
  
    INS = xIN[[i]]
    OFFS = xOFF[[i]]
    
    INS = INS[,c(6:10,12,1,13:15,36)]
    OFFS = OFFS[,c(6:10,12,1,13:15,36)]
    
    
    
    x[[i]] = rbind(INS, OFFS)
    
  }


# mat = as.matrix(NA, ncol=1, nrow=length(x))
# 
# for ( i in 1:length(x)){
#   
#   g = x[[i]]
#   f = subset(g, HSI>=0.8)
#   mat[i] = nrow(f)/nrow(g)*100
# }

#rm(list=setdiff(ls(), 'x'))







##########################------------------------------ plotting & MAPPING-----------------------------------------####################



# x=xOFF
# 
# 
# for (i in 1:length(x)){
#   
#   INS = xIN[[i]]
#   OFFS = xOFF[[i]]
#   
#   INS = INS[,c(6:10,12,1,13:15,36)]
#   OFFS = OFFS[,c(6:10,12,1,13:15,36)]
#   
#   
#   
#   x[[i]] = rbind(INS, OFFS)
#   
# }
# 
# 
# for (i in 1:length(x)){
#   aa = x[[i]]$HSI
#   print(length(which(aa>0.8))/length(aa)*100)
# }


#----subset by zone----

# x=xOFF #all offshore data
# 
# 
#  x=xIN #all inshore data
# 
#  for (i in 1:length(x)){
# 
#    z = x[[i]]
#    z = subset(z, z$Zone=="3") #pick a zone
#    x[[i]] = z
# 
#  }



#----Create Median dataframe & plot----


# medians = matrix(NA, ncol = 5, nrow = length(x))
# 
# for (i in 1:length(x)) {
#   y = x[[i]]
#   
#   med_HSI <- median(y$HSI)
#   medtemp <- median(y$temperature_b)
#   medsal <- median(y$salinity_b)
#   medflow <- median(y$Current_b)
#   year <- y$year[1]
#   
#   medians[i,] = c(year, medtemp, medsal, medflow, med_HSI )
# }
# 
# medians <- data.frame(medians)
# names(medians)[1:5]=c("Year","Median_temperature","Median_salinity", "Median_flow","Median_HSI")
# 
# 
# 
# 
# 
# ## HSI linear regression plot##
# 
# #overall area
# 
# word.tif('off_med')
# plot(medians$Median_HSI~medians$Year, ylab = "HSI", xlab = "Year", ylim=(c(0.1,0.8)))
# abline(lm(medians$Median_HSI~medians$Year))
# 
# lm(medians$Median_HSI~medians$Year, data = medians)
# dev.off ()
# 
# 
# #----cross correlation----
# 
# ccf(medians$Median_HSI, medians$Median_temperature, type = "correlation")


#----Set up Plot 2D HSI Maps----
COAST = readShapePoly("C:/Users/Mike/Documents/Scallop/Data/GIS/US_Coastlines/US_Coast.shp")

threeline = readOGR("C:/Users/Mike/Documents/Scallop/Data/GIS/3miline/3milell.shp")

projection(threeline, asText=TRUE)
projection(COAST, asText=TRUE)
projection(COAST) <- projection(threeline, asText=TRUE)
projection(p, asText=TRUE)

FV_HSI = x

# for (i in 1:length(FV_HSI)){
#   t = FV_HSI[[i]]
#   if (lifeStage == "Adult"){t = t[,c(5,7:9,22)]} #22 corresponds to adult HSI
#   if (lifeStage == "Juv"){t = t[,c(5,7:9,23)]} #23 corresponds to juv HSI
#   
#   FV_HSI[[i]] = t
# }

a = list("sp.polygons", COAST)

##----Produce 2D HSI map for all years----

# pb <- txtProgressBar(min = 0, max = length(FV_HSI), style = 3)
# system.time(
# for (i in 1:length(FV_HSI)){
#   d = as.data.frame(FV_HSI[i])
#   d = d[,c(1,2,4,5,11)]
#   colnames(d) <- c("year", "station_id", "longitude", "latitude", "HSI")
# 
#   coordinates(d) = ~longitude + latitude
# 
#   v = variogram(HSI ~ 1, d) 
# 
#   v.g = vgm(nugget = 0.01, psill = 0.03, range = 0.9, model = "Gau") 
# 
#   v.f = fit.variogram(object = v, model = v.g)  
# 
#   #Plot variogram, check the model fitting. 
#   plot(v, model = v.f)
# 
#   g = gstat(formula = HSI~1, model = v.f, data = d, maxdist = 0.1)  
#   
#   xrange = range(d$longitude)
#   yrange = range(d$latitude)
#   
#   grid= expand.grid(lon = seq(from = xrange[1], to = xrange[2], by = 0.01), 
#                     lat = seq(from = yrange[1], to = yrange[2], by = 0.01))
#   
#   gridded(grid) = ~lon + lat
#   gridded(grid) = TRUE
#   
#   p = predict(g, newdata = grid) 
#   
#   grid = expand.grid(nx=480, ny=480)
#   
#   col = matlab.like(10)
#   
#   
#   tmp = spplot(p, sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "white"),
#          col.regions = col,
#          at = (0:10)/10, 
#          zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
#          xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))
#   
#   
#   filePath = "C:/Users/Mike/Documents/Scallop/Data/Scallop HSI/HSI Plots/"
# #   if (lifeStage == "Adult"){folder = "HSI Adult Plots/"} 
# #   if (lifeStage == "Juv"){folder = "HSI Juv Plots/"}
#   
#   
#   png(filename=paste(filePath,"HSI_",FV_HSI[[i]]$year[1],".png",sep=""),
#       width = 1500, height = 1500)
#   print(tmp)
#   dev.off()
#   setTxtProgressBar(pb, i)
# })  
# close(pb)  
  
  
  


##------ Median plot-------  

for (i in 1:length(FV_HSI)){
  
  t = FV_HSI[[i]]
  t=t[,c(4,5,11)]
  FV_HSI[[i]] = t
}

ts = join_all(FV_HSI, by = c("latitude", "longitude"))

ts$HSImed = apply(ts[-c(1,2)], 1, median, na.rm = TRUE)
med.HSI = ts[-c(3:38)] # for all years


coordinates(med.HSI) = ~longitude + latitude
v = variogram(HSImed ~ 1, med.HSI)
v.g = vgm(nugget = 0.003, psill = 0.0197, range = 0.9, model = "Sph") 
v.f = fit.variogram(object = v, model = v.g)  #Fit variogram 
plot(v, model = v.f)

g = gstat(formula = HSImed ~ 1, model = v.f, data = med.HSI, maxdist = 0.1)  

xrange = range(med.HSI$longitude)
yrange = range(med.HSI$latitude)

grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
                  latitude = seq(from = yrange[1], to = yrange[2], by = .004))
gridded(grid) = ~longitude + latitude

p = predict(g, newdata = grid)

l4 = list("sp.text", c(-70, 44.8), cex = 2)
col = matlab.like(10)

word.tif('Med_HSI2')
spplot(p, sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
             col.regions = col,
             at = (0:10)/10, 
            scales=list(draw=TRUE),
             zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
             xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))
dev.off()



spplot(p, sp.layout = list("sp.lines", threeline),
       col.regions = col,
       at = (0:10)/10, 
       scales=list(draw=TRUE),
       zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
       xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))

sp.lines(obj, col = 1, ...)

spplot(p, sp.layout = list("sp.lines",threeline, first=TRUE) ,
       col.regions = col,
       at = (0:10)/10,
       scales=list(draw=TRUE),
       zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
       xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))



# filePath = "C:/Users/Mike/Documents/Scallop/Data/Scallop HSI/HSI Plots/"
# 
# 
# png(filename=paste(filePath, "Median_HSI_","_plot",".png",sep=""),
#     width = 1200, height = 1200)
# print(tmp)
#dev.off()



###------------and Delta HSI plot----------------------

ts = join_all(FV_HSI, by = c("latitude", "longitude"))


# # calculate difference in HSI over 36 years, comment out line 130-144 if you want HSI
betaf = function(vec){
  
  beta = lm(vec ~ seq(1978:2013))$coef[2]
  #p = summary(lm(vec ~ seq(1978:2013)))$ coefficients [2,4]
  return(beta) # beta gives you a slope, if you want p-value, change it to p
#   return(p) # beta gives you a slope, if you want p-value, change it to p
  
}

res = as.data.frame(apply(ts[, 3:38], 1, betaf))
med.HSI = cbind(ts[,1:2], res)
# med.HSI = ts[,3:38] - ts[,39]#baseline is median HSI
# med.HSI = ts[,3:38] - ts[,3]#baseline is 1978
# med.HSI$difference = apply(med.HSI, 1, mean, na.rm = TRUE)
# med.HSI = cbind(ts[,1:2], med.HSI[,37])
colnames(med.HSI)[3] = "HSI"


med.HSI = med.HSI[!is.na(med.HSI$latitude) & !is.na(med.HSI$longitude) &!is.na(med.HSI$HSI),]


coordinates(med.HSI) = ~longitude + latitude
v = variogram(HSI ~ 1, med.HSI)
v.g = vgm(nugget = 0.003, psill = 0.0197, range = 0.9, model = "Sph") 
v.f = fit.variogram(object = v, model = v.g)  #Fit variogram 
plot(v, model = v.f)

g = gstat(formula = HSI ~ 1, model = v.f, data = med.HSI, maxdist = 0.1)  

xrange = range(med.HSI$longitude)
yrange = range(med.HSI$latitude)

grid= expand.grid(longitude = seq(from = xrange[1], to = xrange[2], by = .004), 
                  latitude = seq(from = yrange[1], to = yrange[2], by = .004))
gridded(grid) = ~longitude + latitude

p = predict(g, newdata = grid)

l4 = list("sp.text", c(-70, 44.8),  cex = 2)


col = pal

word.tif('Delta_HSI1')
spplot(p, sp.layout = list("sp.polygons", COAST, first = FALSE, fill = "grey"),
             col.regions = col,
             at = (-15:15)/1000,
       scales=list(draw=TRUE),
             zcol = "var1.pred", #turn this off when you want both variance and prediction map in one output
             xlim = c(-71.2, -66.6), ylim = c(41.2, 45.6))
dev.off()


# png(filename=paste(filePath, "HSI_","Change plot",".png",sep=""),
#     width = 1500, height = 1500)
# print(tmp)
# dev.off()



#----------------------Blue Red Color---------------

#blue-white-red colors
color.palette = function(steps, n.steps.between=NULL, ...){
  
  if(is.null(n.steps.between)) n.steps.between = rep(0, (length(steps)-1))
  if(length(n.steps.between) != length(steps)-1) stop("Must have one less n.steps.between value than steps")
  
  fill.steps = cumsum(rep(1, length(steps))+c(0,n.steps.between))
  RGB = matrix(NA, nrow=3, ncol=fill.steps[length(fill.steps)])
  RGB[,fill.steps] = col2rgb(steps)
  
  for(i in which(n.steps.between>0)){
    col.start=RGB[,fill.steps[i]]
    col.end=RGB[,fill.steps[i+1]]
    for(j in seq(3)){
      vals = seq(col.start[j], col.end[j], length.out=n.steps.between[i]+2)[2:(2+n.steps.between[i]-1)]  
      RGB[j,(fill.steps[i]+1):(fill.steps[i+1]-1)] = vals
    }
  }
  
  new.steps = rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)
  pal = colorRampPalette(new.steps, ...)
  return(pal)
}
steps = c("blue", "white", "red")
pal = color.palette(steps, space="rgb")
col = pal

trellis.par.get() # need this to enlarge font size on color bar

 
#---------------------END----------------------



# ##HSI plot spatial aggregate
# par(mfrow = c(3,1))
# 
# plot(medians$Year,medians$Median_HSI, xlab=NULL, main="Median HSI")
# #labels=year
# lines(medians$Year,medians$Median_HSI) 
# 
# plot(medians$Year,medians$Median_temperature, main="Median Yearly Maximum Bottom Temperature")
# #labels=year
# lines(medians$Year,medians$Median_temperature) 
# 
# plot(medians$Year,medians$Median_salinity, main="Median Yearly Minimum Bottom Salinity")
# #labels=year
# lines(medians$Year,medians$Median_salinity) 
# 
# plot(medians$Year,medians$Median_flow, xlab=NULL, main="Median_flow")
# #labels=year
# lines(medians$Year,medians$Median_flow)
# par(mfrow=c(1,1))


