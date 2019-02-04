# Produce SI plots for variables in HSI Model
# HSI Stat Areas 467,511,512,513,514,515

library(classInt)


setwd("C:/Users/Mike/Documents/Scallop/Data/Scallop HSI")



word.tif = function(filename="Word_Figure_%03d.tif", zoom=3, width=20, height=18, pointsize=10, ...) {
  if (!grepl("[.]ti[f]+$", filename, ignore.case=TRUE))
    filename = paste0(filename,".tif")
  tiff(filename=filename, compression="lzw", res=400*zoom,
       width=width, height=height, units='cm', pointsize=pointsize, ...)
}

#font to Times
windowsFonts(Times=windowsFont("TT Times New Roman"))



scallop = read.csv("scallop data16mean.csv", header = T)


dd=scallop

dd$abundance = dd$adult_abundance



#----Calculate suitability indexes for each variable INSHORE----

d = subset(dd, !(cruise %in% c('CRNGM09','CRNGM12')))# Exclude NGOM data
d = subset(d, depth > 2)

##----flow2d SI
flow = d[,"maxFlowMag2d"]
flow = as.numeric(as.character(flow))
flow2d_int = (classIntervals(flow, 6, style = "fisher"))
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

flow2d$SI_flow2d_all = ((flow2d$total_abundance - min(flow2d$total_abundance))/
                          (max(flow2d$total_abundance) - min(flow2d$total_abundance)))
flow2d$SI_flow2d_adult = ((flow2d$abundance_adult - min(flow2d$abundance_adult))/
                            (max(flow2d$abundance_adult) - min(flow2d$abundance_adult)))
flow2d$SI_flow2d_juv = ((flow2d$abundance_juv - min(flow2d$abundance_juv))/
                          (max(flow2d$abundance_juv) - min(flow2d$abundance_juv)))
flow2d_IN = flow2d[, c(1,2,6,7,8)]

flow2d_bins = as.character(flow2d_bins)
a = lapply(strsplit(as.character(flow2d$flow2d_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
flow2d_axis_IN = aa
flow2d_axis_IN[1] = 0

#rm(list=setdiff(ls(), c("d","flow2d", "flow2d_axis")))


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

colnames(temperature)[1] = "temperature_bins"
colnames(temperature)[2] = "count"
colnames(temperature)[3] = "abundance_all"
colnames(temperature)[4] = "abundance_adult"
colnames(temperature)[5] = "abundance_juv"
temperature = temperature[order(temperature$temperature_bins),]

temperature$SI_abundance_all = ((temperature$abundance_all - min(temperature$abundance_all))/
                                      (max(temperature$abundance_all) - min(temperature$abundance_all)))
 temperature$SI_temperature_adult = ((temperature$abundance_adult - min(temperature$abundance_adult))/
                                       (max(temperature$abundance_adult) - min(temperature$abundance_adult)))
temperature$SI_temperature_juv = ((temperature$abundance_juv - min(temperature$abundance_juv))/
                                    (max(temperature$abundance_juv) - min(temperature$abundance_juv)))
temperature_IN = temperature[, c(1,2,6,7,8)]

temperature_bins = as.character(temperature_bins)
a = lapply(strsplit(as.character(temperature$temperature_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
temperature_axis_IN = aa

#rm(list=setdiff(ls(), c("d", "flow2d", "flow2d_axis", "temperature",
#                        "temperature_axis")))



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
salinity_IN = salinity[, c(1,2,6,7,8)]

salt_bins = as.character(salt_bins)
a = lapply(strsplit(as.character(salinity$salinity_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
salinity_axis_IN = aa

# rm(list=setdiff(ls(), c("d","flow2d", "flow2d_axis", "temperature",
#                         "temperature_axis", "salinity", "salinity_axis")))



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

depth_IN = depth[, c(1,2,6,7,8)]

depth_bins = as.character(depth_bins)
a = lapply(strsplit(as.character(depth$depth_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
depth_axis_IN = aa

# rm(list=setdiff(ls(), c("d","flow2d", "flow2d_axis", "temperature",
#                         "temperature_axis", "salinity", "salinity_axis", "depth", "depth_axis")))



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

sed_IN = sed[, c(1,2,6,7,8)]


# rm(list=setdiff(ls(), c("d", "flow2d", "flow2d_axis", "temperature",
#                         "temperature_axis", "salinity", "salinity_axis", "depth", "depth_axis",
#                         "sed")))





#----Calculate suitability indexes for each variable OFFSHORE----

d = subset(dd, cruise %in% c('CRNGM09','CRNGM12'))# Grab only NGOM data
d = subset(d, depth > 5)

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

flow2d$SI_flow2d_all = ((flow2d$total_abundance - min(flow2d$total_abundance))/
                          (max(flow2d$total_abundance) - min(flow2d$total_abundance)))
flow2d$SI_flow2d_adult = ((flow2d$abundance_adult - min(flow2d$abundance_adult))/
                            (max(flow2d$abundance_adult) - min(flow2d$abundance_adult)))
flow2d$SI_flow2d_juv = ((flow2d$abundance_juv - min(flow2d$abundance_juv))/
                          (max(flow2d$abundance_juv) - min(flow2d$abundance_juv)))
flow2d_OFF = flow2d[, c(1,2,6,7,8)]

flow2d_bins = as.character(flow2d_bins)
a = lapply(strsplit(as.character(flow2d$flow2d_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
flow2d_axis_OFF = aa
flow2d_axis_OFF[1] = 0

# rm(list=setdiff(ls(), c("d","flow2d", "flow2d_axis")))


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

colnames(temperature)[1] = "temperature_bins"
colnames(temperature)[2] = "count"
colnames(temperature)[3] = "abundance_all"
colnames(temperature)[4] = "abundance_adult"
colnames(temperature)[5] = "abundance_juv"
temperature = temperature[order(temperature$temperature_bins),]

temperature$SI_abundance_all = ((temperature$abundance_all - min(temperature$abundance_all))/
                                  (max(temperature$abundance_all) - min(temperature$abundance_all)))
temperature$SI_temperature_adult = ((temperature$abundance_adult - min(temperature$abundance_adult))/
                                      (max(temperature$abundance_adult) - min(temperature$abundance_adult)))
temperature$SI_temperature_juv = ((temperature$abundance_juv - min(temperature$abundance_juv))/
                                    (max(temperature$abundance_juv) - min(temperature$abundance_juv)))
temperature_OFF = temperature[, c(1,2,6,7,8)]

temperature_bins = as.character(temperature_bins)
a = lapply(strsplit(as.character(temperature$temperature_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
temperature_axis_OFF = aa

# rm(list=setdiff(ls(), c("d", "flow2d", "flow2d_axis", "temperature",
#                         "temperature_axis")))



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
salinity_OFF = salinity[, c(1,2,6,7,8)]

salt_bins = as.character(salt_bins)
a = lapply(strsplit(as.character(salinity$salinity_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
salinity_axis_OFF = aa

# rm(list=setdiff(ls(), c("d","flow2d", "flow2d_axis", "temperature",
#                         "temperature_axis", "salinity", "salinity_axis")))



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

depth_OFF = depth[, c(1,2,6,7,8)]

depth_bins = as.character(depth_bins)
a = lapply(strsplit(as.character(depth$depth_bins), split = ","), "[", 1)
aa = as.numeric(sub("\\(","", unlist(a)))
depth_axis_OFF = aa

# rm(list=setdiff(ls(), c("d","flow2d", "flow2d_axis", "temperature",
#                         "temperature_axis", "salinity", "salinity_axis", "depth", "depth_axis")))



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

sed_OFF = sed[, c(1,2,6,7,8)]


# rm(list=setdiff(ls(), c("d", "flow2d", "flow2d_axis", "temperature",
#                         "temperature_axis", "salinity", "salinity_axis", "depth", "depth_axis",
#                         "sed")))



















#---------------------------------------plot SI------------------------------------------------
#windowsFonts(Arial=windowsFont("TT Arial"))

library(scales)


word.tif('SI plots meantempSal')
par(mfcol = c(3,2), las = 1, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
par(mar = c(4, 5, 2, 1.5))

##----Temperature
plot(c(temperature_IN$SI_abundance_all,temperature_OFF$SI_abundance_all) ~ 
       c(temperature_axis_IN,temperature_axis_OFF), 
     type = "n", 
     #main = "Adult Scallop SI plots", 
     xlab = NA, ylab = "SI"  
     #xlim = c(2, 20), 
     #tck = 0.02
     )

newx <- seq(1, 20, 0.01)
trim_all_IN <- loess(temperature_IN$SI_abundance_all ~ temperature_axis_IN, span = 0.8)
trim_all_OFF <- loess(temperature_OFF$SI_abundance_all ~ temperature_axis_OFF, span = 0.8)
#trim_adult <- loess(temperature$SI_temperature_adult ~ temperature_axis, span = 0.3)
#trim_juv <- loess(temperature$SI_temperature_juv ~ temperature_axis, span = 0.3)

lines(rescale(predict(trim_all_IN, newdata = newx),c(0,1)) ~ newx, col = 'black', lwd = 2)
lines(rescale(predict(trim_all_OFF, newdata = newx),c(0,1)) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_adult, newdata = newx) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_juv, newdata = newx) ~ newx, col = 'green', lwd = 2)
#legend("bottomright",c("All","Adult", "Juvenile"),
     # lwd=c(2.5,2.5,2.5),col=c("black", "blue", "green"),cex = 0.5) 

abline(h=0.8, lty=2, lwd=2)

mtext(line = 2.5, side = 1, expression(paste("Temperature",degree,"C")))


##----depth
plot(c(depth_IN$SI_depth_all,depth_OFF$SI_depth_all) ~ c(depth_axis_IN, depth_axis_OFF), 
     type = "n", lty = 1, lwd = 4,
     xlab = NA, ylab = "SI" 
     #xlim = c(0, 100), 
     #tck = 0.02
     )

newx <- seq(1, 100, 0.01)
trim_all_IN <- loess(depth_IN$SI_depth_all ~ depth_axis_IN, span = 0.6)
trim_all_OFF <- loess(depth_OFF$SI_depth_all ~ depth_axis_OFF, span = 0.6)
#trim_adult <- loess(depth$SI_depth_adult ~ depth_axis, span = 0.3)
#trim_juv <- loess(depth$SI_depth_juv ~ depth_axis, span = 0.3)

lines(rescale(predict(trim_all_IN, newdata = newx),c(0,1)) ~ newx, col = 'black', lwd = 2)
lines(rescale(predict(trim_all_OFF, newdata = newx),c(0,1)) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_adult, newdata = newx) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_juv, newdata = newx) ~ newx, col = 'green', lwd = 2)
#legend("bottomright",c("All","Adult", "Juvenile"),
      # lwd=c(2.5,2.5,2.5),col=c("black", "blue", "green"),cex = 0.5) 

abline(h=0.8, lty=2, lwd=2)
mtext(line = 2.5, side = 1, "Depth (m)")


##----2dFlow
plot(c(flow2d_IN$SI_flow2d_all, flow2d_OFF$SI_flow2d_all) ~ c(flow2d_axis_IN,flow2d_axis_OFF), 
     type = "n", lty = 1, lwd = 4,
     xlab = NA, ylab = "SI" 
     #xlim = c(-0.1, 0.5) 
     #tck = 0.02
     )

newx <- seq(0, 0.5, 0.001)
trim_all_IN <- loess(flow2d_IN$SI_flow2d_all ~ flow2d_axis_IN, span = 0.6)
trim_all_OFF <- loess(flow2d_OFF$SI_flow2d_all ~ flow2d_axis_OFF, span = 1.3)
#trim_adult <- loess(flow2d$SI_flow2d_adult ~ flow2d_axis, span = 0.3)
#trim_juv <- loess(flow2d$SI_flow2d_juv ~ flow2d_axis, span = 0.3)

lines(rescale(predict(trim_all_IN, newdata = newx),c(0,1)) ~ newx, col = 'black', lwd = 2)
lines(rescale(predict(trim_all_OFF, newdata = newx),c(0,1)) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_adult, newdata = newx) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_juv, newdata = newx) ~ newx, col = 'green', lwd = 2)
#legend("bottomright",c("All","Adult", "Juvenile"),
     #  lwd=c(2.5,2.5,2.5),col=c("black", "blue", "green"),cex = 0.5) 

abline(h=0.8, lty=2, lwd=2)
mtext(line = 2.5, side = 1, "Current Velocity (m/s)")



##----Salinity
plot(c(salinity_IN$SI_salinity_all,salinity_OFF$SI_salinity_all) ~ c(salinity_axis_IN,salinity_axis_OFF), 
     type = "n", lty = 1, lwd = 4, 
     xlab = NA, ylab = "SI"
     #xlim = c(-0.1, 0.5) 
     #tck = 0.02
    )

newx <- seq(1, 40, 0.01)
trim_all_IN <- loess(salinity_IN$SI_salinity_all ~ salinity_axis_IN, span = 0.8)
trim_all_OFF <- loess(salinity_OFF$SI_salinity_all ~ salinity_axis_OFF, span = 0.7)
#trim_adult <- loess(salinity$SI_salinity_adult ~ salinity_axis, span = 0.2)
#trim_juv <- loess(salinity$SI_salinity_juv ~ salinity_axis, span = 0.2)

lines(rescale(predict(trim_all_IN, newdata = newx),c(0,1)) ~ newx, col = 'black', lwd = 2)
lines(rescale(predict(trim_all_OFF, newdata = newx),c(0,1)) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_adult, newdata = newx) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_juv, newdata = newx) ~ newx, col = 'green', lwd = 2)
#legend("bottomright",c("All","Adult", "Juvenile"),
      # lwd=c(2.5,2.5,2.5),col=c("black", "blue", "green"),cex = 0.5) 

abline(h=0.8, lty=2, lwd=2)
mtext(line = 2.5, side = 1, "Salinity (ppt)")


##----Sediment
x=1:7
sediment_IN <- sed_IN[order(sed_IN$sediment),]
sediment_OFF <- sed_OFF[order(sed_OFF$sediment),]
plot(sediment_IN$SI_sediment_all ~ x, 
     type = "n",  lty = 1, lwd = 4,
     xlab = NA, ylab = "SI", 
     xaxt = "n" 
     #tck = 0.02
     )


newx <- seq(1, 7, 0.01)

trim_all_IN <- loess(sediment_IN$SI_sediment_all ~ x, span = 0.3)
trim_all_OFF <- loess(sediment_OFF$SI_sediment_all ~ x, span = 0.3)
#trim_adult <- loess(sediment$SI_sediment_adult ~ x, span = 0.3)
#trim_juv <- loess(sediment$SI_sediment_juv ~ x, span = 0.3)

lines(rescale(predict(trim_all_IN, newdata = newx),c(0,1)) ~ newx, col = 'black', lwd = 2)
lines(rescale(predict(trim_all_OFF, newdata = newx),c(0,1)) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_adult, newdata = newx) ~ newx, col = 'blue', lwd = 2)
#lines(predict(trim_juv, newdata = newx) ~ newx, col = 'green', lwd = 2)
#legend("bottomright",c("All","Adult", "Juvenile"),
      # lwd=c(2.5,2.5,2.5),col=c("black", "blue", "green"),cex = 0.5) 

abline(h=0.8, lty=2, lwd=2)
mtext(line = 2.5, side = 1, "Bottom Composition")
mtext(at=x, line=1, side=1, sediment_IN$sediment)



dev.off()























































