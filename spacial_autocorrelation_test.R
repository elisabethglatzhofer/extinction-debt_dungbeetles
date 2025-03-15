####model 1, random effect: month####
install.packages("nlme")
library(nlme)
install.packages('phylin')
library(phylin)
install.packages("sp")
library(sp)
install.packages("ape")
library(ape)

data_corr2022.latlong <- read.table("C:/Betty/UNI_WIEN/MASTERARBEIT/Variablen/Variablen_Weiden_2022_lonlat.csv",
                            sep = ";",
                            dec = ",",
                            header = T,
                            text = "",
                            stringsAsFactors = T)

sapply(data_corr2022.latlong, class)
data_corr2022.latlong$LONG <- as.numeric(as.character(data_corr2022.latlong$LONG))
data_corr2022.latlong$LAT <- as.numeric(as.character(data_corr2022.latlong$LAT))

x11()
plot(data_corr2022.latlong$LONG,data_corr2022.latlong$LAT)

###bubble plot:
# convert simple data frame into a spatial data frame object
coordinates(data_corr2022.latlong)= ~ LONG+LAT

x11()
bubble(data_corr2022.latlong, zcol='PAST', fill=TRUE, do.sqrt=FALSE, maxsize=3)
x11()
bubble(data_corr2022.latlong, zcol='WOOD', fill=TRUE, do.sqrt=FALSE, maxsize=3)
x11()
bubble(data_corr2022.latlong, zcol='gr_history', fill=TRUE, do.sqrt=FALSE, maxsize=3)

###Moran's Test (https://rfunctions.blogspot.com/2017/06/how-to-identify-and-remove-spatial.html)
geo<-cbind(data_corr2022.latlong$LONG, data_corr2022.latlong$LAT)
# Then, let us produce a distance matrix (Euclidean) using the longitude and latitude values.
samples.dist <- as.matrix( dist(geo) )
# Now, we have to divide "one by each distance value", creating an inverse distance matrix. Basically, instead of a "distance matrix", we'll end up with a "proximity matrix" (higher values indicate close sites). We'll also make all diagonal values equal zero, due to methodological reasons.
samples.dist.inv <- 1/samples.dist
diag(samples.dist.inv) <- 0
#The Moran's I statistic ranges from -1 to 1. Values in the interval (-1, 0) indicate negative spatial autocorrelation (low values tend to have neighbours with high values and vice versa), values near 0 indicate no spatial autocorrelation (no spatial pattern - random spatial distribution) and values in the interval (0,1) indicate positive spatial autocorrelation (spatial clusters of similarly low or high values between neighbour municipalities should be expected.)
#Moran’s Test uses the following null and alternative hypotheses: Null Hypothesis (H0): The data is randomly dispersed. Alternative Hypothesis (HA): The data is not randomly dispersed, i.e. it is clustered in noticeable patterns. If the p-value that corresponds to Moran’s I is less than a certain significance level (i.e. α = .05), then we can reject the null hypothesis and conclude that the data is spatially clustered together in such a way that it is unlikely to have occurred by chance alone. parameter called alternative =. This parameter has three possible values: "greater" (the default), "less", and "two.sided". The choice will be dictated by the side of the distribution we want to compute the p-value for. If our observed Moran’s I is to the right of the expected distribution, we will want to adopt the "greater" option which will focus on the upper tail of the distribution. If our observed value is to the left of the distribution, we will want to choose the "less" option to focus on the lower tail of the distribution. You can usually tell from the computed Moran’s I value which tail you will want to emphasize by its sign. A general rule of thumb is to place emphasis on the lower tail if Moran’s I value is negative, and to place emphasis on the upper tail if Moran’s I value is positive.
Moran.I( log(data_corr2022.latlong$PAST+1) , samples.dist.inv ,alternative="greater") #0.03
Moran.I( log(data_corr2022.latlong$PAST+1) , samples.dist.inv ,alternative="two.sided") #0.07

Moran.I( log(data_corr2022.latlong$gr_history+1) , samples.dist.inv ,alternative="less") #0.51
Moran.I( log(data_corr2022.latlong$WOOD+1) , samples.dist.inv ,alternative="greater") #0.14

??Moran.I
