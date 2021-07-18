library(mvoutlier)
library(Rfunspaceadventure)
str(humus)

x <- log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")])
distances <- dd.plot(x, quan=1/2, alpha=0.025)

# The classical and Robust distances now exist as vectors

str(distances$md.cla)

str(distances$md.rob)


x <- log(humus[,c("U","Pb")])
colorData <- color.plot(x, quan=1/2, alpha=0.025)


str(colorData$md)

str(colorData$euclidean)

str(colorData$outliers)

# Generate the data frame with sleected features
x <- log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")])

# Execute
outliers <- aq.plot(x, delta=qchisq(0.975, df = ncol(x)),
                    quan = 1/2, alpha = 0.05)

# Inspect outliers
str(outliers)


x <- log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")])

par(mfrow = c(1,1))

chisq.plot(x, quan = 0.5, ask = TRUE)

res <- chisq.plot(log(humus[ , c("Al","Co","Cu","Ni", "Pb","U")]), quan = 0.5, ask = TRUE)
res$outliers


# kola background is a map from which humus samples were taken
kola <- rbind(kola.background$coast,kola.background$boundary,
              kola.background$bordes) 

# XY coordinates within humus dataset
XY <- humus[,c("XCOO","YCOO")]

# The mvoutlier.CoDa function returns all pertinent information needed for further analysis
data <- mvoutlier.CoDa(humus[ , c("Al","Cu", "Ni", "Co")])

# execute:

plot(data,coord=XY,map=kola,onlyout=TRUE,which="map",bw=FALSE,
     symb=TRUE,symbtxt=TRUE)


plot(data,onlyout=TRUE,which="biplot",bw=FALSE,
     symb=TRUE,symbtxt=TRUE) 
