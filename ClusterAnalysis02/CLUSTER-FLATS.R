###########################################################  
# INSTALL PACKAGES
###########################################################

install.packages("factoextra")
install.packages("NbClust")
install.packages("fossil")

###########################################################  
# LOAD AND EXPLORE DATA
###########################################################

flats <- read.csv(file.choose())
flats <- na.omit(flats)
str(flats)
summary(flats)

library(tidyverse)

ggplot(data = flats, aes(x = price)) + 
  geom_histogram(fill = "cornflowerblue", color = "black") + 
  theme_bw() +
  labs(x = "Price (in 1000 $)")

ggplot(data = flats, aes(x = totsp)) + 
  geom_histogram(fill = "firebrick", color = "black") + 
  theme_bw() +
  labs(x = "Total square (in meters sq)")

ggplot(data = flats, aes(x = dist)) + 
  geom_histogram(binwidth = 1.5, fill = "limegreen", color = "black") + 
  theme_bw() + 
  labs(x = "Distance to the city center (in km)")

ggplot(data = flats, aes(x = metrdist)) + 
  geom_histogram(binwidth = 2, fill = "hotpink", color = "black") + 
  theme_bw() + 
  labs(x = "Distance to the metro station (in min)")

################################################################  
# HIERARCHICAL CLUSTER ANALYSIS
################################################################

D <- dist(scale(flats))
hc_ward <- hclust(D, method = "ward.D2")
plot(hc_ward)

ward <- cutree(hc_ward, k = 3)
flats$ward <- factor(ward)

################################################################  
# RESULTS: INTERPRETATION
################################################################

flats %>% group_by(ward) %>% summarise_at(vars(price:floor), 
                                             .funs = median)

flats %>% group_by(ward) %>% summarise_at(vars(price:floor), 
                                             .funs = mean)

ggplot(data = flats, aes(x = ward, y = price, fill = ward)) + 
  geom_boxplot() + 
  theme_bw() + 
  labs(x = "Price (in 1000 $)")

ggplot(data = flats, aes(x = totsp, y = price, color = ward)) + 
  geom_point() + 
  theme_bw() + 
  scale_x_log10() + scale_y_log10()

### 3D PLOT ###

install.packages("rgl")
library(rgl)

mycolors <- c('#F8766D', '#00BA38', '#619CFF')
flats$color <- mycolors[as.numeric(flats$ward)]

setupKnitr() # for RMD only

plot3d( 
  x = flats$totsp, y = flats$price, z = flats$floor, 
  col = flats$color, type = 's', radius = 8,
  xlab="Total space", 
  ylab="Price (in 1000$)", 
  zlab="Floor")

rglwidget() # for RMD only

###

################################################################  
# RESULTS: CHOOSING NUMBER OF GROUPS
################################################################

library(factoextra)
fviz_nbclust(flats[1:9], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(flats[1:9], kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

library(NbClust)
res <- NbClust(flats[1:9], min.nc = 2, max.nc = 8, 
               method = "kmeans")
fviz_nbclust(res)

################################################################  
# RESULTS: TESTING DIFFERENCES (CLASSICAL)
################################################################

kruskal.test(flats$price ~ flats$ward)

tab_walk <- table(flats$ward, flats$walk)
tab_walk
tab_brick <- table(flats$ward, flats$brick)
tab_brick
tab_floor <- table(flats$ward, flats$floor)
tab_floor

chisq.test(tab_walk)
chisq.test(tab_brick)
chisq.test(tab_floor)

#############################################################  
# RESULTS: KMEANS CLUSTERING
#############################################################

kclust <- kmeans(flats[1:9], 3)
flats$k <- factor(kclust$cluster)

#############################################################  
# RESULTS: COMPARE CLUSTERINGS
#############################################################

flats %>% group_by(ward) %>% summarise_at(vars(price:floor), 
                                          .funs = c(mean))

flats %>% group_by(k) %>% summarise_at(vars(price:floor), 
                                          .funs = c(mean))

library(fossil)

rand.index(as.integer(flats$ward), 
           as.integer(flats$k))
