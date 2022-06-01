###########################################################################   
###### LOADING USEFUL LIBRARIES ######
###########################################################################  

library(tidyverse)

###########################################################################   
###### DATA LOADING ######
# source: https://www.kaggle.com/datasets/mrmorj/dataset-of-songs-in-spotify
###########################################################################  

music <- read.csv(file.choose())
str(music)
summary(music)

###########################################################################   
###### SELECTING VARIABLES ######
###########################################################################  

small <- music %>% dplyr::select(danceability, energy, 
                          loudness, speechiness, acousticness, 
                          instrumentalness, valence, tempo)

###########################################################################   
###### PRINCIPAL COMPONENT ANALYSIS ######
########################################################################### 

# run PCA
pca <- prcomp(small, center = TRUE, scale = TRUE)

# look at results
pca
summary(pca)

# choose number of components if needed
plot(pca, type = "l", main = "Scree plot")

###########################################################################   
###### COMPUTE INDICES ######
########################################################################### 

# predict values of pc
small2 <- predict(pca, newdata = small)
head(small2, 3)

# check correlation
cor(mat2)

# take first two components and merge datasets
with_pc <- cbind(small, small2[, 1:2])

