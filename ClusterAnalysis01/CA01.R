########################
# CLUSTER ANALYSIS 01 
########################

# prepare data
x <- c(2, 2, 8, 10, 5)
y <- c(6, 8, 2, 3, 5)
dat <- cbind.data.frame(x, y)
dat

# add row names A, B, C, D, E
rownames(dat) <- LETTERS[1:5]
dat

# create distance matrix 
# (euclidean by default)
D <- dist(scale(dat))
D
?dist

# hierachical CA, single linkage method
hc <- hclust(D, method = "single")
hc

# dendrogram
plot(hc, main = "Single linkage method")

# dendrogram and 3 clusters
plot(hc, main = "Single linkage method")
rect.hclust(hc, k = 3, border = "red")

# explore hclust object
hc$height
hc$method
hc$order
