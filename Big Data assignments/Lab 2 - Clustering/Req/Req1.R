rm(list=ls())

setwd("F:\\CMP 2020\\Big Data\\Labs\\Lab 2 - Clustering\\Lab 2\\Req")

# x, y data points
# dim = (300, 2)
#(2)
df = read.csv("clustering_data.csv")
x = df[,1]
y = df[,2]
plot(x, y, main='Data points', type='p', xlab='x', ylab='y')
## Comment
# from the Plot we can see that the optimal number of clusters should be around k=3

#x = c(1,2,3)
#y = c(1,1,1)
#x = c(2.5)
#y = c(1)
#points(x,y)

set.seed(1234)
# Apply kmeans with 10 centers (k) with max_iter=15
k=10
iter=15
#(3)
km = kmeans(df, centers=k, iter.max=iter)
km
#(4)
km$cluster
# Centroids
km$centers
str(km)
#(5)
plot(df, col=km$cluster, main='Clusters', type='p', xlab='x', ylab='y')
#(6)
# pch=17 for the solid Triangle
points(km$center, col=1:k, pch=17, cex=2)

##(7) Diff between Plot() and Points():
# Plot: Does the actual plotting (x-axis, y-axis) and plot data points.
# Points: Adds points to an existing plot so it must be used after plot (can't be used alone)
# and the added points must be in the range of the plot x-axis and y-axis to be seen
# otherwise the newly added points won't be seen (out of existing plot range).

###(8)Method: 1
dist = 0
for (i in 1:10) dist[i] = kmeans(df, centers=i, iter.max=iter)$tot.withinss
dist
plot(1:10, dist, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

## Comment
# Dist[i] = sum(distances of data point to it's cluster's centroid)
# Dist is decreasing which is reasonable -> will reach 0 when k = data points
# Notice the amount of decrease is pretty small after i=3 so as i said above
# the optimal number of cluster is around 3

###(8)Method: 2
library(NbClust)
nc = NbClust(df, min.nc=2, max.nc=10, method="kmeans")

#(9)see the voting results 
table(nc$Best.n[1,])

## Comment
# According to the second method the best number of clusters is 3 which is kinda expected
# as i said above :D

### Repeat 4,5,6
k=3
# max_iterations should be less i think since we use less number of clusters.
# tried multiple iterations and compared the centroids and 3 is good enough.
iter=3
#(10)
km = kmeans(df, centers=k, iter.max=iter)
km
# Centroids
km$cluster
km$centers
str(km)
plot(df, col=km$cluster, main='Clusters', type='p', xlab='x', ylab='y')
#(11) pch=17 for the solid Triangle
points(km$center, col=1:k, pch=17, cex=2)

