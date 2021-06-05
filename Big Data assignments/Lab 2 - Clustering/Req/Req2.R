rm(list=ls())

setwd("F:\\CMP 2020\\Big Data\\Labs\\Lab 2 - Clustering\\Lab 2\\Req")

library(png)
#(2)
img = readPNG("bird_small.png")

#RGB
R = img[,,1]
G = img[,,2]
B = img[,,3]

#(3) Convert matrix to vector
df = data.frame(c(R), c(G), c(B))
colnames(df) = c('R', 'G', 'B')

set.seed(1234)
k=16
iter=15
#(4)
km = kmeans(df, centers=k, iter.max=iter)
km
#(5)
km$cluster
##(6)These centers are the new selected colors
km$centers

#(7)Assign each pixel to the centroid of its cluster.
for (i in 1:(dim(img)[1]*dim(img)[2])) df[i,] = km$centers[km$cluster[i],]
#(8)
compressed = array(c(df$R, df$G, df$B), c(dim(img)[1],dim(img)[2],3))

#(9)Compressed size is 1/3 of the original size ! about 11KB
writePNG(compressed, 'compressed.png')

