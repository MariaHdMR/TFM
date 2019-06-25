#matriz espacial
#estos datos solo son del plot 1
sitios <- data.frame(name = c("1A","2A","3A","4A","5A","6A","1B","2B", "3B", "4B", "5B","6B","1C","2C","3C","4C","5C","6C","1D","2D","3D","4D","5D","6D","1E","2E","3E","4E", "5E","6E","1F","2F","3F","4F","5F","6F"),lat= c(0.5,0.5,0.5,0.5,0.5,0.5,2,2,2,2,2,2,3.5,3.5,3.5,3.5,3.5,3.5,5,5,5,5,5,5,6.5,6.5,6.5,6.5,6.5,6.5,8,8,8,8,8,8), lng= c(0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8))
h <- dist(sitios, method= "euclidean", diag=T, upper=T)
summary(h)

plot(sitios$lat~sitios$lng)
plot(h)# ni idea de lo que esta pasando aqui

