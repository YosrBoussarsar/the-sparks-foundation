library(cluster)
x <- read.csv("C:/Users/lenovo/Documents/Iris.csv")
irisdata=x[,2:5]
str(irisdata)
summary(irisdata)

wss <- sapply(1:10, function(k) {
  kmeans(irisdata, centers = k)$tot.withinss})
elbow_point <- 3  # You can manually set the elbow point based on the plot
points(elbow_point, wss[elbow_point], col = "red", cex = 2, pch = 19)
       
kmeans_model <- kmeans(irisdata,3)
kmeans_model
clusplot(irisdata, kmeans_model$cluster, color = TRUE, shade = TRUE, lines = 0)
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters",
     ylab = "Within cluster sum of squares")

