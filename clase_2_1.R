library('factoextra')
# install.packages("factoextra")

data("USArrests")
head(USArrests)
print(dim(USArrests))
summary(USArrests)

hist(USArrests$Murder)
hist(USArrests$Assault)
hist(USArrests$UrbanPop)
hist(USArrests$Rape)

df <- USArrests
df <- scale(df)

head(df, n = 3)

set.seed(123)
ss <- sample(1:50, 15)
df <- USArrests[ss, ]
df.scaled <- scale(df)

dist.eucl <- dist(df.scaled, method = "euclidean")

round(as.matrix(dist.eucl)[1:3, 1:3], 1)

print("----------------")

dist.maximum <- dist(df.scaled, method = "maximum")
round(as.matrix(dist.maximum)[1:3, 1:3], 1)
dist.manhattan <- dist(df.scaled, method = "manhattan")
round(as.matrix(dist.manhattan)[1:3, 1:3], 1)
dist.canberra <- dist(df.scaled, method = "canberra")
round(as.matrix(dist.canberra)[1:3, 1:3], 1)
dist.binary <- dist(df.scaled, method = "binary")
round(as.matrix(dist.binary)[1:3, 1:3], 1)
dist.minkowski <- dist(df.scaled, method = "minkowski")
round(as.matrix(dist.minkowski)[1:3, 1:3], 1)

print("----------------")
dist.pearson <- get_dist(df.scaled, method = "pearson")
round(as.matrix(dist.pearson)[1:3, 1:3], 1)
dist.spearman <- get_dist(df.scaled, method = "spearman")
round(as.matrix(dist.spearman)[1:3, 1:3], 1)
dist.kendall <- get_dist(df.scaled, method = "kendall")
round(as.matrix(dist.kendall)[1:3, 1:3], 1)

dist.cor <- get_dist(df.scaled, method = "pearson")
round (as.matrix(dist.cor)[1:3, 1:3], 1)


library(cluster)
data(flower)
head(flower, 3)

str(flower)

summary(flower)


dd <- daisy(flower)
round(as.matrix(dd)[1:3, 1:3], 2)

library(factoextra)
fviz_dist(dist.eucl)
