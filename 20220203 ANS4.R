#Algoritmos partitivos
#Vamos a trabajar con K-medias, PAM y Clara

library(factoextra)
library(stats)
library(cluster)

data("USArrests")
df <- scale(USArrests)
head(df, n = 3)
set.seed(123)

#Algoritmo de k-medias
fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
km.res <- kmeans(df, 4, nstart = 25)
print(km.res)
aggregate(USArrests, by = list(cluster = km.res$cluster), mean)
dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)
km.res$cluster
km.res$size
fviz_cluster(km.res, data = df, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

#Algoritmo PAM
fviz_nbclust(df, pam, method = "silhouette")
pam.res <- pam(df, 2)
print(pam.res)
dd2 <- cbind(USArrests, cluster = pam.res$cluster)
head(dd2, n = 3)
pam.res$medoids
pam.res$clustering
fviz_cluster(pam.res, ellipse.type = "t", star, repel = TRUE, ggtheme = theme_classic())

#Algoritmo CLARA
df1 <- rbind(cbind(rnorm(200, 0, 8), rnorm(200, 0, 8)), cbind(rnorm(300, 50, 8), rnorm(300, 50, 8)))
colnames(df1) <- c("x", "y")
rownames(df1) <- paste0("S", 1:nrow(df1))
head(df1, nrow = 6)
fviz_nbclust(df1, clara, method = "silhouette") + theme_classic()
clara.res <- clara(df1, 2, samples = 50, pamLike = TRUE)
print(clara.res)
fviz_cluster(clara.res, ellipse.type = "t", geom = "point", pointsize = 1, ggtheme = theme_classic())
