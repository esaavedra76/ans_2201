library("factoextra")
library("clustertend")

head(iris, 3)
df <- iris[, -5]

random_df <- apply(df, 2, function(x) {
  runif(length(x), min(x), max(x))
})
random_df <- as.data.frame(random_df)

head(df)
head(random_df)

# estandarizar los datos
df <- scale(df)
random_df <- scale(random_df)

fviz_pca_ind(prcomp(df), title = "PCA-iris", habillage = iris$Species,
             geom = "point", ggtheme = theme_classic())
fviz_pca_ind(prcomp(random_df), title = "PCA-iris",
             geom = "point", ggtheme = theme_classic())


set.seed(123)
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster), ellipse.type = "norm",
             geom = "point", stand = FALSE, pallette = "jco",
             ggtheme = theme_classic())

km.res2 <- kmeans(random_df, 3)
fviz_cluster(list(data = random_df, cluster = km.res2$cluster), ellipse.type = "norm",
             geom = "point", stand = FALSE, pallette = "jco",
             ggtheme = theme_classic())

fviz_dend(hclust(dist(random_df)), k = 3, k_colors = "jco",
          as.ggplot = TRUE, show_labels = FALSE)

# -------------------------
hopkins(df, n = nrow(df) - 1)
hopkins(random_df, n = nrow(random_df) - 1)

res1 <- get_clust_tendency(df, n = nrow(df) - 1)
res1$hopkins_stat

res2 <- get_clust_tendency(random_df, n = nrow(random_df) - 1)
res2$hopkins_stat

fviz_dist(dist(df), show_labels = TRUE)

......

library("NbClust")
library("cluster")

fviz_nbclust(df, kmeans, method = "wss") + labs(title = "Elbow")
