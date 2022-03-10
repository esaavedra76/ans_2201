library(factoextra)
library(fpc)
library(clustertend)
library(NbClust)
library(readxl)
library(ggplot2)
library(cluster)
library(clValid)

df_no_scale <- read_excel('D:\\Documents\\DataSpellDirectories\\aprendizaje_no_supervisado_r\\Customer_1.xlsx')
df <- scale(df_no_scale)

head(df)

set.seed(123)

# point 1
# hopkins
hopkins(df, n = nrow(df) - 1)
res <- get_clust_tendency(df, n = nrow(df) - 1)
res$hopkins_stat
# visual
fviz_dist(dist(df), show_labels = FALSE) + labs(title = "Datos del Customer")


# point 2
# número óptimo de clusters
fviz_nbclust(df, kmeans, method = "wss") + labs(title = "Elbow")
fviz_nbclust(df, kmeans, method = "silhouette") + labs(title = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat") + labs(title = "Gap statistic")
nb <- NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 11, method = "kmeans")
fviz_nbclust(nb)

km.res <- kmeans(df, 2)
sil <- silhouette(km.res$cluster, dist(df))
fviz_silhouette(sil, palette = "jco", ggtheme = theme_classic())

# point 3
# medidas de validación interna
clmethods <- c("hierarchical", "kmeans", "pam")
intern <- clValid(df, nClust = 2:6, clMethods = clmethods, validation = "internal")
summary(intern)


# point 4
# medidas de estabilidad
clmethods <- c("hierarchical", "kmeans", "pam")
stab <- clValid(df, nClust = 2:6, clMethods = clmethods, validation = "stability")
optimalScores(stab)

#
km.res1 <- kmeans(df, 2)
fviz_cluster(list(data = df, cluster = km.res1$cluster), ellipse.type = "norm",
             geom = "point", stand = FALSE, pallette = "jco",
             ggtheme = theme_classic())

#Clustering jerárquico
hc.res <- eclust(df, "hclust", k = 2, hc_metric = "euclidean",
                 hc_method = "ward.D2", graph = FALSE)
#Visualizar dendrograma
fviz_dend(hc.res, show_labels = FALSE, pallette = "jco", as.ggplot = TRUE)

# pam.res <- pam(df, 6)
fviz_cluster(hc.res, ellipse.type = "norm",
             geom = "point", stand = FALSE, pallette = "jco",
             ggtheme = theme_classic())

df1 <- subset(df_no_scale, hc.res$cluster == 1)
df2 <- subset(df_no_scale, hc.res$cluster == 2)
