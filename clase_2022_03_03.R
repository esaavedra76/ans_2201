library(factoextra)
library(fpc)
library(clustertend)
library(NbClust)
library(readxl)
library(ggplot2)
library(cluster)
library(clValid)

df <- read_excel('D:\\Documents\\DataSpellDirectories\\aprendizaje_no_supervisado_r\\Customer_1.xlsx')

head(df)

set.seed(123)

# point 1
# hopkins
hopkins(df, n = nrow(df) - 1)
res <- get_clust_tendency(df, n = nrow(df) - 1)
res$hopkins_stat
# visual
fviz_dist(dist(df), show_labels = FALSE) + labs(title = "Datos de Iris")


# point 2
# número óptimo de clusters
fviz_nbclust(df, kmeans, method = "wss") + labs(title = "Elbow")
fviz_nbclust(df, kmeans, method = "silhouette") + labs(title = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat") + labs(title = "Gap statistic")
nb <- NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 11, method = "kmeans")
fviz_nbclust(nb)


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
