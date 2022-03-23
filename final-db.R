library(factoextra)
library(fpc)
library(clustertend)
library(NbClust)
library(readxl)
library(ggplot2)
library(cluster)
library(clValid)

df_no_scale <- read.csv('D:\\Documents\\DataSpellDirectories\\aprendizaje_no_supervisado_r\\output_sample_w_perfil.csv')
# df_no_scale <- subset(df_no_scale, select = c('edad', 'genero', 'ingreso', 'i_juego', 'intentos', 'duracion', 'egreso', 'resultado'))
df_no_scale <- subset(df_no_scale, select = c('edad','ingreso','egreso','intentos','duracion','i_juego'))
df <- scale(df_no_scale)

set.seed(123)
km.res <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res$cluster), ellipse.type = "norm",
             geom = "point", stand = FALSE, pallette = "jco",
             ggtheme = theme_classic())

# fviz_nbclust(df, kmeans, method = "wss") + labs(title = "Elbow")
# fviz_nbclust(df, kmeans, method = "silhouette") + labs(title = "silhouette")
# fviz_nbclust(df, kmeans, method = "gap_stat") + labs(title = "Gap statistic")
# nb <- NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 11, method = "kmeans")
# fviz_nbclust(nb)
#
# SAMPLE_SIZE <- 500
# hopkins(df, n = SAMPLE_SIZE)
# # res <- get_clust_tendency(df, n = nrow(df) - 1)
# res <- get_clust_tendency(df, n = SAMPLE_SIZE)
# res$hopkins_stat
