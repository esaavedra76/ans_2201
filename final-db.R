library(factoextra)
library(fpc)
library(clustertend)
library(NbClust)
library(readxl)
library(ggplot2)
library(cluster)
library(clValid)

KEY_CLUSTER <- 'cluster'
KEY_EDAD <- 'edad'
KEY_I_EDAD <- 'i_edad'
KEY_GENERO <- 'genero'
KEY_PERFIL <- 'perfil'
KEY_INGRESO <- 'ingreso'
KEY_EGRESO <- 'egreso'
KEY_INTENTOS <- 'intentos'
KEY_DURACION <- 'duracion'
KEY_I_JUEGO <- 'i_juego'

df_no_scale <- read.csv('D:\\Documents\\DataSpellDirectories\\aprendizaje_no_supervisado_r\\output_scenario_19_w_perfil-fixed.csv')
# df_no_scale <- subset(df_no_scale, select = c('edad', 'genero', 'ingreso', 'i_juego', 'intentos', 'duracion', 'egreso', 'resultado'))
df_no_scale <- subset(df_no_scale, select = c(KEY_EDAD, KEY_INGRESO, KEY_EGRESO, KEY_INTENTOS, KEY_DURACION, KEY_I_JUEGO))
df <- scale(df_no_scale)

set.seed(123)
km.res <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res$cluster), ellipse.type = "norm",
             geom = "point", stand = FALSE, pallette = "jco",
             ggtheme = theme_classic())

fviz_nbclust(df, kmeans, method = "wss") + labs(title = "Elbow")
fviz_nbclust(df, kmeans, method = "silhouette") + labs(title = "silhouette")

# SAMPLE_SIZE <- 500
# hopkins(df, n = SAMPLE_SIZE)
hopkins(df, n = nrow(df) - 1)
res <- get_clust_tendency(df, n = nrow(df) - 1)
# res <- get_clust_tendency(df, n = SAMPLE_SIZE)
res$hopkins_stat

# fviz_nbclust(df, kmeans, method = "gap_stat") + labs(title = "Gap statistic")
# nb <- NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 11, method = "kmeans")
# fviz_nbclust(nb)
