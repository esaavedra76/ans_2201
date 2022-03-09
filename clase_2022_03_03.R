library(factoextra)
library(fpc)
library(clustertend)
library(NbClust)
library(readxl)

df <- read_excel('D:\\Documents\\DataSpellDirectories\\aprendizaje_no_supervisado_r\\Customer_1.xlsx')

head(df)

set.seed(123)
hopkins(df, n = nrow(df) - 1)

res <- get_clust_tendency(df, n = nrow(df) - 1)
res$hopkins_stat

fviz_dist(dist(df), show_labels = FALSE) + labs(title = "Datos de Iris")
