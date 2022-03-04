#Estadísticas de Validación

#visualizando los datos
library(factoextra)
#validación
library(fpc)
#Número óptimo de clústeres
library(NbClust)

#Preparación de los datos de Iris
#Excluimos la columna de especie

df<-iris[,-5]
df<-scale(df)
?eclust

#k-medias
km.res<-eclust(df,"kmeans",k=3,nstart=25,graph=FALSE)

#Visualización de los clusters de k-medias
fviz_cluster(km.res,geom="point",ellipse.type="norm",
             palette="jco",ggtheme=theme_minimal())

#Clustering jerárquico
hc.res<-eclust(df,"hclust",k=3,hc_metric="euclidean",
               hc_method="ward.D2",graph=FALSE)
#Visualizar dendrograma
fviz_dend(hc.res,show_labels=FALSE, pallette="jco",as.ggplot=TRUE)

#Resumen de silhouette
fviz_silhouette(km.res,palette="jco",ggtheme=theme_classic())

km.res
#Extraer la información de silhouette:
#Informaicón de silhouette
silinfo<-km.res$silinfo
names(silinfo)
#Ancho de silueta para cada observación
silinfo$widths
#Promedio total (de todos los anchos de silueta individuales)
silinfo$avg.width
#Tamaño de cada cluster
km.res$size

#Ancho de silueta para cada observación
sil<-km.res$silinfo$widths[,1:3]
neg_sil_index<-which(sil[,'sil_width']<0)
sil[neg_sil_index, ,drop=FALSE]


#Cálculo del índice de Dunn
#install.packages("fpc")
#library(fpc)
#library(NbClust)

?cluster.stats

#Estadísticos para k-medias
km_stats<-cluster.stats(dist(df),km.res$cluster)



#índice de dunn
km_stats$dunn
km_stats
data(iris)
#Validación externa
table(iris$Species,km.res$cluster)

#Calcular las estadísticas de cluster
species<-as.numeric(iris$Species)
clust_stats<-cluster.stats(d=dist(df),species,km.res$cluster)
#índice Rand
clust_stats$corrected.rand
#índice de Meila
clust_stats$vi


#Comparativo de validación externa
#Realizando PAM
pam.res<-eclust(df,"pam",k=3,graph=FALSE)
table(iris$Species,pam.res$cluster)
cluster.stats(d=dist(df),species,pam.res$cluster)$corrected.rand



#Realizando Clustering jerárquico
res.hc<-eclust(df,"hclust",k=3,graph=FALSE)
table(iris$Species,res.hc$cluster)
cluster.stats(d=dist(df),species,res.hc$cluster)$corrected.rand

#Para iris
#Validación interna
library(cluster)
library(clValid)

clmethods<-c("hierarchical","kmeans","pam")
intern <- clValid(df,nClust=2:6,clMethods=clmethods,validation="internal")
summary(intern)

#Medidas de estabilidad
clmethods<-c("hierarchical","kmeans","pam")
stab <- clValid(df,nClust=2:6,clMethods=clmethods,validation="stability")
optimalScores(stab)
