#Ejercicio 

#Vamos a revisar la tendencia en los clústeres
#Si no tienes instalados factoextra y clustertend, deberás instalarlos.
#install.packages("factoextra")
#install.packages("clustertend")

library("factoextra")
library("clustertend")

#Vamos a trabajar con la base de datos de iris.
head(iris,3)
df<-iris[,-5]

#Vamos a generar una tabla de datos aleatoria a partir de iris.
#función apply: al dataframe, aplicar a columnas, la función: 
#distribución uniforme a lo largo de cada entrada del df, variando del mín al max.
#de los datos.
random_df<-apply(df,2,function(x){runif(length(x),min(x),max(x))})
random_df<-as.data.frame(random_df)

#Los datos con los que vamos a trabajar
head(df)
head(random_df)

#Estandarizamos los datos de ambos data frames
df<-scale(df)
random_df<-scale(random_df)


#Graficamos los datos reales y los datos aleatorios.
fviz_pca_ind(prcomp(df),title="PCA-iris",habillage=iris$Species,
             geom="point",ggtheme=theme_classic())

fviz_pca_ind(prcomp(random_df), title="PCA-aleatorios",
             geom="point",ggtheme=theme_classic())

#Fijamos una semilla
set.seed(123)
km.res1<-kmeans(df,3)
fviz_cluster(list(data=df,cluster=km.res1$cluster),ellipse.type="norm",
             geom="point",stand=FALSE,palette="jco",
             ggtheme=theme_classic())

km.res2<-kmeans(random_df,3)
fviz_cluster(list(data=random_df,cluster=km.res1$cluster),ellipse.type="norm",
             geom="point",stand=FALSE,palette="jco",
             ggtheme=theme_classic())

fviz_dend(hclust(dist(random_df)),k=3,k_colors="jco",
         as.ggplot=TRUE,show_labels=FALSE)


#Forma 1
#Notar que la forma 1 entrega valores 1-H, por lo que buscamos
#valores cercanos a 0

set.seed(123)
hopkins(df,n=nrow(df)-1)
hopkins(random_df,n=nrow(random_df)-1)

#Forma 2
# Buscar valores cercanos a 1
res1<-get_clust_tendency(df,n=nrow(df)-1)
res1$hopkins_stat

res2<-get_clust_tendency(random_df,n=nrow(df)-1)
res2$hopkins_stat

#Para el método visual
fviz_dist(dist(df),show_labels=FALSE)+labs(title="Datos de Iris")
fviz_dist(dist(random_df),show_labels=FALSE)+labs(title="Aleatorios")


#Número óptimo de clústeres
#install.packages("NbClust")
library(NbClust)
library(cluster)

fviz_nbclust(df,kmeans,method="wss")+labs(title="Elbow")
fviz_nbclust(df,kmeans,method="silhouette")+labs(title="silhouette")
fviz_nbclust(df,kmeans,method="gap_stat")+labs(title="Gap statistic")

nb<-NbClust(df,distance="euclidean",min.nc=2,max.nc=11,method="kmeans")

fviz_nbclust(nb)













