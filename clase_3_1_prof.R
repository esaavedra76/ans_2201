#Vamos a trabajar Componentes Principales y FAMD
#Customer_Personality_Analysis
#Importamos desde Excel

library("FactoMineR")
library("factoextra")


#Creamos un data frame, pero sin incluir la primera columna que es el ID
bc <- as.data.frame(Customer_Personality_Analysis[, 2:18])
#Asignamos la primera columna como nombres de los individuos
rownames(bc) <- Customer_Personality_Analysis$ID

#Tenemos variables cualitativas y cuantitativas
summary(bc)

#Definimos dataframe solamente con variables cuantitativas
df <- bc[, 5:17]

#Resumen del tipo de variables
str(df)

#Realizamos el análisis de componentes principales
res.pca <- PCA(df, scale.unit = TRUE, graph = FALSE)

#Revisemos el resumen del análsis de componentes principales
summary(res.pca)

#Obtenemos los valores propios
eig.val <- get_eigenvalue(res.pca)
eig.val

#Dos maneras diferentes de obtener el scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

#Obtenemos las variables
var <- get_pca_var(res.pca)
var
fviz_pca_var(res.pca)

#Para analizar la calidad de representación
var$cos2
library("corrplot")
corrplot(var$cos2, is.corr = FALSE)

#También es posible crear un gráfico de barras con la info.
fviz_cos2(res.pca, choice = "var", axes = 1:2)

#Podemos combinar ambas ideas en un solo gráfico.
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#Ahora vamos a revisar la contribución de las variables a cada PC
var$contrib
corrplot(var$contrib, is.corr = FALSE)

#Usaremos fviz_contrib() para dibujar un gráfico de barras
#para analizar la contribución de cada variable a los PC 1 y 2

fviz_contrib(res.pca, choice = "var", axes = 1)
fviz_contrib(res.pca, choice = "var", axes = 2)
fviz_contrib(res.pca, choice = "var", axes = 1:2)

#Obtenemos la información de los individuos
ind <- get_pca_ind(res.pca)
ind
#Coordenadas de los individuos, calidad de representación y contribución
head(ind$coord)
head(ind$cos)
head(ind$contrib)

#Para crear una gráfica de individuos u observaciones.
fviz_pca_ind(res.pca, geom = "point")

#Podemos cambiar el tamaño de los puntos de acuerdo con cos2 de los individuos
fviz_pca_ind(res.pca, pointsize = "cos2",
             pointshape = 21, fill = "#E7B800",
             repel = TRUE)
#Crear un gráfico de barras para la calidad de representación de los individuos
#en el mapa de facotres.
fviz_cos2(res.pca, choice = "ind")

#Visualizar la contribución de los individuos a los primeros componentes
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")


#Hasta aquí, hemos realizado el análisis solamente con las variables cuantitativas


#Ahora vamos a realizar el análisis mixto FAMD
res.famd <- FAMD(bc, graph = FALSE)

#Obtenemos la información de los eigenvalores
eig.val1 <- get_eigenvalue(res.famd)
head(eig.val1)

#Graficamos el scree plot
fviz_screeplot(res.famd)

#Obtenemos las variables
var1 <- get_famd_var(res.famd)

#Podemos acceder a los distintos componentes:
head(var1$coord)
head(var1$cos2)
head(var1$contrib)

#Para graficar las variables tanto cuantitativas como cualitativas
fviz_famd_var(res.famd, repel = TRUE)

#Para graficar la contribución a las dimensiones
fviz_contrib(res.famd, "var", axes = 1)
fviz_contrib(res.famd, "var", axes = 2)

#Para extraer las variables cuantitativas
quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var

#Graficamos las variables cuantitativas
fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")

#Gráfica incluyendo su contribución
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#Gráfica destacando la calidad de la representación
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#Gráfica de las variables cualitativas
quali.var <- get_famd_var(res.famd, "quali.var")
quali.var

#Para visualizar las variables cualitativas:
fviz_famd_var(res.famd, "quali.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

#Para graficar a los individuos
ind <- get_famd_ind(res.famd)
ind

#Individuos con perfiles similares aparecerán cercanos entre sí.
fviz_famd_ind(res.famd, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
