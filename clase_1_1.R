library('cluster')

data('iris')
head(iris)  # primeros

print(iris)
plot(iris)
summary(iris)
str(iris) # estructura
tail(iris)  # ultimos
dim(iris) # dimension
nrow(iris)  # numero filas
ncol(iris)  # numero cols
colnames(iris)  # nombre cols

iris[1:10,] # primeras 10 filas
iris[, 3:4]  # columnas 3 a la 4
iris[1:10, 3:4] # primeras 10 filas, columnas 3 y 4
iris[, "Species"] # columna por nombre
iris$Species # columna por nombre
iris[iris$Species == "setosa",] # todas las filas cuya columna Species tiene el valor setosa

copia.iris <- iris  # copiar
ls()  # lista de objetos
rm(copia.iris)  # eliminar!!!
ls()
mi.iris <- iris
head(mi.iris)
mi.iris$Petal.Area <- mi.iris$Petal.Length * mi.iris$Petal.Width  # crea nueva columna
head(mi.iris)
mi.iris$Petal.Area <- NULL  # eliminar columna?

print(mi.iris)
mi.iris <- iris[order(iris$Petal.Length),]  # crea(sobreescribe) una nueva copia ordenada segun Petal.Length
print(mi.iris)

hist(iris$Sepal.Width)  # histograma de Sepal.Width

hist(iris$Sepal.Width, main="iris: histograma de la anchura de los sepalos", xlab="anchura del sepalo", ylab="frecuencia", col="steelblue") # histograma con titulo, etiqueta de x y y y color

barplot(table(iris$Species))

boxplot(iris$Sepal.Width ~iris$Species, col="gray", main="Especies de iris segun la anchura del sepalo")  # boxplot del Sepal.Width agrupado segun Species
