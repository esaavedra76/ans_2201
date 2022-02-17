# parte 1

Country_data <- read.csv('C:\\Users\\saave\\Documents\\DataSpellDirectories\\ans_2201\\Country_data.csv', header = TRUE, fileEncoding = 'UTF-8-BOM')
df <- Country_data[, -1]
rownames(df) <- Country_data$country
head(df, 3)

summary(df)
apply(df, 2, sd)

outliersfun <- function(col) {
  t_q25 <- quantile(col, 0.25)
  t_q75 <- quantile(col, 0.75)
  iqr <- t_q75 - t_q25
  lim_inf <- t_q25 - 1.5 * iqr
  lim_sup <- t_q75 + 1.5 * iqr
  return(sum((col > lim_sup) | (col < lim_inf)))
}

apply(df, 2, outliersfun)

par(mfrow = c(1, 2))
hist(df$child_mort)
boxplot(df$child_mort)

par(mfrow = c(1, 2))
hist(df$exports)
boxplot(df$exports)

par(mfrow = c(1, 2))
hist(df$health)
boxplot(df$health)

par(mfrow = c(1, 2))
hist(df$imports)
boxplot(df$imports)

par(mfrow = c(1, 2))
hist(df$income)
boxplot(df$income)

par(mfrow = c(1, 2))
hist(df$inflation)
boxplot(df$inflation)

par(mfrow = c(1, 2))
hist(df$life_expec)
boxplot(df$life_expec)

par(mfrow = c(1, 2))
hist(df$total_fer)
boxplot(df$total_fer)

par(mfrow = c(1, 2))
hist(df$gdpp)
boxplot(df$gdpp)

df_scaled <- scale(df, center = FALSE, scale = TRUE)
summary(df_scaled)

# install.packages("corrplot")
library(corrplot)
correlaciones <- cor(df)
corrplot(correlaciones)

# parte 2

library("FactoMineR")
library("factoextra")

res.pca <- PCA(df, scale.unit = TRUE, graph = TRUE)
summary(res.pca)

var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr = FALSE)

fviz_pca_biplot(res.pca, geom = "point")

# parte 3

library(factoextra)
library(stats)
library(cluster)

df_scaled <- scale(df)

fviz_nbclust(df_scaled, kmeans, method = "wss")
clusters_count <- 4
fviz_nbclust(df_scaled, kmeans, method = "wss") + geom_vline(xintercept = clusters_count, linetype = 2)
km.res <- kmeans(df_scaled, clusters_count)
print(km.res)
aggregate(df, by = list(cluster = km.res$cluster), mean)
dd <- cbind(df, cluster = km.res$cluster)
head(dd)
km.res$cluster
km.res$size
# fviz_cluster(km.res, data = df, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
fviz_cluster(km.res, data = df, ellipse.type = "euclid", star.plot = TRUE, ggtheme = theme_minimal())


fviz_nbclust(df_scaled, pam, method = "silhouette")
pam.res <- pam(df_scaled, 4)
print(pam.res)
dd2 <- cbind(df, cluster = pam.res$cluster)
head(dd2, n = 3)
pam.res$medoids
pam.res$clustering
# fviz_cluster(pam.res, ellipse.type = "t", star, repel = TRUE, ggtheme = theme_classic())
fviz_cluster(pam.res, ellipse.type = "t", star, ggtheme = theme_classic())


fviz_nbclust(df_scaled, clara, method = "silhouette") + theme_classic()
clara.res <- clara(df_scaled, 4, samples = 50, pamLike = TRUE)
print(clara.res)
fviz_cluster(clara.res, ellipse.type = "t", geom = "point", pointsize = 1, ggtheme = theme_classic())
