# install.packages("FactoMineR")
# install.packages("factoextra")

library("FactoMineR")
library("factoextra")

bc <- as.data.frame(Customer_Personality_Analysis[, 2:18])
rownames(bc) <- Customer_Personality_Analysis$ID

summary(bc)

# componentes principales
df <- bc[, 5:17]
str(df)

res.pca <- PCA(df, scale.unit = TRUE, graph = FALSE)
# res.pca <- PCA(df, scale.unit = TRUE, graph = FALSE, ncp = 13)

summary(res.pca)

# obterner los valores propios
eig.val <- get_eigenvalue(res.pca)
eig.val

# screen plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50), ncp = 13)
fviz_screeplot(res.pca)

# obtenemos las variables
var <- get_pca_var(res.pca)
var
var$coord
var$cos2

library("corrplot")

corrplot(var$cos2, is.corr = FALSE)

fviz_cos2(res.pca, choice = "var", axes = 1:2)
fviz_pca_var(res.pca, col.var = "cos2")

var$contrib

ind <- get_pca_ind(res.pca)
ind

ind$coord
fviz_pca_ind(res.pca, label = "none")

fviz_pca_biplot(res.pca, geom = "point")
fviz_pca_biplot(res.pca)
ind

# famd
res.famd <- FAMD(bc, ncp = 50)

eig.val1 <- get_eigenvalue(res.famd)

str(bc)

fviz_screeplot(res.famd, ncp = 23)
