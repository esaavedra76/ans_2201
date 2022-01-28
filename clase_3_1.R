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

summary(res.pca)
