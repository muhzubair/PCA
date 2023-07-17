library(datasets)

# Omitting missing values, and converting it into df
st <- na.omit(state.x77)
st <- as.data.frame(st)

# EDA 
summary(st)

# Pair plot
cor(st)
pair(st)

# boxplots
par(mfrow = c(2, 4))
invisible(lapply(1:ncol(st), function(i) boxplot(st[, i])))

par(mfrow=c(2,4))
for (i in 1:length(st)) {
  boxplot(st[,i], main=names(st[i]), type="l")
}

# 2c:

# Standardizing the varibles and doing pca
pca.fit <- prcomp(st, center = T, scale = T)

# Loading matrix
comp <- pca.fit$rotation

# Computing PVE
pc.var <- pca.fit$sdev^2
pve <- pc.var/sum(pc.var)
pve
cumsum(pve)

# Scree plot
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained"
     , ylim = c(0,1), type = 'b', main = "Scree Plot")

# Cum PVE plot
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b',
     main = "Cumulative PVE Plot")


# 2d
library(PerformanceAnalytics)
library(ISLR)

# Correlation between PC1 & PC2 
corr <- rbind(pca.fit$rotation[,1]*pca.fit$sdev[1], pca.fit$rotation[,2]*pca.fit$sdev[2])

#cumulative percentage of the total variability explained by the two components
print(cumsum(pve)[1:2])

# Score matrix 
pca.fit$rotation[,1:2]

# bi plot 
biplot(pca.fit, scale = 0)

