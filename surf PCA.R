# Principal Component Analysis for surf data
# thedatagame.net
library(ggplot2)
library(gclus)


# load data
surf <- read.csv("https://dl.dropboxusercontent.com/u/13904341/Data/Surf%20data.csv")
attach(surf)

# name columns
row.names(surf) <- surf$NAME
surf$NAME <- NULL

# visualise correlations with a scatterplot matrix
surf.cor <- cor(surf)
surf.colour <- dmat.color(surf.cor, colors=heat.colors(6))
cpairs(surf, panel.colors = surf.colour, pch=".",gap=.5, border.color = "grey70", upper.panel=panel.smooth, main = "Surf Data Correlations")


# ignore earnings, total points, average heat score and REA for PCA
surf2 <- surf
surf2$Earnings <- NULL
surf2$TFP <- NULL
surf2$AHS <- NULL
surf2$REA <- NULL


# Principal Components Analysis
surf.pca <- princomp(surf2, cor = TRUE, scale = TRUE, scores = TRUE)
summary(surf.pca)


# plot surfers 
pca.data <- data.frame(cbind(Factor2 = surf.pca$scores[,2], Factor1 = surf.pca$scores[,1]))
ggplot(pca.data, aes(Factor2, Factor1)) + 
	geom_point(aes(color = TFP), shape=19, size=3.5) +
	scale_colour_gradientn(colours=rev(heat.colors(nrow(pca.data)))) + 
	geom_text(aes(label=rownames(pca.data)), hjust=1.1, vjust=1.1, size=3) +
	ggtitle(expression(atop("2014 WSL Season", atop(italic("Total Final Points"), ""))))


# analyse factor loadings
factor.loading <- cbind(factor1=surf.pca$loadings[,1], factor2=surf.pca$loadings[,2])
print(factor.loading)

## end ##
