options(future.globals.maxSize = 4000 * 1024^5)
rm(list=setdiff(ls(), "investors"))

# Loading the Libraries
library(tidyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)    # number of clusters identification
library(PerformanceAnalytics)
library(corrr)
library(PerformanceAnalytics)

# Preparing the Investor Data
investors2 <- investors[, c(1,3,10,19,34)]
investors2 <- na.omit(investors2)
investors2$Regions <- as.numeric(as.factor(investors2$Regions))
investors2$`Investor Type` <- as.numeric(as.factor(investors2$`Investor Type`))

# Correlation Analysis
chart.Correlation(investors2[, 2:5], histogram=TRUE, pch=19, title = "c")
plot(investors2$`Number of Investments`, investors2$`Number of Portfolio Organizations`,
     xlab = "Number of Investments",
     ylab = "Number of Portfolio Organizations",
     main = "No. of Portfolio Organizations VS No. of Investments")

# Correlation Analysis with Network Plot
investors2[, 2:5] %>% correlate() %>% network_plot(min_cor=0.6)

# Investor Data for Clustering (Selecting & Scaling the Required Variables)
investors_clust <- scale(investors2[, c(2, 3, 5)])

# Function to compute total within-cluster sum of square
wss <- function(k) {
        kmeans(investors_clust, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

# Plotting WSS
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "WSS Vs Probable Number of Clusters for Investors Data")

# Cluster Analysis (5 Clusters)
k5_investors <- kmeans(investors_clust, centers = 5, nstart = 25)

# Visualizing the Clusters
p5_investors <- fviz_cluster(k5_investors, geom = "point",  data = investors_clust) + 
        ggtitle("Investors Data k-means Clustering") + 
        theme(plot.title = element_text(size = 20, face = "bold"))
p5_investors

# Creating new Df with Cluster Label
investors3 <- data.frame(cbind(investors2, k5_investors[1]))

# Box-plot for No. of Investments (V1)
boxplot(investors3$Number.of.Investments ~ investors3$cluster,
        xlab = "Cluster",
        ylab = "Number of Investments",
        main = "Investor Data Clusters by No. of Investments")

# Box-plot for No. of Regions (V2)
boxplot(investors3$Regions ~ investors3$cluster,
        xlab = "Cluster",
        ylab = "Regions",
        main = "Investor Data Clusters by Regions")

# Box-plot for No. of Portfolio Orgs (V3)
boxplot(investors3$Number.of.Portfolio.Organizations ~ investors3$cluster,
        xlab = "Cluster",
        ylab = "Number of Portfolio Organizations",
        main = "Investor Data Clusters by Number of Portfolio Organizations")

# Box-plot for No. of Portfolio Orgs (V4)
boxplot(investors3$Investor.Type ~ investors3$cluster,
        xlab = "Cluster",
        ylab = "Investor Type",
        main = "Investor Data Clusters by Investor Types")



