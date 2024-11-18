##+++++++++++++++++++++++++++++++++++++++++++++##
#                                               #
#           Clustering//KMeans-Clustering       #
#                                               #
##++++++++++++++++++++++++++++++++++++++++++++++#
library(factoextra)
library(dplyr)
x=read.csv("employee.csv")
names(x)

##Lets cluster the employees wrt physical characteristics

names(x)
##select the two variables of interest 
x1=x %>%
  select(Height, Weight)
head(x1)
#help("fviz_nbclust")
##apply k-means clustering to cluster the employees
#First: Optimal k
n_clust=fviz_nbclust(x1, kmeans, method = "silhouette")
n_clust

##Hence: k=2
k_m=kmeans(x1, 2)

h_w_cl=k_m$cluster
h_w_cl
x1=cbind(x1, h_w_cl)
View(x1)

##display it 
plot(x1$Height, x1$Weight,
     col=x1$h_w_cl, 
     xlab = "Height", 
     ylab = "Weight", 
     pch=9,
     main = "K-means clusters")
legend(165,90,unique(x1$h_w_cl),
       col=1:length(x1$h_w_cl),pch=9)

#Now we can use the h_w_clust as our classification variable
head(x1)
x1=x1 %>%
  mutate(y=ifelse(h_w_cl==2, 1,0))
View(x1)

