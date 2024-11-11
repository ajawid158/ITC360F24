##+++++++++++++++++++++++++++++++++++++++++++++##
#                                               #
#        Clustering//H-Clustering               #
#                                               #
##++++++++++++++++++++++++++++++++++++++++++++++#
library(dplyr)
library(ggplot2)
library(factoextra)

g=read.csv("employee.csv")
names(g)
dim(g)
View(g)
##cluster the employees wrt to their H/W

g1=g %>%
  select(Height, Weight)

head(g1)

n_clust=fviz_nbclust(g1, FUNcluster = hcut,  method = "silhouette")
n_clust

h1=hclust(dist(g1), method = "average")
plot(h1)

h1=hclust(dist(g1), method = "complete")

plot(h1)

hw.cl=cutree(h1, 2)
hw.cl

g1=g1 %>%
  mutate(hw.cl)
head(g1)
View(g1)

g2=data.frame(g1$Height, g1$Weight, hw.cl)
View(g2)
##+++++++++++++++++##+++++++++++++++##++++++++++++##
###visualize the clusters
plot(g2$g1.Height, g2$g1.Weight,
     col=g2$hw.cl, 
     xlab = "Height", 
     ylab = "Weight", 
     main = "H-clust of H/W", 
     pch=8)

legend(165,90,unique(g1$hw.cl),
       col=1:length(g1$hw.cl),pch=8)

##++++++++++++++++++++++++++++++++++++++++++++++++##

###Now we have labeled the variables, create a dummy classifier 
head(g1)
g1=g1%>%
  mutate(y=ifelse(hw.cl==1, 0, 1))
head(g1)
View(g1)
##Lets make prediction using logistic regression
#usign y as classifier

cl0=glm(y~Height+Weight, data = g1, family = "binomial")
fit0=cl0$fitted.values
plot(g1$Height, fit0)

###New data 
Height=c(186, 166)
Weight=c(90, 60)
new.data=data.frame(cbind(Height,Weight))
head(new.data)

p=predict(cl0, new.data, type = "response")
p
round(p)
