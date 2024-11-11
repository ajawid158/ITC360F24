#Unsupervised Learning Methods
#PCA

library(corrplot)
library(dplyr)
x=read.csv("employee.csv")
View(x)

###Can we use less number of financial variables 

fv=x %>%
  select(Salary, Tax, Spending, Sving)

names(fv)
View(fv)
##check the correlation
cr.fv=cor(fv)
cr.fv
corrplot(cr.fv, method = "pie", 
         type="lower")
##apply PCA
fv_pca=prcomp(fv, scale=TRUE)
summary(fv_pca)
plot(fv_pca, type="l")

View(x)

pca_select=fv_pca$x
head(pca_select)
dim(pca_select)

head(pca_select)

pca_new=pca_select[,c(1,2)] %>%
  as.data.frame()
View(pca_new)
head(pca_new)
cor(pca_new$PC1, pca_new$PC2)
##instead of original 4 F_vars, we use the first 2 PCAs
fv=fv%>%
  cbind(pca_new)
View(fv)
cor(fv$PC1, fv$PC2)

#++++++++++++++++NEW DATASET+++++++++++++#

y=read.csv("rainfall.csv")
View(y)
names(y)
cr.y=cor(y)
cr.y
corrplot(cr.y, method = "pie", 
         type="lower")
y.rain.winter=y %>%
  select(r1, r2, r3)
rain_pca=prcomp(y.rain.winter, scale=TRUE)
summary(rain_pca)
plot(rain_pca, type="l")
pca_select=rain_pca$x
head(pca_select)
pca_new=pca_select[,c(1,2)] %>%
  as.data.frame()
head(pca_new)
cor(pca_new$PC1, pca_new$PC2)



y= y%>%
  cbind(pca_new)
View(y)
