#logistic Regression Classification

x=read.csv("employee.csv")
names(x)
dim(x)
names(x)
#
s=sample(nrow(x), 24)
x.train=x[s,]
x.test=x[-s,]
###Model 0
L.m0=glm(Gymcode~Age, family = "binomial", data = x.train)
x.fit=fitted(L.m0, x.train)
x.fit
plot(x.train$Age, x.fit, col="blue", pch=12)

###Make predication with the model 
m0_pr=predict(L.m0, x.test)
m0_pr
cl0_pr=ifelse(m0_pr>0.5, 1, 0)   #dummy variable
table(x.test$Gymcode, cl0_pr)   #confussion matrix

#error rate=/6 =
plot(x.test$Age, x.test$Gymcode, col='red', pch=16)
points(x.test$Age, cl0_pr, col='green', lwd=3)


###Model 1
names(x)
L.m1=glm(Gymcode~Weight, family = "binomial", data = x.train)

m1.pr.train=fitted(L.m1, x.train)
#summary(m1.pr.train)
plot(x.train$Weight, m1.pr.train, col="blue", pch=12)

###Make predication with the model 
m1_pr=predict(L.m1, x.test)
cl1_pr=ifelse(m1_pr>0.5, 1, 0)
table(x.test$Gymcode, cl1_pr)
##error rate=/6=

plot(x.test$Weight, x.test$Gymcode, col='red', pch=16)
points(x.test$Weight, cl1_pr, col='green', lwd=3)

## >> choose model


##New data 
Age.new=data.frame(Age=c(20, 26, 60))
cl_pred_new=predict(L.m0, Age.new)
cl_pred_new
class_new=ifelse(cl_pred_new>0.5, 1, 0)
class_new