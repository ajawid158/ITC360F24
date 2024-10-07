##Linear Regression Classification

x=read.csv("employee.csv")
names(x)
dim(x)
head(x)
View(x)


#problem:::predict the class of employees in terms of whether they go to
  #gym or not

#Y=Gym [yes=1. no=0]
#X=Gender, Age, Weight, Whr

0.7*30

s=sample(nrow(x), 21)

x.train=x[s,]
x.test=x[-s,]

View(x.train)
View(x.test)
##Predict the MS of the employees let say using their Age, Gender, Salary
#Model 0 Whether sobd going to gym or NOT?
#we think that younger people are more likely to go to gym 

m0=lm(Gymcode~Age, data=x.train)

View(x.test)
m0_pr=predict(m0, x.test)
m0_pr

gym_pred=ifelse(m0_pr>0.5, 1, 0)   ##to create a dummy variable
gym_pred
x.test$Gymcode

table(x.test$Gymcode, gym_pred)   #confussion matrix
##wrog classifications 22%     Error Rate
plot(x.test$Age, x.test$Gymcode, col='red', pch=16)
points(x.test$Age, gym_pred, col='green', lwd=3)


names(x)
###Model 1
m1=lm(Gymcode~Weight, data=x.train)
m1_pr=predict(m1, x.test)
m1_pr
gym_pred1=ifelse(m1_pr>0.5, 1, 0)
gym_pred1
#ms_pred1
##plot(x.test$Age, m1_pr)
table(x.test$Gymcode, gym_pred1)
###Error rate= 2/6=33%
plot(x.test$Weight, x.test$Gymcode, col='red', pch=16)
points(x.test$Weight, gym_pred1, col='green', lwd=3)

###Suppose we choose model 1 how we use this for the new data
Weight.new=data.frame(Weight=c(60, 90, 85))
pr_new_data=predict(m1, Weight.new)
pr_new_data
cl_new_data=ifelse(pr_new_data>0.5, 1,0)
cl_new_data

names(x)
head(x$Gender, x$GenCode)
###
m2=lm(Gymcode~Weight+GenCode, data=x.train)
m2_pr=predict(m2, x.test)
m2_pr
gym_pred2=ifelse(m2_pr>0.5, 1, 0)
gym_pred2

table(x.test$Gymcode, gym_pred2)

###Choose m2 over m0 and m1

newData=data.frame(Weight=c(60, 90, 85), 
                   GenCode=c(0, 1, 1))
newData
pr_new_data=predict(m2, newData)
pr_new_data
cl_new_data=ifelse(pr_new_data>0.5, 1,0)
cl_new_data
