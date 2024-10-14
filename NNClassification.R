#ANN Classification
##a new data set on students scores and grades (real dataset)
g=read.csv("grades1.csv")
nrow(g)
#install.packages("neuralnet")
library(neuralnet)
head(g)

#problem: classify the students in A or NotA using thier
#Oral score and Q Score

x=data.frame(g$Oral, g$Q, g$Grade)
colnames(x)=c("Oral", "Q", "Grade")
head(x)
#Split the dataset x into training and test

s=sample(nrow(x), floor(0.8*nrow(x)))
x.train=x[s, ]
x.test=x[-s, -3]
x.test.y=x[-s, 3]
head(x.test)
head(x.test.y)

##Run the ANN classification Model

nn=neuralnet(Grade~Oral+Q, data =x.train, hidden = 3,
             act.fct = "logistic", linear.output = FALSE)
plot(nn)

#help("neuralnet")
##Compute the error rate using test dataset 
pr.test=compute(nn, x.test)


p1=pr.test$net.result
head(p1)

pred=ifelse(p1>0.5, 1, 0)  ###dummy variable
pred
table(x.test.y, pred)   ##confussion matrix

Err.rate=3/NROW(x.test)
Err.rate

###Use the ANN model to classify new students

Oral=c(94, 89, 60)
Q=c(97, 88, 70)

new.st=data.frame(cbind(Oral, Q))
head(new.st)
pr.test=compute(nn, new.st)

p1=pr.test$net.result
p1
pred0=ifelse(p1>0.5, 1, 0)  ##dummay variable
pred0

##Part 2 of Homework: consturct 2 more models using NN classifcation
#and compare their Error Rates