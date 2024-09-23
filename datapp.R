#DataPreprocessing

x=read.csv("datapreprocessing.csv")

View(x)
dim(x)

##name of attributes
names(x)

##+++++++++++++++++
#Inconsistent values

#Gender

class(x$Gender)
table(x$Gender)

#child is an inconsistent value for Gender
x$Gender=gsub("Child", NA, x$Gender)
table(x$Gender)
x$Gender
table(is.na(x$Gender))

#Height  ##nature numerical 
summary(x$Height)
class(x$Height)
x$Height

x$Height=gsub("164 cm ", 164, x$Height)
x$Height=gsub("186cm", 186, x$Height)
x$Height
class(x$Height)
x$Height=as.numeric(x$Height)
summary(x$Height)
class(x$Height)

#Weight
summary(x$Weight)
x$Weight
table(x$Weight)
x$Weight=gsub("Male", NA, x$Weight)
x$Weight
class(x$Weight)

x$Weight=as.numeric(x$Weight)
summary(x$Weight)

##
x=read.csv("datapreprocessing.csv")
View(x)
x_height=x$Height
class(x_height)


x_height
x_height=replace(x_height, x_height %in% c("186cm", "164 cm "), c(186, 164))
x_height
x_height=as.numeric(x_height)
summary(x_height)

##+++++++++++++++++
#Chapter 2: Data pre processing
#Duplicated rows 
x=read.csv("datapreprocessing.csv")
View(x)
##package tidyverse
library(tidyverse)
dp_value=duplicated(x)
dp_value
table(dp_value)


##Lets introduce some duplicated objects (rows)
dp_rows=x[c(1,2), ]
View(dp_rows)

y=rbind(x, dp_rows)
View(y)

dp_object=duplicated(y)
table(dp_object)

y[duplicated(y), ]   #this gives the duplicated rows with all details

###how to remove duplicated Rows/Objects
y_u=unique(y)
y_dup=duplicated(y_u)
table(y_dup)


##+++++++++++++++++
#+Chapter 2: Data pre processing+#
#Missing Values#

#install.packages("mice")
library(mice)

x=read.csv("navalues.csv")
View(x)   ##missing values present

##spotting the missing values
is.na(x)
colSums(is.na(x))   #it gives the # of NAs in each col.
sum(is.na(x$Gender))  #for indiv col.
sum(is.na(x))   #for the entire dataset

md.pattern(x)

#+How to deal with missing values+
y=x
View(y)

#1: omit the rows with missing values>>> the easiest way to deal NAs
y_new=na.omit(y)

View(y_new)
colSums(is.na(y_new))
md.pattern(y_new)
dim(y_new)
dim(y)
8/20   ##if you remove about 5% of your objects---you are still fine

###calculations in the presence of NAs
sum(is.na(x$GPA))
mean(x$GPA)
mean(x$GPA, na.rm=T)
median(x$GPA, na.rm = T)
var(x$GPA, na.rm = T)


#+++Very very very very important discussion here+++

#+systematic missing values vs Random missing values+# 
View(x)

#+test for random OR systematic Missing Values 
##Weight 
sum(is.na(x$Weight))

dum_na=is.na(x$Weight)  #binary variable recall from ITC 255
dum_na 
table(dum_na)

#Check whether missing values on Weight is related to Gender+
#Variable Gender: 
#test whether dum_na and Gender are associated
#Gender is QL  and dum_na is QL(binary)
#which test of association we use here>>>Chi-square test 
chisq.test(x$Gender, dum_na)  #No association, i.e. random missing values


#Check whether missing values on Weight is related to Height+
#Binary and QNT>t-test

t.test(x$Height~dum_na)   ##Hence no association, i.e. random missing values
###if the missing values are at random then we can impute them>>estimate them
##mice package
names(x)

help("mice")

#If missing values are random, you can estimate most of the missing the values
#using
x.fill=mice(x)
x.cmpl=complete(x.fill)
colSums(is.na(x.cmpl))
View(x.cmpl)
md.pattern(x.cmpl)
x.nona=na.omit(x.cmpl)
md.pattern(x.nona)
#write.csv(x.nona, file = "no_na_values.csv")



##+++++++++++++++++
#+Chapter 2: Data pre processing
#+outliers

x=read.csv("no_na_values.csv")
View(x)
names(x)
str(x)
###suppose we wanna work with the variable Age
summary(x$Age)
plot(density(x$Age), col="blue") #may be out-liers on the righthandside
##check for the outliers
boxplot(x$Age, horizontal = T)
boxplot.stats(x$Age)

x_out=boxplot.stats(x$Age)[4]
min(x_out$out)
###We remove the outliers only from Age
age_new=x$Age[x$Age<24]
boxplot(age_new, horizontal = T)

##you want to have a new dataset 
x_new=x[x$Age<24, ]
View(x_new)


###height
boxplot(x$Height, horizontal = T)
boxplot.stats(x$Height)

x_new_height=x[x$Height>58, ]
boxplot(x_new_height$Height, horizontal = T)



##+++++++++++++++++
#Chapter 2: Data pre processing
#Variable transformations

names(x)
##Height in cm>>>generate a new var height in foot/feet
#1 cm=0.0328084 feet

height_in_feet=x$Height*0.0328084
height_in_feet
x=cbind(x, height_in_feet)
View(x)

##Decoding of the non-numerical var
##Gender
factor(x$Gender)
levels(x$Gender)
help("levels")
##0: Female, 1: Male
Gender_new=c()    ##empty vector 

for (i in 1:length(x$Gender)){
  if (x$Gender[i]=="Female"){
    Gender_new[i]=0
  } else {
    Gender_new[i]=1
  }
}

x=cbind(x, Gender_new)
View(x)
