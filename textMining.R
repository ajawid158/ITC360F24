#required packages
library(tm)
library(SnowballC)
library(dplyr)
library(qdap)

##upload the dataset 
ibis=read.csv("ibishotel.csv", stringsAsFactors = FALSE)
View(ibis)
str(ibis)

##frequency of the words in the Review column 
fr=freq_terms(ibis)
fr
plot(fr)

##Creat documents
ibiscr=Corpus(VectorSource(ibis$Reviews))
ibiscr[[2]]$content


#Preprocessing
#1. trun all words to lower case
ibiscr= ibiscr %>%
  tm_map(tolower)
ibiscr[[2]]$content

#2. Remove punctuations
ibiscr = ibiscr %>%
  tm_map(removePunctuation)
ibiscr[[2]]$content

#3. removing the stopwords 
stopwords("english")
ibiscr =ibiscr %>%
  tm_map(removeWords, c("hotel", "koblenz", "ibis", stopwords("english")))
ibiscr[[2]]$content

#4. Remove numbers
ibiscr = ibiscr %>%
  tm_map(removeNumbers)
ibiscr[[2]]$content

#5. stripping the white space
ibiscr= ibiscr %>%
  tm_map(stripWhitespace)
ibiscr[[2]]$content

#6. Stemming
ibiscr = ibiscr %>%
  tm_map(stemDocument)
ibiscr[[2]]$content

fr1=freq_terms(ibiscr, 5)   #5 most frequent words
plot(fr1)



#Feature Extraction 
ibisfreq=DocumentTermMatrix(ibiscr)

dim(ibisfreq)    #39 rows and 289 variables

inspect(ibisfreq)

inspect(ibisfreq)[1:4, 1:3]   #document 1:4 columns 1:3

findFreqTerms(ibisfreq)
l=findFreqTerms(ibisfreq, lowfreq = 8)
l
length(l)
##we have many features with too many zeros, high sparsity
ibissparse=removeSparseTerms(ibisfreq, 0.80)  
##keep only the terms that appears in 20% or more of the feedback/documents/columns
dim(ibissparse)

inspect(ibissparse)

##convert it to dataframe 
ibis_review=as.data.frame(as.matrix(ibissparse))
View(ibis_review)
dim(ibis_review)

###visualize the freq of terms

ibis_names=colnames(ibis_review)


ibis_freq=c()

for (i in 1:5){
  ibis_freq[i]=sum(ibis_review[,i])
}
ibis_freq
barplot(ibis_freq,
        col=rainbow(5), 
        names.arg = ibis_names, 
        ylim=c(0,20))

####Sentiment analysis 
#create your sentiment variable
View(ibis)
rate=ibis$Rating
View(ibis_review)
ibis_review= ibis_review %>%
  mutate(y=ifelse(rate>3, "Positive","Negative"))
View(ibis_review)

##which terms derive positive rating
#Decision tree Model
library(rpart)
library(rpart.plot)
ibis_sent=rpart(y~.,
                data = ibis_review,
                method = "class")
#prp(ibis_sent)
rpart.plot(ibis_sent)



##+++++++++++++++++++++++##

##APPLE EXAMPLE
###customers feedback on Apple's product

t=read.csv("tweets.csv", stringsAsFactors = FALSE)
##structure of data 
##
View(t)

t= t%>%
  mutate(Negative=Avg<=-1)
table(t$Negative)
View(t)
dim(t)
#term_count=freq_terms(t$Tweet)
#View(term_count)
tfreq=freq_terms(t$Tweet, 10)
plot(tfreq)
##curpus//set of documents
crps=Corpus(VectorSource(t$Tweet))

crps[[3]]$content


##preprocessing
#1 lower case
crps = crps %>%
  tm_map(tolower)
crps[[2]]$content

#2 remove the punctuations
crps=crps%>%
  tm_map(removePunctuation)
crps[[1]]$content

#3 remove stopwords
stopwords("english")

crps= crps %>%
  tm_map(removeWords, c("apple", stopwords("english")))
crps[[2]]$content

#4 Stemming 
crps=crps %>%
  tm_map(stemDocument)
crps[[2]]$content

#5 Strip white space 
crps = crps %>%
  tm_map(stripWhitespace)
crps[[2]]$content

x=freq_terms(crps, 20)
plot(x)

#Features extraction

freq=DocumentTermMatrix(crps)  #transform the words into Features
freq

inspect(freq[1:10,1:5])   ##first 10 rows of the first 5 cols
inspect(freq)

findFreqTerms(freq)
x=findFreqTerms(freq, lowfreq = 50)   ### create a sub matrix with at least 20 times mentioned
length(x)

#Take care of so many terms/columns/features with so many 0s/not mentioned many times, high sparsity
#

freq1=removeSparseTerms(freq, 0.98)  #keep terms which are mentioned in 2% or more of the reviews 100%-99%=1% 
freq1
tspar=as.data.frame(as.matrix(freq1))
head(tspar)
dim(tspar)
colnames(tspar)=make.names(colnames(tspar))
View(tspar)
sum(tspar$best)   #how many times best is mentioned
hist(tspar$iphon)



##Sentiment Analysis
#install.packages("caTools")
library(caTools)

tspar=tspar %>%
  cbind(t$Negative)
View(tspar)
dim(tspar)
sum(tspar$care)

set.seed(123)
splt=sample.split(tspar$`t$Negative`, SplitRatio = 0.7)
train=subset(tspar, splt==TRUE)
test=subset(tspar, splt==FALSE)

##
library(rpart)
library(rpart.plot)
tweetcart=rpart(`t$Negative`~., data = train, method = "class")
prp(tweetcart)
predcart=predict(tweetcart, newdata=test, type="class")
table(test$`t$Negative`, pred)
