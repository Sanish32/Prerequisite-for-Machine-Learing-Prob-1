#1.i).Loading Data
from<-read.csv("~/Downloads/x.csv")

#1.ii). Finds the two variables having the largest variances
from<-as.data.frame(from) #Making into data frame

#Useless
#length(from[,1]) just checking length
#from  

#Finding mean of each variables in from data frame
am<-apply(from,2,mean)
am

#Just transposing the vector am which stands for arithmetic mean
am<-t(am)

#Useless
#from
#length(from)
#from[,32]
#am
#am[32]

#Useless
#from[,1] just checking elements
#from[,2] just checking elements

#Function that subtracts every elements of 
#variables with respective mean stored in am
abc<-function(x,y){
  
  for (i in 1:length(x)){
    x[,i]=x[,i]-y[i]
  }
  return(x)
}

#Using functions
a<-abc(from,am)
a

#Squaring the difference for each elements of the variables
b<-a^2
b

#Suming all the elements of individual variables
c<-apply(b,2,sum)
c

#Diving the sum we got previously by length of each variables
d<-c/length(from[,1])
d

#Making d into data frame
d<-as.data.frame(d)
d

#Finding the largest variance among 32 variances
l1<-max(d)
l1

#Making subset of d data from
f1<-subset(d,d==l1)
f1

#Making data frame
f1<-as.data.frame(f1)
f1

#Making subset from f1 where there exists only 
#largest variance
sub1<-subset(f1,f1==l1)
sub1

#Just checking the rowname
var1<-rownames(sub1)
var1

#Again making subset from d but this time
#we will ignore the variable which has largest variance
d<-subset(d,d!=l1)
d

#Second largest variance variable
l2<-max(d)
l2


#Making subset of d data from
f2<-subset(d,d==l2)

#Making f2 into data frame
f2<-as.data.frame(f2)
f2

#Making subset whose value is not equal to l1
f2<-subset(f2,f2!=l1)
f2


#Making subset whose value is equal to l2
sub2<-subset(f2,f2==l2)
sub2

#Just checking the rowname
var2<-rownames(sub2)
var2

#Just..
from

#Filtering the desired variable which has 
#first largest variance 
a<-subset(from,select=c(colnames(from)==var1))
a<-c(a[,1])
a

#Filtering the desired variable which has 
#second largest variance 
b<-subset(from,select=c(colnames(from)==var2))
b<-c(b[,1])
b

#1.iii)
#Finally, plotting the scatter plot between two variables
#which has first 2 largest variances among other variables
plot(a,b)

