data11 <- read.csv("first_one.csv",header = FALSE,stringsAsFactors = F,na.strings =c("NA","0"))
data12 <- read.csv("first_two.csv",header = FALSE,stringsAsFactors = F,na.strings =c("NA","0"))

data21 <- read.csv("second_one.csv",header = F,stringsAsFactors = F,na.strings =c("NA","0"))
data22 <- read.csv("second_two.csv",header = F,stringsAsFactors = F,na.strings =c("NA","0"))

data31 <- read.csv("third_one.csv",header = F,stringsAsFactors = F,na.strings =c("NA","0"))
data32 <- read.csv("third_two.csv",header = F,stringsAsFactors = F,na.strings =c("NA","0"))

data41 <- read.csv("fourth_one.csv",header = F,stringsAsFactors = F,na.strings =c("NA","0"))
data42 <- read.csv("fourth_two.csv",header = F,stringsAsFactors = F,na.strings =c("NA","0"))

head(data11)[1:4]
returnR2 <- function(data1,data2){
  r2=c(0)
  for(i in 1:nrow(data1)){
    if(length(which(data1[i,]!='NA'))<30) r2[i]=0
    else {
      l<-c(0)
      for(j in 1:ncol(data1)){
      w1 <- as.numeric(data2[,j][-i])
      da <- as.numeric(data1[,j][-i])
      loc <- which(w1!='NA'&da!='NA')
      w1new <- w1[loc]
      danew <- da[loc]
      w=w1new/sum(w1new)
      l[j]=sum(danew*w)
       }
    r2[i]=summary(lm(as.numeric(data1[i,])~l))$r.squared
    }
    }
  return(r2)
}
d<-returnR2(data11,data12)

d1<-returnR2(data11[1:1300,],data12[1:1300,])
d2<-returnR2(data11[1301:2663,],data12[1301:2663,])
d <- c(d1,d2)

m1<-returnR2(data21[1:1300,],data22[1:1300,])
m2 <- returnR2(data21[1301:2663,],data22[1301:2663,])
m <- c(m1,m2)

n1<-returnR2(data31[1:1300,],data32[1:1300,])
n2 <- returnR2(data31[1301:2663,],data32[1301:2663,])
n <- c(n1,n2)

t1<-returnR2(data41[1:1300,],data42[1:1300,])
t2 <- returnR2(data41[1301:2663,],data42[1301:2663,])
t <- c(t1,t2)
