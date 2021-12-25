
######################################################
############ ##################

a=read.table("data1.txt")
head(a)

#################################################################

y1=a$V5
y2=a$V6
y3=a$V7

m1=a$V1
m2=a$V2
m3=a$V3
m4=a$V4




##################### pearson cor ###################################
cor.test(a$V11,a$V6,method = "pearson")
cor.test(a$V10,a$V7,method = "pearson")
cor.test(a$V12,a$V7,method = "pearson")



###################### density function ########################
par(mfrow=c(1,1))
############rearedg ############### 

x2 <- a$V11
x = (a$V5)+x2
h=hist(x2, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit2<-seq(min(x2),max(x2),length=40)
yfit2<-dnorm(xfit2,mean=mean(x2),sd=sd(x2))
yfit2 <- yfit2*diff(h$mids[1:2])*length(x2)
plot(xfit2, yfit2, col="red",type="l",main="density plot of rearedg")

xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", type="l") 

############leadingedg ############### 


x4 <- a$V10
x3 = (a$V6)+x4

h=hist(x4, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit4<-seq(min(x4),max(x4),length=40)
yfit4<-dnorm(xfit4,mean=mean(x4),sd=sd(x4))
yfit4 <- yfit4*diff(h1$mids[1:2])*length(x4)
plot(xfit4, yfit4, col="red",type="l",main="density plot of leadingshift")

#h1=hist(x3, breaks=12, col="red",main="density plot of leadingshift",
ylim=c(0,45),xlim=c(-300,3200)) 
xfit3<-seq(min(x3),max(x3),length=40)
yfit3<-dnorm(xfit3,mean=mean(x3),sd=sd(x3))
yfit3<- yfit3*diff(h1$mids[1:2])*length(x3)
lines(xfit3, yfit3, col="blue", lwd=2) 



############ optimu ############### 

x6 <- a$V12
x5 = (a$V7)+x6

h2=hist(x6, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit6<-seq(min(x6),max(x6),length=40)
yfit6<-dnorm(xfit6,mean=mean(x6),sd=sd(x6))
yfit6 <-yfit6*diff(h2$mids[1:2])*length(x6)
plot(xfit6, yfit6, col="red",type="l",main="density plot of optimu")


#h2=hist(x5, breaks=12, col="red",xlim=c(-400,3000),
#ylim=c(0,40),main="density plot of optimu") 
xfit5<-seq(min(x5),max(x5),length=40)
yfit5<-dnorm(xfit5,mean=mean(x5),sd=sd(x5))
yfit5 <- yfit5*diff(h2$mids[1:2])*length(x5)
lines(xfit5, yfit5, col="blue", lwd=2) 


############ aboundenc ############### 
x8 <- a$V14
x7 =(a$V9)+x8

h4=hist(x8, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit8<-seq(min(x8),max(x8),length=40)
yfit8<-dnorm(xfit8,mean=mean(x8),sd=sd(x8))
yfit8 <-yfit8*diff(h4$mids[1:2])*length(x8)
plot(xfit8, yfit8, col="red",type="l",main="density plot of aboundenc")


#h4=hist(x7, breaks=12, col="red",xlim=c(-2,2),main="density plot of aboundenc") 
xfit7<-seq(min(x7),max(x7),length=40)
yfit7<-dnorm(xfit7,mean=mean(x7),sd=sd(x7))
yfit7 <- yfit7*diff(h4$mids[1:2])*length(x7)
lines(xfit7, yfit7, col="blue", lwd=2) 
############optimu history ############### 






############## univariate lineare regression    model 1 ####################
lm.1 <- lm(y1~m1+m2+m3+m4,data=a)
summary (lm.1)
par(mfrow = c(2,2))
plot(lm.1)

############## univariate lineare regression    model 2 ####################
lm.2 <- lm(y1~m1+m2+m3+m4,data=a)
summary (lm.2)
par(mfrow = c(2,2))
plot(lm.2)

############## univariate lineare regression    model 3 ####################
lm.3 <- lm(y1~m1+m2+m3+m4,data=a)
summary (lm.3)
par(mfrow = c(2,2))
plot(lm.3)


