
#################################################
#################################################

a=read.table(file.choose())
head(a)

#######################################################


status=a$V1
type=a$V2
diversity=a$V3
Dissimilarity=a$V4


LeadingH=a$V10
Leadingshift=a$V6
recentleading=LeadingH+Leadingshift

RearH=a$V11
Rearshift=a$V5
recentrear=RearH+Rearshift

optimumH=a$V12
optimumshift=a$V7
recentopt=optimumH+optimumshift

AbundanceH=a$V13
Abundanceshift=a$V9
recentAbundance=AbundanceH+Abundanceshift

##################### pearson cor  history  similar fig 3  #############################

cor.test(LeadingH,optimumH,method = "pearson")
cor.test(LeadingH,RearH,method = "pearson")
cor.test(RearH,optimumH,method = "pearson")


#################################
par(mfrow=c(3,2))
#################################
plot(LeadingH,optimumH)
lm(optimumH~LeadingH)
abline(-622.69,1.22)


##########################

plot(RearH,optimumH)
lm(optimumH~RearH)
abline(897.35,0.747)


##########################

plot(RearH,LeadingH)
lm(LeadingH~RearH)
abline(1687.57,0.474)



##################### pearson cor  recent  similar fig 3 #############################

cor.test(recentleading,recentopt,method = "pearson")
cor.test(recentleading,recentrear,method = "pearson")
cor.test(recentrear,recentopt,method = "pearson")

#################################
plot(recentleading,recentopt)
lm(recentopt~recentleading)
abline(-532.24,1.092)


##########################

plot(recentrear,recentopt)
lm(recentopt~recentrear)
abline(1055.18,0.659)


##########################

plot(recentrear,recentleading)
lm(recentleading~recentrear)
abline(1690.28,0.4708)






############## univariate lineare regression  ####################
### Elevation ######
lm.1 <- lm(Rearshift~RearH,data=a)
summary(lm.1)
lm.2 <- lm(Leadingshift~LeadingH,data=a)
summary(lm.2)
lm.3 <- lm(optimumshift~optimumH,data=a)
summary(lm.3)
lm.4 <- lm(Abundanceshift~AbundanceH,data=a)
summary(lm.4)


######## status ######
lm444 <- lmer(RearH~factor(status)( 1 | status),data=a)
summary (lm444)
lm88 <- lm((RearH+abs(min(RearH)))^(1/3)~factor(status)-1,data=a)


lm.5 <- lmer(Rearshift~factor(status)+( 1 | status),data=a)
lm.5 <- lm((Rearshift+abs(min(Rearshift)))^(1/3)~factor(status)-1,data=a)

summary (lm.5)

lm.6 <- lm(Leadingshift~status,data=a)
summary (lm.6)
lm.7 <- lm(optimumshift~status,data=a)
summary (lm.7)
lm.8 <- lm(Abundanceshift~status,data=a)
summary (lm.8)


######## type ######

lm.9 <- lm(Rearshift~type,data=a)
summary (lm.9)
lm.10 <- lm(Leadingshift~type,data=a)
summary (lm.10)
lm.11 <- lm(optimumshift~type,data=a)
summary (lm.11)
lm.12 <- lm(Abundanceshift~type,data=a)
summary (lm.12)


 ################# box plot of status ################
	
	par(mfrow=c(2,2))

	boxplot(Rearshift~status,main="rear status")
	boxplot(Leadingshift~status,main="leading status")
	boxplot(optimumshift~status,main="opt status")
	boxplot(Abundanceshift~status,main="abund status")


 ################# box plot of type ################
	
	par(mfrow=c(2,2))

	boxplot(Rearshift~type,main="rear type")
	boxplot(Leadingshift~type,main="leading type")
	boxplot(optimumshift~type,main="opt type")
	boxplot(Abundanceshift~type,main="abund type")

	#abline(lm(y ~ x, data=Meds))

	#####################################################

	b=cbind(recentrear,recentleading,recentopt,RearH,LeadingH,optimumH)
		head(b)



###################### density function ########################
	
	par(mfrow=c(2,2))
############ rearedg ############### 

x2 = RearH
x = recentrear
h=hist(x2, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit2<-seq(min(x2),max(x2),length=40)
yfit2<-dnorm(xfit2,mean=mean(x2),sd=sd(x2))
yfit2 <- yfit2*diff(h$mids[1:2])*length(x2)
plot(xfit2, yfit2, col="red",type="l",main="density plot of rearedgShift")

xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", type="l") 

############leadingedg ############### 


x4 = LeadingH
x3 = recentleading

h1=hist(x4, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit4<-seq(min(x4),max(x4),length=40)
yfit4<-dnorm(xfit4,mean=mean(x4),sd=sd(x4))
yfit4 <- yfit4*diff(h1$mids[1:2])*length(x4)
plot(xfit4, yfit4, col="red",type="l",main="density plot of leadingshift")

#h1=hist(x3, breaks=12, col="red",main="density plot of leadingshift",ylim=c(0,45),xlim=c(-300,3200)) 
xfit3<-seq(min(x3),max(x3),length=40)
yfit3<-dnorm(xfit3,mean=mean(x3),sd=sd(x3))
yfit3<- yfit3*diff(h1$mids[1:2])*length(x3)
lines(xfit3, yfit3, col="blue", lwd=2) 



############ optimu ############### 
x6 <- optimumH
x5 = recentopt

h2=hist(x6, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit6<-seq(min(x6),max(x6),length=40)
yfit6<-dnorm(xfit6,mean=mean(x6),sd=sd(x6))
yfit6 <-yfit6*diff(h2$mids[1:2])*length(x6)
plot(xfit6, yfit6, col="red",type="l",main="density plot of optimu")


#h2=hist(x5, breaks=12, col="red",xlim=c(-400,3000),ylim=c(0,40),main="density plot of optimu") 
xfit5<-seq(min(x5),max(x5),length=40)
yfit5<-dnorm(xfit5,mean=mean(x5),sd=sd(x5))
yfit5 <- yfit5*diff(h2$mids[1:2])*length(x5)
lines(xfit5, yfit5, col="blue", lwd=2) 


############ aboundenc ############### 
x8 = AbundanceH
x7 = recentAbundance

h4=hist(x8, breaks=12,xaxt="n",freq = NULL, col="red") 
xfit8<-seq(min(x8),max(x8),length=40)
yfit8<-dnorm(xfit8,mean=mean(x8),sd=sd(x8))
yfit8 <-yfit8*diff(h4$mids[1:2])*length(x8)
plot(xfit8, yfit8, col="red",type="l",main="density plot of aboundenc")


xfit7<-seq(min(x7),max(x7),length=40)
yfit7<-dnorm(xfit7,mean=mean(x7),sd=sd(x7))
yfit7 <- yfit7*diff(h4$mids[1:2])*length(x7)
lines(xfit7, yfit7, col="blue", lwd=2) 




###########################################
###########################################
xfit2<-seq(min(x2),max(x2),length=40)
yfit2<-dnorm(xfit2,mean=mean(x2),sd=sd(x2))
yfit2 <- yfit2*diff(h$mids[1:2])*length(x2)
plot(xfit2, yfit2, col="red",type="l",main="density plot of rearedgShift")

xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", type="l") 

xfit4<-seq(min(x4),max(x4),length=40)
yfit4<-dnorm(xfit4,mean=mean(x4),sd=sd(x4))
yfit4 <- yfit4*diff(h1$mids[1:2])*length(x4)
plot(xfit4, yfit4, col="red",type="l",main="density plot of leadingshift")

xfit3<-seq(min(x3),max(x3),length=40)
yfit3<-dnorm(xfit3,mean=mean(x3),sd=sd(x3))
yfit3<- yfit3*diff(h1$mids[1:2])*length(x3)
lines(xfit3, yfit3, col="blue", lwd=2) 

xfit6<-seq(min(x6),max(x6),length=40)
yfit6<-dnorm(xfit6,mean=mean(x6),sd=sd(x6))
yfit6 <-yfit6*diff(h2$mids[1:2])*length(x6)
plot(xfit6, yfit6, col="red",type="l",main="density plot of optimu")


xfit5<-seq(min(x5),max(x5),length=40)
yfit5<-dnorm(xfit5,mean=mean(x5),sd=sd(x5))
yfit5 <- yfit5*diff(h2$mids[1:2])*length(x5)
lines(xfit5, yfit5, col="blue", lwd=2) 

xfit8<-seq(min(x8),max(x8),length=40)
yfit8<-dnorm(xfit8,mean=mean(x8),sd=sd(x8))
yfit8 <-yfit8*diff(h4$mids[1:2])*length(x8)
plot(xfit8, yfit8, col="red",type="l",main="density plot of aboundenc")


xfit7<-seq(min(x7),max(x7),length=40)
yfit7<-dnorm(xfit7,mean=mean(x7),sd=sd(x7))
yfit7 <- yfit7*diff(h4$mids[1:2])*length(x7)
lines(xfit7, yfit7, col="blue", lwd=2) 

