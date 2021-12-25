#################################
### Power Analyse, 1st example ###
#################################

##1. generate this 3 data sets
frosch1 <- rnorm(100, 30, 1)
frosch2 <- rnorm(100, 30, 2)
frosch5 <- rnorm(100, 30, 5)

##2. visualize that the samples have a normal distribution
hist(frosch1)
hist(frosch2)
hist(frosch5)
boxplot(frosch1)
boxplot(frosch2)
boxplot(frosch5)
qqnorm(frosch1)
qqnorm(frosch2)
qqnorm(frosch5)

##3. show how the variances influence the distribution
##all plots together with equally scaled axes

windows()#opens a separate display for bigger graphs
par(mfrow=c(3,3)) # partitioning of the display
hist(frosch1, nclass=10, xlim=c(10,50), ylim=c(0,25), xlab="Body mass [g]", ylab="number", main="Histogram - Std = 1")
hist(frosch2, nclass=10, xlim=c(10,50), ylim=c(0,25), xlab="Body mass [g]", ylab="number", main="Histogram - Std = 2")
hist(frosch5, nclass=10, xlim=c(10,50), ylim=c(0,25), xlab="Body mass [g]", ylab="number", main="Histogram - Std = 5")
boxplot(frosch1, frosch2, frosch5, names = c("1","2","5"), main = "Boxplots for Std = 1, 2 und 5", ylab="Body mass [g]")
qqnorm(frosch1, main="Quantil-Quantil-Plot - Std = 1")
qqnorm(frosch2, main="uantil-Quantil-Plot - Std = 2")
qqnorm(frosch5, main="Quantil-Quantil-Plot - Std = 5")

################################
###Power Sampling, example 2###
################################

##calculating sample size assuming different variance
##we fix the power to 0.8
##we compare two samples (north and south slope)
##we test only one side: north slope frogs weigh more than south slope frogs
power.t.test(delta=2, sd=1, power = 0.8,
             type="two.sample", alternative="one.sided")
power.t.test(delta=2, sd=2, power = 0.8,
             type="two.sample", alternative="one.sided")
power.t.test(delta=2, sd=5, power = 0.8,
             type="two.sample", alternative="one.sided")

##visualisation
n.1.5<-power.t.test(delta=2, sd=1, power=0.5,
             type="two.sample", alternative="one.sided")
n.1.7<-power.t.test(delta=2, sd=1, power=0.7,
             type="two.sample", alternative="one.sided")
n.1.9<-power.t.test(delta=2, sd=1, power=0.9,
             type="two.sample", alternative="one.sided")

n.2.5<-power.t.test(delta=2, sd=2, power=0.5,
             type="two.sample", alternative="one.sided")
n.2.7<-power.t.test(delta=2, sd=2, power=0.7,
             type="two.sample", alternative="one.sided")
n.2.9<-power.t.test(delta=2, sd=2, power=0.9,
             type="two.sample", alternative="one.sided")

n.5.5<-power.t.test(delta=2, sd=5, power=0.5,
             type="two.sample", alternative="one.sided")
n.5.7<-power.t.test(delta=2, sd=5, power=0.7,
             type="two.sample", alternative="one.sided")
n.5.9<-power.t.test(delta=2, sd=5, power=0.9,
             type="two.sample", alternative="one.sided")

##we can do the same but much simpler using loops
n.all<-data.frame(n=NULL,delta=NULL,sd=NULL,sig.level=NULL,power=NULL)
for(i in c(1,2,5)){
  for(j in c(0.5,0.7,0.9)){
    x<-power.t.test(delta=2, sd=i, power=j,type="two.sample", alternative="one.sided")
    n.all<-rbind(n.all,x[1:5])
  }
}

##combination of all results into one data frame
par(mfrow=c(1,1))
n.all<-data.frame(rbind(n.1.5,n.1.7,n.1.9,n.2.5,n.2.7,n.2.9,n.5.5,n.5.7,n.5.9))
plot(n.all$n,n.all$sd,ylab="standard deviation",xlab="sample size in each group",
     main="effect size = 2 g N-S difference")
lines(n.all$n[n.all$power==0.5],n.all$sd[n.all$power==0.5],col=2)
lines(n.all$n[n.all$power==0.7],n.all$sd[n.all$power==0.7],col=3)
lines(n.all$n[n.all$power==0.9],n.all$sd[n.all$power==0.9],col=4)
legend("bottomright",lty=c(1,1,1),col=c(2,3,4),
       legend=c("power=0.5","power=0.7","power=0.9"))
