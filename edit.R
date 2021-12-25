rm(list=ls())
library(MASS)
f=function(iter,n,rho)
{
x.hat1=y.hat1=c()
x.hat2=y.hat2=c()
x.hat3=y.hat3=c()
x.hat4=y.hat4=c()

x=y=c()
cor.hat1=c()
cor.hat2=c=c()
cor.hat3=c=c()
cor.hat4=c=c()

cor1=c()
b1=9 ; b2=20
p1=c(0.05,0.9,0.05) ; q1=c(0.04,0.92,0.04)
p2=c(0.15,0.7,0.15) ; q2=c(0.14,0.72,0.14)
p3=c(0.25,0.5,0.25) ; q3=c(0.24,0.52,0.24)
p4=c(0.35,0.3,0.35) ; q4=c(0.34,0.32,0.34)
sigma=matrix(c(1,rho,rho,1),2)
mu.s1=mean(1:9) ; mu.s2=mean(1:20)
a=mvrnorm(n,c(0,0),sigma)

for(j in 1:iter)
{
for(i in 1:n)
{
s1=sample(1:9,1) ; s2=sample(1:20,1)
x[i]=a[i,1] ; y[i]=a[i,2]
T1=sample(c(s2,x[i],b2-x[i]),1,prob=p1)
Z1=sample(c(s1,y[i],b1-y[i]),1,prob=q2)
x.hat1[i]=(T1-q1[1]*mu.s2-q1[3]*b2)/(q1[2]-q1[3])
y.hat1[i]=(Z1-p1[1]*mu.s1-p1[3]*b1)/(p1[2]-p1[3])
T2=sample(c(s2,x[i],b2-x[i]),1,prob=p2)
Z2=sample(c(s1,y[i],b1-y[i]),1,prob=q2)
x.hat2[i]=(T2-q2[1]*mu.s2-q2[3]*b2)/(q2[2]-q2[3])
y.hat2[i]=(Z2-p2[1]*mu.s1-p2[3]*b1)/(p2[2]-p2[3])
T3=sample(c(s2,x[i],b2-x[i]),1,prob=p3)
Z3=sample(c(s1,y[i],b1-y[i]),1,prob=q3)
x.hat3[i]=(T3-q3[1]*mu.s2-q3[3]*b2)/(q3[2]-q3[3])
y.hat3[i]=(Z3-p3[1]*mu.s1-p3[3]*b1)/(p3[2]-p3[3])
T4=sample(c(s2,x[i],b2-x[i]),1,prob=p4)
Z4=sample(c(s1,y[i],b1-y[i]),1,prob=q4)
x.hat4[i]=(T4-q4[1]*mu.s2-q4[3]*b2)/(q4[2]-q4[3])
y.hat4[i]=(Z4-p4[1]*mu.s1-p4[3]*b1)/(p4[2]-p4[3])

}
cor.hat1[j]=cor(x.hat1,y.hat1)
cor.hat2[j]=cor(x.hat2,y.hat2)
cor.hat3[j]=cor(x.hat3,y.hat3)
cor.hat4[j]=cor(x.hat4,y.hat4)
cor1[j]=cor(x,y)
}
cbind(mean(cor1),mean(cor.hat1),mean(cor.hat2),mean(cor.hat3),mean(cor.hat4))
}
f(100,20,0.1)
################################## simulation ############
iter=10
n=c(20,100,500,1000)
rho=0.1
tab2=matrix(0,length(n),5)
set.seed(1)
for(i in 1:length(n))
{
tab2[i,1]=f(iter,n[i],rho)[,1]
tab2[i,2]=f(iter,n[i],rho)[,2]
tab2[i,3]=f(iter,n[i],rho)[,3]
tab2[i,4]=f(iter,n[i],rho)[,4]
tab2[i,5]=f(iter,n[i],rho)[,5]

print(i)
}
rownames(tab2)=paste("n=",n)
colnames(tab2)=paste(c("cor","cor.hat1","cor.hat2","cor.hat3","cor.hat4"))
tab2