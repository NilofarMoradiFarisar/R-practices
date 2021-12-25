
par(mfrow=c(1,1))

####################  4_1 ########################

	a=read.table("s.txt")

CH1=a$V2
CH2=a$V3


#		plot(CH1,CH2)



xc <- 1300 # center x_c or h
yc <- 1350 # y_c or k
a <- 2300 # major axis length
b <-  370 # minor axis length
phi <- pi/4 # angle of major axis with x axis phi or tau

t <- seq(0, 2*pi, 0.01) 
x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
	plot(x,y,pch=19, col='blue',xlim=c(-600,3200),ylim=c(-600,3200),
			xlab="CH1",ylab="CH2",main="figure of 4_1 ")
	lines(CH1,CH2,type="p")

#################### 200 us ###############################

	a1=read.table("200.txt")

CH1=a1$V2
CH2=a1$V3


#		plot(CH1,CH2)



xc <- 1220 # center x_c or h
yc <- 1420 # y_c or k
a <- 2300 # major axis length
b <-  380 # minor axis length
phi <- pi/4 # angle of major axis with x axis phi or tau

t <- seq(0, 2*pi, 0.01) 
x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
	plot(x,y,pch=19, col='blue',xlim=c(-600,3200),ylim=c(-600,3200),
			xlab="CH1",ylab="CH2",main="figure of 200us ")
	lines(CH1,CH2,type="p")




#################### 400 us ###############################

	a2=read.table("400.txt")

CH1=a2$V2
CH2=a2$V3


#		plot(CH1,CH2)



xc <- 1300 # center x_c or h
yc <- 1400 # y_c or k
a <- 2300 # major axis length
b <-  350 # minor axis length
phi <- pi/4 # angle of major axis with x axis phi or tau

t <- seq(0, 2*pi, 0.01) 
x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
	plot(x,y,pch=19, col='blue',xlim=c(-600,3200),ylim=c(-600,3200),
			xlab="CH1",ylab="CH2",main="figure of 400us ")
	lines(CH1,CH2,type="p")


#################### 800 us ###############################

	a3=read.table("800.txt")

CH1=a3$V2
CH2=a3$V3


#		plot(CH1,CH2)



xc <- 1300 # center x_c or h
yc <- 1370 # y_c or k
a <- 2270 # major axis length
b <-  170 # minor axis length
phi <- pi/4 # angle of major axis with x axis phi or tau

t <- seq(0, 2*pi, 0.01) 
x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
	plot(x,y,pch=19, col='blue',xlim=c(-600,3200),ylim=c(-600,3200),
			xlab="CH1",ylab="CH2",main="figure of 800us ")
	lines(CH1,CH2,type="p")

#################### 1400 us ###############################

	a4=read.table("1400.txt")

CH1=a4$V2
CH2=a4$V3


#		plot(CH1,CH2)



xc <- 1310 # center x_c or h
yc <- 1390 # y_c or k
a <- 2300 # major axis length
b <-  170 # minor axis length
phi <- pi/4 # angle of major axis with x axis phi or tau

t <- seq(0, 2*pi, 0.01) 
x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
	plot(x,y,pch=19, col='blue',xlim=c(-600,3200),ylim=c(-600,3200),
			xlab="CH1",ylab="CH2",main="figure of 1400us ")
	lines(CH1,CH2,type="p")


#################### 1600 us ###############################

	a5=read.table("1600.txt")

CH1=a5$V2
CH2=a5$V3


#		plot(CH1,CH2)



xc <- 400 # center x_c or h
yc <- 650 # y_c or k
a <- 990 # major axis length
b <-  50 # minor axis length
phi <- pi/4 # angle of major axis with x axis phi or tau

t <- seq(0, 2*pi, 0.01) 
x <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- yc + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)
	plot(x,y,pch=19, col='blue',xlim=c(-500,1700),ylim=c(-500,1700),
			xlab="CH1",ylab="CH2",main="figure of 1600us ")
	lines(CH1,CH2,type="p")





