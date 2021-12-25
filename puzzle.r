

dat <- read.csv("D:/statistik_lv/jw/r_intro/data1.csv")
dim(dat)

names(dat)

pres <- sum(dat$Dryas.octopetala,na.rm=T)

abs  <- sum(dat$Dryas.octopetala==0,na.rm=T)

prop <- pres/(abs+pres)

prop

temper <- dat$Temp
 meantemp <- mean(temper[dat$Dryas.octopetala==1],na.rm=T)


#########
#
# End of R introduction
#
#########

dat <- c()

for(i in c(0:9))
{
	lambda <- 2^i
	rand   <- rpois(50,lambda)
	dat    <- cbind(dat,rand) 
}

