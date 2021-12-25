############ Regression 2, GLM ###############
################### 2016-12-07 #######################

#### Read in the data #######

#daten.glm <- read.csv("D:/dull/lehre/StatistikVorlesung/2016_17/VO7/DatenUE7GLM.csv", sep=",", dec=".")
#attach(daten.glm)


daten.glm=read.csv(file.choose(), sep=",")
daten.glm
attach(daten.glm)



##### Visualize the data 

par(mfrow=c(1,2))
boxplot(CEC[cycl.purp==0],CEC[cycl.purp==1],names=c("absence","presence"), ylab="CEC")
# Differences in CEC among plots with and without Cylcamen
plot(CEC,cycl.purp)                         # Distribution of presences/absences along CEC-gradient

####################################################################################
##### Does CEC affect occurrence of C. purpurascencs and if yes, is the correlation linear or hump-shaped (within the study area) 
## Compare linear and polynomial model

glm.lin  <- glm(cycl.purp ~ CEC, family = binomial, data = daten.glm)
summary(glm.lin)
anova(glm.lin, test="Chi")

d.squared.glm.lin <- (glm.lin$null.deviance - glm.lin$deviance)/glm.lin$null.deviance

### drop1(glm.lin) ## Bewertung der relativen Bedeutung von Variablen in einem multiplen Regressionsmodell


## now the hump-shaped model
glm.unimod <- glm(cycl.purp ~ poly(CEC,2), family = binomial, data = daten.glm)
summary(glm.unimod)
anova(glm.unimod, test="Chi")

d.squared.glm.unimod <- (glm.unimod$null.deviance - glm.unimod$deviance)/glm.unimod$null.deviance

## use of anova() to test difference among models
## the appropriate test-statistic is chi-square
anova(glm.lin, glm.unimod, test = "Chi")
## same test computed "manually"
1-pchisq((glm.lin$deviance-glm.unimod$deviance),df=1)

##### Graphical representation of distribution of C. purpurascens along the CEC gradient
##### first create sequence of CEC-values:
CEC.new <- data.frame(CEC=seq(0,1200,10))
CEC.pred <- predict.glm (glm.lin, newdata = CEC.new, type = "response")
##### then plot sequence against model predictions
par(mgp=c(5,2,0))
par(mar=c(8,8,4,4))
plot (CEC.new[,1],CEC.pred, xlab = "Cation exchange capacity", ylab = "Probability of occurrence", type="l", cex.axis=2, cex.lab=2)
# add real data points
points(CEC,cycl.purp, pch=19)


