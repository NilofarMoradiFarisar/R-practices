######## Regression 3, plant species niche model with GAM ##########
################### 2015-12-08 #####################################

#install gam package and load library
install.packages("gam")
library(gam)

##### reading data and attaching the data frame ####
data.gam<-read.csv(file.choose())
attach(data.gam)

##### visualising correlations of predictors ####

pairs(cbind(SD,TCC,CEC))

##### calculating pearson correlation coef

cor(data.gam[,4:6])

#only SD and CEC are correlated, but still below our threshold of 0.6
#we use all predictor variables

### full GAM model with a smoothing spline, complexity 4 #####
gam.card.trif<- gam(card.trif~ s(SD,4) + s(TCC,4)
                   + s(CEC,4), family = binomial, data = data.gam)

summary (gam.card.trif) # quality of the model
# for CEC neither the non parametric nor the parametric effects are signifficant
d.squared <- (gam.card.trif$null.deviance - gam.card.trif$deviance)/gam.card.trif$null.deviance
d.squared  # 44.14 % of the variance are explained

anova (gam.card.trif, test = "Chisq")

par(mfrow=c(2,2))
plot.gam(gam.card.trif,se=T) #the form of the relationships

## in SD the relation looks quadratic, we therefore also try to fit a quadratic term
## in the following we use the step function to produce the simplest model
## for all predictors I choose to (here for example for SD)
## either leaf it out(1), 
## a linear relation ship (SD), 
## a quadratic relationship (poly(SD,2)), 
## or a fitted smooth term (s(SD,4))
################################################################

gam.object <- gam(card.trif~SD+TCC+CEC,data=data.gam,family=binomial)
step.object <-step.gam(gam.object, scope=list("SD"=~1+SD+poly(SD,2)+s(SD,4),"TCC"=~1+TCC+poly(TCC,2)+s(TCC,4),"CEC"=~1+CEC+poly(CEC,2)+s(CEC,4)))


## the best model (lowest AIC) only has a quadratic term of SD 
## hence, we should build a glm

################################################################
#best fit a glm
glm.object <- glm(card.trif~poly(SD,2),data=data.gam,family=binomial)
summary(glm.object)


####################################################################
# plotting the response function
detach(data.gam)   #we skip the search path to the data frame data.gam
# because the names of the variables are the same as those in data.new
SD <- seq(1,100,1)  #a sequence of SC values for predictions

data.new<-data.frame(SD)
pred.glm<-predict.glm(glm.object,newdata=data.new,type="response")

## pred.gam also works with the object resulting from the step procedure, you can try: 
#pred.gam<-predict.gam(step.object,newdata=data.new,type="response")


par(mfrow=c(1,1))
plot(data.new$SD,pred.glm,type="l",
     xlab="soil depth [cm]",
     ylab="ocurrence prob",
     ylim=c(0,1), 
     main="Cardamine trifolia (mean canopy closure)")



#####################
## a little ad on, see how the built model nicely fits the smoothed function 
par(mfrow=c(2,2))
plot.gam(gam.card.trif)
plot(data.new$SD,pred.glm,type="l",
     xlab="soil depth [cm]",
     ylab="ocurrence prob",
     ylim=c(0,1), 
     main="Cardamine trifolia (mean canopy closure)")





######################################################################
######################################################################
######################################################################
################  DIFFERENT WAY	 #################################

## you could have also done the stepwise reduction by hand, by deleting the predictors that are not signifficant
## here is an example:

gam.card.trif.red1<- gam(card.trif~ s(SD,4) + s(TCC,4)
                   + CEC, family = binomial, data = data.gam)
# may be the straight-line relationship of CEC is not significant?
# to test this we fit another model without CEC and apply anova
gam.card.trif.red2<- gam(card.trif~ s(SD,4) + s(TCC,4)
                   , family = binomial, data = data.gam)
anova (gam.card.trif.red1,gam.card.trif.red2, test = "Chisq")
#CEC does not significantly improve the model, we therefore
#proceed without it

##################################################################
# You may recognise that in this model the non-parametric relationship
# of TCC becomes non-significant. Therefore we may also proceed like...
gam.card.trif.red3<- gam(card.trif~ s(SD,4) + TCC
                   , family = binomial, data = data.gam)
gam.card.trif.red4<- gam(card.trif~ s(SD,4)
                   , family = binomial, data = data.gam)
anova (gam.card.trif.red3,gam.card.trif.red4, test = "Chisq")
anova(gam.card.trif.red4, test = "Chisq")
#... and only SD remains in the model

