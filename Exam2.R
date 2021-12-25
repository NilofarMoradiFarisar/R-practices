


   data.exam   <- read.csv(file.choose(), sep=",")

   data.exam

   head(data.exam)

  attach(data.exam)



boxplot(   data.exam[,"Seeds"] ~    data.exam[,"SlopeOrientation"], range=0)
boxplot(   Seeds ~ SlopeOrientation, data=data.exam , range=0)


slopeOrientation =c("S", "N","E","W")
par(mfrow=c(2,2))
for(i in 1:length(slopeOrientation )) ### Loop: set i to 1 and execute commands in curled brackets; then: set i to 2 and execute commands in curled brackets again; and so on until i=length(treatments)=4
  ## repeat until i = length(slopeOrientation ), i.e. 4 in this case.
{
  qqnorm(  data.exam$Seeds[data.exam$SlopeOrientation == slopeOrientation[i]])
  qqline( data.exam$Seeds[data.exam$SlopeOrientation == slopeOrientation[i]])
}


######### Homogeneity of variances? Bartlett-Test ###################
bartlett.test(data.exam$Seeds ~ data.exam$SlopeOrientation )


######### Does data transformation improve the situation? ################
bartlett.test(sqrt(data.exam$Seeds) ~ data.exam$SlopeOrientation)





########### ANOVA (for untransformed and transformed data) #################

modell1.aov <- aov(Seeds ~ SlopeOrientation, data = data.exam)						   # untransformed
modell1sqrt.aov <- aov(sqrt(Seeds) ~ SlopeOrientation, data = data.exam)		                     # transformed

########### Print results ###################

summary(modell1.aov)			# Summary of main results: Df - degrees of freedom, SumSq - sum of squares between factor levels, MS - SumSq/Df, F - F-value
summary(modell1sqrt.aov)                 # (= MS Among / MS Within) and probability to get such an F-value if the null hypothesis holds
# Sum of squares within and appropriate mean squares are given under "Residuals"

############ Multiple comparisons #################

TukeyHSD(modell1.aov)				# Multiple comparisons among treatment levels: which of them do differ, which do not? 
TukeyHSD(modell1sqrt.aov)                # diff - difference of means; "lwr" and "upr" - upper and lower 95%-confidence bounds for this difference; 
# "p adj" - adapted probability of null hypothesis for each comparison.  

plot(TukeyHSD(modell1.aov))
plot(TukeyHSD(modell1sqrt.aov))



sd=sd(Seeds)

power.t.test(delta=3.4 , sd=11.57  , power = 0.8 , type="two.sample", alternative="two.sided")





 