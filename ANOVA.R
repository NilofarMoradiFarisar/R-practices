################ Visualise data by boxplots #########################

###   DatenUE5   <- read.csv(file.choose(), sep=",")

DatenUE5=read.csv(file.choose("DatenANOVA(2).txt"),sep=",")




boxplot(DatenUE5[DatenUE5$Treatment == "A","Seeds"],DatenUE5[DatenUE5$Treatment == "B","Seeds"],
DatenUE5[DatenUE5$Treatment == "C","Seeds"],
        DatenUE5[DatenUE5$Treatment == "D","Seeds"],range=0)
#or
boxplot(DatenUE5[,"Seeds"] ~ DatenUE5[,"Treatment"], range=0)
#or:
boxplot(Seeds~Treatment, data=DatenUE5, range=0)

######### Normal distribution? Check with qq-plots ###################
treatments <- c("A","B","C","D")
par(mfrow=c(2,2))
for(i in 1:length(treatments)) ### Loop: set i to 1 and execute commands in curled brackets; then: set i to 2 and execute commands in curled brackets again; and so on until i=length(treatments)=4
  ## repeat until i = length(treatments), i.e. 4 in this case.
{
  qqnorm(DatenUE5$Seeds[DatenUE5$Treatment == treatments[i]])
  qqline(DatenUE5$Seeds[DatenUE5$Treatment == treatments[i]])
}







######### Homogeneity of variances? Bartlett-Test ###################
bartlett.test(DatenUE5$Seeds ~ DatenUE5$Treatment)


######### Does data transformation improve the situation? ################
bartlett.test(sqrt(DatenUE5$Seeds) ~ DatenUE5$Treatment)









########### ANOVA (for untransformed and transformed data) #################

modell1.aov <- aov(Seeds ~ Treatment, data = DatenUE5)						   # untransformed
modell1sqrt.aov <- aov(sqrt(Seeds) ~ Treatment, data = DatenUE5)		                     # transformed

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


########## ANOVA f???r Aufgabe 2 mit transformierten Daten ###################
modell2sqrt.aov <- aov(sqrt(Seeds) ~ Treatment*Exposition, data = DatenUE5)           # "*" tells R to account for the interaction of the two predictor variables

########### Print results ###################

summary(modell2sqrt.aov)		    # Summary of main results: see above	

############ Multiple comparisons #################
TukeyHSD(modell2sqrt.aov)
plot(TukeyHSD(modell2sqrt.aov))
# of special interest are AN - AS, BN - BS, CN - CS and DN - DS. although there is a marginally significant intearction there is, however, no indication that exposition has any effect in these pairwise comparisons.
###############################################################################################################################################################################################

