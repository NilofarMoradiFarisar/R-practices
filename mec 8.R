library(nlme)
setwd
datentab <- read.csv(file.choose("data2.csv"))
head(datentab)
Familie=datentab$Familie
HistoricalRearEdge=datentab$HistoricalRearEdge
#################  RearedgeShift  ###########
boxplot(datentab[datentab$mycorrhiza_status=="a","RearEdgeShift"],datentab[datentab$mycorrhiza_status == "o","RearEdgeShift"],
        datentab[datentab$mycorrhiza_status == "f","RearEdgeShift"],range=0)

boxplot(RearEdgeShift~mycorrhiza_status, data=datentab,xlab="Mycorrhizal-status",
        ylab= "RearEdgeShift (m)",col=c(6,7,5) ,range=0)
abline(0,0)

mycorrhiza_status<-c("a","o","f")

##################### OptimumShift  ##########

boxplot(datentab[datentab$mycorrhiza_status=="a","OptimumShift"],datentab[datentab$mycorrhiza_status == "o","OptimumShift"],
        datentab[datentab$mycorrhiza_status == "f","OptimumShift"],range=0)

boxplot(OptimumShift~mycorrhiza_status, data=datentab,xlab="Mycorrhizal-status",
        ylab= "OptimumShift(m)",col=c(6,7,5), range=0)
abline(0,0)
########################### LeadingEdgeShift


boxplot(LeadingEdgeShift~mycorrhiza_status, data=datentab,xlab="Mycorrhizal-status",
        ylab= "LeadingEdgeShift (m)",col=c(6,7,5), range=0)
abline(0,0)
##########################  HistoricalRearEdge
boxplot(HistoricalRearEdge~mycorrhiza_status, data=datentab,xlab="Mycorrhizal-status",
        ylab= "HistoricalRearEdge (m)",col=c(6,7,5),ylim=c(1000,3000), range=0)

###############  HistoricalOptimum

boxplot(HistoricalOptimum~mycorrhiza_status, data=datentab,xlab="Mycorrhizal-status",
        ylab= "HistoricalOptimum (m)",col=c(6,7,5),ylim=c(1000,3000), range=0)


############################### HistoricalLeadingEdge
boxplot(HistoricalLeadingEdge~mycorrhiza_status, data=datentab,xlab="Mycorrhizal-status",
        ylab= "HistoricalLeadingEdge (m)",col=c(6,7,5), range=0)

#######################  Check ANOVA requirements: normal distribution, homogeneity of variances
par(mfrow=c(2,2))
for(i in 1:length("mycorrhiza_status"))
{
  qqnorm(datentab$RearEdgeShift[datentab$mycorrhiza_status == mycorrhiza_status[i]])
  qqline(datentab$RearEdgeShift[datentab$mycorrhiza_status == mycorrhiza_status[i]])
}
head(datentab)
bartlett.test(datentab$HistoricalRearEdge ~ datentab$mycorrhiza_status)
bartlett.test(sqrt(datentab$HistoricalRearEdge)~datentab$mycorrhiza_status)

HistoricalRearEdge.aov <- aov(HistoricalRearEdge ~ mycorrhiza_status, data = datentab)
HistoricalRearEdgesqrt.aov <- aov(sqrt(HistoricalRearEdge) ~ mycorrhiza_status, data = datentab)
HistoricalRearEdgesqrt.aov
summary(HistoricalRearEdge.aov)
summary(HistoricalRearEdge.aov) 

TukeyHSD(HistoricalRearEdge.aov)
TukeyHSD(HistoricalRearEdgesqrt.aov)
plot(TukeyHSD(HistoricalRearEdge.aov))
plot(TukeyHSD(HistoricalRearEdgesqrt.aov))
HistoricalRearEdgesqrt.aov <- aov(sqrt(HistoricalRearEdge) ~ mycorrhiza_status*mycorrhiza_type, data = datentab)
summary(HistoricalRearEdgesqrt.aov)
TukeyHSD(HistoricalRearEdgesqrt.aov)
plot(TukeyHSD(HistoricalRearEdgesqrt.aov))

###########################################################################################
RearEdgeShift=datentab$RearEdgeShift
mycorrhiza_status=datentab$mycorrhiza_status

bartlett.test(datentab$RearEdgeShift~ datentab$mycorrhiza_status)
bartlett.test(sqrt(datentab$RearEdgeShift)~datentab$mycorrhiza_stat)

geb <- levels(datentab$mycorrhiza_status)
geb

mycorrhiza_status=factor(datentab$mycorrhiza_status)
geb =mycorrhiza_status
RearEdgeShift=datentab$RearEdgeShift
RearEdgeShift
HistoricalRearEdge=datentab$HistoricalRearEdge
HistoricalRearEdge
#################################################################################
x11()
par(mfrow=c(1,3))
for(i in 1:length(geb))
{
  plot(datentab$RearEdgeShift[datentab$mycorrhiza_status==geb[i]], 
       datentab$HistoricalRearEdge[datentab$mycorrhiza_status==geb[i]], 
       xlab="RearEdgeShift(m)", ylab="HistoricalRearEdge(m)",
       main=geb[i],ylim = c(1000,3000),xlim = c(-600,600))
  abline(lsfit(datentab$RearEdgeShift[datentab$mycorrhiza_status==geb[i]], 
               datentab$HistoricalRearEdge[datentab$mycorrhiza_status==geb[i]]))
}
##################################################################################  mixed model
##################
mycorrhiza_status22=factor(datentab$mycorrhiza_status)
Familie=datentab$Familie

RearEdgeShift1=(RearEdgeShift+abs(min(RearEdgeShift))^(1/3))
RearEdgeShift2=sqrt(abs(RearEdgeShift))

lme.RearEdge =lmer(RearEdgeShift2~I(mycorrhiza_status22)+(HistoricalRearEdge|mycorrhiza_status22)+(1|Familie)-1, data=datentab)
summary(lme.RearEdge)
##################################


 HistoricalOptimum= datentab$HistoricalOptimum
OptimumShift=datentab$OptimumShift


lme5<- lmer(OptimumShift~I(mycorrhiza_status22)+(HistoricalOptimum|mycorrhiza_status22)+(1|Familie)-1, data=datentab)
summary(lme5)
###########################################


anova(lme.RearEdge.Familie, lme.RearEdge)

AIC(CM.TrcGebBerg.lme)
AIC(CM.TrcGeb.lme)

x11()
plot(CM.TrcGebBerg.lme, Trc~resid(.,type="p"),ylab="Region")
CM.TrcGebBergVarHet.lme <- lme(CScoreFE ~ coldestMonthMean, random=~ coldestMonthMean|Trc/Suc, weights=varIdent(form=~1|Trc), control=list(maxIter=100, msMaxIter=100), data=datentab, method="REML")
summary(CM.TrcGebBergVarHet.lme)

anova(CM.TrcGebBerg.lme, CM.TrcGebBergVarHet.lme)
AIC(CM.TrcGebBerg.lme)
AIC(CM.TrcGebBergVarHet.lme)
