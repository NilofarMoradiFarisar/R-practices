HistoricalRearEdge=datentab$HistoricalRearEdge

####################  mixed model #################

bartlett.test(datentab$RearEdgeShift~ datentab$mycorrhiza_status)
bartlett.test(sqrt(abs(datentab$RearEdgeShift))~datentab$mycorrhiza_stat)

############################################

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