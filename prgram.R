

	d=read.table("book40.txt")


y=d$V4
jensisat=d$V5
sen=d$V6
tasadof=d$V7
estefade=d$V8
tedadkhodro=d$V9
sorat=d$V10
piadero=d$V11
jazire=d$V12
tedadekhat=d$V13
mabar=d$V2




jensisat=factor(jensisat)
tasadof=factor(tasadof)
piadero=factor(piadero)
jazire=factor(jazire)




######## packages #####


library(lme4)
library(nlme)
library(zoo)
library(lmtest)


############### A?C?? I??E?? ?CE??? ##########

dwtest(y~sen+jensisat+piadero+tedadekhat+jazire+tasadof+tedadkhodro+sorat+estefade)



########   ?I? ????? ?C??C??   ########

mod1 <- glmer(y ~ 1+( 1|mabar), data=d)
summary(mod1)



################################################################################################
######### ?I? ??? C? ?EIC E?CI?? EC ???? ?EU???C? ??E??  #########


mod2 <- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat+( 1 | mabar), data=d)
summary(mod2)




################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? ????E ############

mod3 <- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(jensisat|mabar), data=d)
summary(mod3)





################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? ?? ############

mod4 <- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(sen|mabar), data=d)
summary(mod4)

################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? E?ICI I?I?? ############

mod5 <- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(tedadkhodro|mabar), data=d)
summary(mod5)




################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? ???E I?I?? ############

mod6 <- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(sorat|mabar), data=d)
summary(mod6)

################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? ??CI? ?? ############

mod7<- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(piadero|mabar), data=d)
summary(mod7)



################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? ????? ############

mod8 <- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(jazire|mabar), data=d)
summary(mod8)

################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? E?CI? ############

mod9<- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(tasadof|mabar), data=d)
summary(mod9)


################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? I? I?I?? ############

mod10 <- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(tedadekhat|mabar), data=d)
summary(mod10)

################################################################################################
################# ??? C? ?EIC E?CI?? ? O?E E?CI?? E?C? ?EU?? C?E?CI? ############

mod11<- glmer(y~ sen+I(jensisat)+I(piadero)+tedadekhat+I(jazire)+estefade+
I(tasadof)+tedadkhodro+sorat +( 1 | mabar)+(estefade|mabar), data=d)

summary(mod11)

##################################################################################





mod13<- glmer(y~ sen+I(jensisat)+( 1 | mabar)+(jensisat|mabar), data=d)
summary(mod13)

######### ??C??? ?I? ?C ###########

anova(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11)





