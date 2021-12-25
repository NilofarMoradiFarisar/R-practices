
######################################################
############ Regression 1, Stemflow ##################
################### 2016-23-11 #######################

##### setting the working directory
#setwd("D:/statistik_lv/VO6")


##### reading data and transforming into a data.frame ####
###file.choose() allows interactive navigation to the file
#UE1 <- data.frame(read.csv("UE4_Regression1.csv"))

##### check if everything worked, looking at the head (first 6 lines) of the data
UE1=read.table("reg.txt")
head(UE1)

##### the assumptions for the linear model are that the residuals are normally distributed and
 #the variance is homogenouse
##### the data does NOT have to be normally distributed
##### first, plot the data, and think what relationship could exist between prec (dbh) and stemflow
pairs(UE1[,3:5]) #visualises pairwise relationships of the variables


##################################################################################################
##### conclusion: dbh and stemflow should relate linearily, prec and stemflow could be quadratic
##################################################################################################



stemflow=UE1$V3
dbh=UE1$V5
prec=UE1$V4

#### univariate lineare regression
lm.1 <- lm(stemflow~dbh,data=UE1)
summary (lm.1)
par(mfrow = c(2,2))
plot(lm.1)

###################################################################################################
##### residuals look ~ok, (residuals versus fitted: no clear pattern, qqplot: could be better)
##### lm: dbh gives a positive relationship (estimate positive), and is highly significant
##### model explains 44.27% of the variance
##### model is significantly (p=0.0003882) different from Null model (i.e., predictors have no influence)
####################################################################################################

##### lets try to transforme stemflow to sqrt(stemflow, using x11() opens a new plotting window, to better compaire the results
#### univariate lineare regression
lm.2 <- lm(sqrt(stemflow)~dbh,data=UE1)
summary (lm.2)
x11()
par(mfrow = c(2,2))
plot(lm.2)

###################################################################################################
##### residuals look better, (qqplot is better)
##### lm: dbh gives a positive relationship (estimate positive), and is highly significant
##### model explains 40.92% of the variance
##### model is significantly (p=0.000763) different from Null model (i.e., predictors have no influence)
##### as the assumptions are better met, we should use this model. 
##### the significance values are a bit worse, also R², but more reliable, becaus of the assumptions!!!!
#####################################################################################################

### multiple lineare regression
lm.3 <- lm(sqrt(stemflow)~dbh + prec,data=UE1)
summary (lm.3)
x11()
par(mfrow = c(2,2))
plot(lm.3)

###################################################################################################
#### even better, significance of prec ok (still below 0.05)
#### especially R² is better!!
###################################################################################################


### Polynomial transformation of prec
lm.4 <- lm(sqrt(stemflow)~dbh + poly(prec,2),data=UE1)
summary (lm.4)
x11() 
par(mfrow = c(2,2))
plot(lm.4)




##################################
lm.5 <- lm(sqrt(stemflow)~ prec,data=UE1)
summary (lm.5)

###################################################################################################
#### summary shows: quadratic prec term (poly(prec,2)2), is not significant, hence, dont use it :)
###################################################################################################


### simplifying the model (parsimony)
drop1(lm.3, test="F") # removing prec from the model makes no sense
drop1(lm.5, test="F") # removing prec from the model makes no sense
drop1(lm.2, test="F") # removing prec from the model makes no sense

### or use step()
step(lm.3,direction="both")

#####################################################################################################
#### complete model is the best, with an AIC of 97.653
#####################################################################################################

### Predicting stemflow for all trees on the 1 ha area
# reading in the data holding all trees
all.trees <- data.frame(read.csv("UE4_Regression1_dbh.csv"))

head(all.trees)

#### before predicting, check if new predictors are outside of the range of the parameterising values

summary(UE1$dbh)
summary(all.trees$dbh)

summary(UE1$prec)
summary(all.trees$prec)

##########################################################################################################
#### maximum should not be such a problem (they are quite close, and for bigger models the model should work ok),
#### but small values are a problem
#### prec values are all the same (min=max!!!!!), because data is from one year and one area
##########################################################################################################


#### getting rid of trees with a dbh smaller than in the parameterising data

j= read.table("UE4_Regression1_dbh (1).txt")
j=j[2:25,]
write.table(j,"all.trees.txt")
all.trees <- read.table("all.trees.txt")

trees=all.trees$V1
dbh=all.trees$V2
prec=all.trees$V3

all.trees <- all.trees[all.trees$V2 >= min(UE1$V3),]



# lm.3 is the most parsimonious model, therefore we use this one
lm.3.predict <- predict.lm(lm.3, all.trees, interval="confidence",level=0.95)
sum(lm.3.predict[,1]^2) #the sum of the stemflow of all trees;
                        #don't forget to back-transform sqrt(stemflow)!



######### sorting the plotting data, in order of dbh
######### order tells you in which order the elements of a vector would be, if it would be sorted
######### x <- c(10,70,20,80,50,60,0)
######### order(x)
######### using:   [,] to rearange the dataset


plotting_data  <- data.frame(all.trees$dbh,lm.3.predict[,1]^2)
plotting_data1 <- plotting_data[order(plotting_data$all.trees.dbh),] 

plotting_data  <- data.frame(all.trees$dbh,lm.3.predict[,2]^2)
plotting_data2 <- plotting_data[order(plotting_data$all.trees.dbh),] 

plotting_data  <- data.frame(all.trees$dbh,lm.3.predict[,3]^2)
plotting_data3 <- plotting_data[order(plotting_data$all.trees.dbh),] 


# Visualisation of the regression model with 95% confidence intervals
par(mfrow=c(1,1))
r.sq <- summary(lm.3)$r.squared # extracts the R² of the model
plot(plotting_data1,type ="l",xlab="tree dbh [cm]",
     ylab="stemflow [l/ha/year]",main="Least Square Regression with 0.95 confidence interval",
     sub=paste("Annual precipitation = 1500 mm, R²= ",round(r.sq,digits=3)))
lines(plotting_data2,type ="l",lty=2)
lines(plotting_data3,type ="l",lty=2)
#### and adding the original points
points(UE1$dbh,UE1$stemflow, col="red")


###################################################################################################
###### we dont need to plot against prec, because they are all the same !!!
###################################################################################################
