#============================================================#
# VO: Analysis of ecological data - univariate methods       #
# Lecture: Phylogenetic signal in comparative analysis       #
# Bernd Lenzner                                              #
# 18.01.2017                                                 #
# Sample solution                                            #
#============================================================#


# Loading the relevant packages for analysis ----
### use install.packages() if the packages are not installed on the computer yet
library(ape)
library(caper)

# Load test datasets for analysis
### The comparative dataset is derived from Lislevand & Thomas (2006) Limited male incubation ability and the evolution of egg size in shorebirds. Biology Letters, 2(2):206-208.
### Data abbreviations are:
##### Genus: Genus name the respective species belongs to
##### Species: Species name
##### M.Mass: Mean body mass of the males of the respective species
##### F.Mass: Mean body mass of the females of the respective species
##### Egg.Mass: Mean egg weight for the respective species
##### Cl.size: Mean number of eggs per nest for the respective species
##### Mat.syst: Mating system of the respective species

# Set working directory that corresponds to the folder on your computer where the datasets are stored
setwd("")
# Read the dataset containing the above variables and store it as data frame object; check that the relevant seperator for the data is used (sep"")
birds <- read.csv("bird_data.csv",sep=";" , header=T, stringsAsFactors = F)
# Read the phylogeny that corresponds to the dataset and store it as a tree object
birds.tree <- read.tree("bird_tree.new")



# Check data
### Look at the first 6 rows of your data to see if all variables are loaded correctly and if the data frame has the correct headers for each column.
head(birds)

### As we want to look at the relationship between Clutch size (Cl.size) and female bodymass (F.Mass) we should check if our response variable is more or less normally distributed and if not we should use an appropriate transformation to achieve that. Assessment can be done easily by plotting relevant variable as a histogram and check the distribution visually
par(mfrow=c(1,2)) # this command changes the default plot window so that you can display 2 plots side by side (1 sets the number of rows; 2 the number of columns)
hist(birds$Cl.size,breaks=20,main="Histrogram for mean clutch size", xlab="Female bird body mass")
hist(log(birds$Cl.size),breaks=20,main="Histrogram for mean clutch size - log transformed", xlab="Female bird body mass (log)")
###### the parameter "breaks" specifies the number bars used in the histogram which sometimes improves the visual assessment of the data
###### as the transformation does not make the data more normally distributed we will use the data untransformed (You can as well check other transformations like square root or power transformation)


# Check tree
### To get an idea of how the phylogenetic tree looks like you check the tree element itslef and subsequently plot the tree 
par(mfrow=c(1,1)) # see explanation above
birds.tree # provides information on the number of tips and nodes of the tree; lists the first tip labels and gives you information if the tree is rooted and has branch lengths attatched
plot(birds.tree, cex=0.6) # the parameter "cex" adjusts the size of the tip labels
is.ultrametric(birds.tree) # test if the tree is ultrametric or not (ultrametric = same distance from the root to the tip for all species) --> always check this, because R displays uninformative trees as ultrametric when you plot them!!!



# Build OLS regression model for female bodymass ~ clutch size
mod.ols <- lm(Cl.size~log(F.Mass), birds) # female body size is log transformed, because then the model residuals fit (a little) better

# check model assumptions
par(mfrow=c(2,2))
plot(mod.ols) #you can as well test the model assumptions for models with different variable transformations

# investigate the model output (significance level, intercept and slope estimates)
summary(mod.ols)




# Create comparative data set for PGLS regression

birds.comp <- comparative.data(birds.tree, birds, Species, vcv=TRUE) # the parameter "VCV=TRUE" specifies that R safes the calculated variance-covariane matrix in the data object

# Build PGLS regression model for female bodymass ~ clutch size (use same transformations as above)

mod.pgls <- pgls(Cl.size~log(F.Mass), birds.comp)

#check model assumptions
par(mfrow=c(2,2))
plot(mod.pgls)
#investigate model output
summary(mod.pgls)

# Build PGLS regression model for female bodymass ~ clutch size where lambde is estimated via maximum likelihood

mod.pgls.ML <- pgls(Cl.size~log(F.Mass), birds.comp, lambda = "ML")
#check model assumptions
plot(mod.pgls.ML)
#investigate model output
summary(mod.pgls.ML)




# Plot data and regression lines for the different models including a legend

par(mfrow=c(1,1))
plot(Cl.size~log(F.Mass), birds,pch=21,bg="grey", xlab="Female body mass (log)", ylab = "Clutch size")

# add the regression lines for all three models that were constructed earlier and give each regression line a different color
abline(mod.ols, col="black")
abline(mod.pgls,col="blue")
abline(mod.pgls,col="red",lty=2, lwd=2)

# add a legend to the plot that gives information about the regression lines
legend("bottomleft",c("OLS", "PGLS", "PGLS.ML"), col=c("black","blue","red"), lty=c(1,2,1,2))


##### It is possible to plot individual points in different colors based on subsets of the actual data frame
##### for that you need to subset your data based on a condition (here a specific genus) --> this is done using the term in squared brackets after the dataset
##### the "points" function draws points into an already existing plot

##### Additionally we can plot the genus names next to the respective points using the text function
##### the "labels" parameter distinguishes what is written. if the number of terms in the labels parameter does not match the numbers of point we draw ther element will be replicated until the numbers are equal

points(Cl.size~log(F.Mass), birds[birds$Genus == "Haematopus",], col="red", pch=19)
text(Cl.size~log(F.Mass), birds[birds$Genus == "Haematopus",], labels = "Haematopus", cex=0.6, pos = 2)
points(Cl.size~log(F.Mass), birds[grep("Calidris",birds$Species),], col="blue", pch=19)
text(Cl.size~log(F.Mass), birds[birds$Genus == "Calidris",], labels = "Calidris", cex=0.6, pos = 2)
points(Cl.size~log(F.Mass), birds[grep("Tringa",birds$Species),], col="orange", pch=19)
text(Cl.size~log(F.Mass), birds[birds$Genus == "Tringa",], labels = "Tringa", cex=0.6, pos = 4)


