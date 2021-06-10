# Data contains 13 different chemicals in wines grown in the same region in Italy that derived 
# from three different cultivars, 178 samples of wine
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
wine

library("car")
wine[2:6]

########################## Scatter Plot ##########################
# In this matrix scatterplot, the diagonal cells show histograms of each of the variables, 
# in this case the concentrations of the first five chemicals (variables V2, V3, V4, V5, V6).
#  the second cell in the first row is a scatterplot of V2 (y-axis) against V3 (x-axis)
scatterplotMatrix(wine[2:6])
plot(wine$V4, wine$V5)
# We can see from the scatterplot of V4 versus V5 that the wines from cultivar 2 seem to have lower values of V4
# compared to the wines of cultivar 1.
text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")

######################## Profile Plot #############################
# Shows the variation in each of the variables, by plotting
# the value of each of the variables for each of the samples.
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else { points(vectori, col=colouri,type="l") }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}
library(RColorBrewer)
names <- c("V2","V3","V4","V5","V6")
mylist <- list(wine$V2,wine$V3,wine$V4,wine$V5,wine$V6)
makeProfilePlot(mylist,names)
# It is clear from the profile plot that the mean and standard deviation for 
# V6 is quite a lot higher than that for the other variables.


######################## Calculating Summary Statistics for Multivariate Data #############################
# Another thing that you are likely to want to do is to calculate summary statistics such as the mean and standard
# deviation for each of the variables in your multivariate data set.
# sapply
# The "sapply()" function can be used to apply some other function to each column in a data frame, eg.
# sapply(mydataframe,sd) will calculate the standard deviation of each column in a dataframe "mydataframe".
sapply(wine[2:14],mean)
sapply(wine[2:14],sd)
# We can see here that it would make sense to standardise in order to compare the variables because the variables
# have very different standard deviations - the standard deviation of V14 is 314.9074743, while the standard deviation
# of V9 is just 0.1244533. Thus, in order to compare the variables, we need to standardise each variable so that
# it has a sample variance of 1 and sample mean of 0.

# To extract out the data for just cultivar 2, we can type:
cultivar2wine <- wine[wine$V1=="2",]
sapply(cultivar2wine[2:14],mean)
sapply(cultivar2wine[2:14],sd)

# prints out the mean and standard deviation of the variables for each group in your data set:
printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}
# mean and standard deviation for each of the 13 chemical
# concentrations, for each of the three different wine cultivars, we type:
printMeanAndSdByGroup(wine[2:14],wine[1])
# In this case, we see that there are 59 samples of cultivar 1, 71 of cultivar 2, and 48 of cultivar 3.


################################### Variance ####################################
calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}
#to calculate the withingroups variance of the variable V2 (the concentration of the first chemical), we type:
calcWithinGroupsVariance(wine[2],wine[1])
# Thus, the within-groups variance for V2 is 0.2620525.

calcBetweenGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the overall grand mean:
  grandmean <- mean(variable)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the mean and standard deviation for group i:
    meani <- mean(levelidata[,1])
    sdi <- sd(levelidata)
    numi <- levelilength * ((meani - grandmean)^2)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the between-groups variance
  Vb <- numtotal / (numlevels - 1)
  Vb <- Vb[[1]]
  return(Vb)
}
# calculate the between-groups variance for a variable such as V2:
calcBetweenGroupsVariance(wine[2],wine[1])
# Thus, the between-groups variance of V2 is 35.39742. But doens work here

calcSeparations <- function(variables,groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the separation for each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablename <- variablenames[i]
    Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
    sep <- Vb/Vw
    print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
  }
}
calcSeparations(wine[2:14],wine[1])


######################################## Covariance #####################################
calcWithinGroupsCovariance <- function(variable1,variable2,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the covariance of variable 1 and variable 2 for each group:
  Covw <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata1 <- variable1[groupvariable==leveli,]
    levelidata2 <- variable2[groupvariable==leveli,]
    mean1 <- mean(levelidata1)
    mean2 <- mean(levelidata2)
    levelilength <- length(levelidata1)
    # get the covariance for this group:
    term1 <- 0
    for (j in 1:levelilength)
    {
      term1 <- term1 + ((levelidata1[j] - mean1)*(levelidata2[j] - mean2))
    }
    Cov_groupi <- term1 # covariance for this group
    Covw <- Covw + Cov_groupi
  }
  totallength <- nrow(variable1)
  Covw <- Covw / (totallength - numlevels)
  return(Covw)
}
calcWithinGroupsCovariance(wine[8],wine[11],wine[1])

calcBetweenGroupsCovariance <- function(variable1,variable2,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the grand means
  variable1mean <- mean(variable1[,1])
  variable2mean <- mean(variable2[,1])
  # calculate the between-groups covariance
  Covb <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata1 <- variable1[groupvariable==leveli,]
    levelidata2 <- variable2[groupvariable==leveli,]
    mean1 <- mean(levelidata1)
    mean2 <- mean(levelidata2)
    levelilength <- length(levelidata1)
    term1 <- (mean1 - variable1mean)*(mean2 - variable2mean)*(levelilength)
    Covb <- Covb + term1
  }
  Covb <- Covb / (numlevels - 1)
  Covb <- Covb[[1]]
  return(Covb)
}
calcBetweenGroupsCovariance(wine[8],wine[11],wine[1])

######################################## Correlation #####################################
# correlation coefficient for the first two chemicals' concentrations, V2 and V3, we type:
# This tells us that the correlation coefficient is about 0.094, which is a very weak correlation. Furthermore, the
# P-value for the statistical test of whether the correlation coefficient is significantly different from zero is 0.21.
# This is much greater than 0.05 (which we can use here as a cutoff for statistical significance), so there is very weak
# evidence that that the correlation is non-zero.
cor.test(wine$V2, wine$V3)

# The function "mosthighlycorrelated()" will print out the linear correlation coefficients 
# for each pair of variables in your data set, in order of the correlation coefficient.
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
# top 10 pairwise correlation coefficients
mosthighlycorrelated(wine[2:14], 10)


############################  Standardising Variables ##############################
#  the first principal component would be dominated by the variables which show the largest variances, such as V14.
# Thus, it would be a better idea to first standardise the variables so that they all have variance 1 and mean 0, and to
#then carry out the principal component analysis on the standardised data. This would allow us to find the principal
#components that provide the best low-dimensional representation of the variation in the original data, without
#being overly biased by those variables that show the most variance in the original data.
standardisedconcentrations <- as.data.frame(scale(wine[2:14]))
# Note that we use the "as.data.frame()" function to convert the output of "scale()" into a "data frame", which is the
# same type of R variable that the "wine" variable.
# We can check that each of the standardised variables stored in "standardisedconcentrations" has a mean of 0 and
# a standard deviation of 1 by typing:
sapply(standardisedconcentrations,mean)
sapply(standardisedconcentrations,sd)



############################  Principal Component Analysis ##############################

# The purpose of principal component analysis is to find the best low-dimensional representation of the variation in
# a multivariate data set. For example, in the case of the wine data set, we have 13 chemical concentrations describing
# wine samples from three different cultivars. We can carry out a principal component analysis to investigate
# whether we can capture most of the variation between samples using a smaller number of new variables (principal
# components), where each of these new variables is a linear combination of all or some of the 13 chemical
# concentrations.
wine.pca <- prcomp(standardisedconcentrations) # do a PCA
summary(wine.pca)
wine.pca$sdev
# The total variance explained by the components is the sum of the variances of the components:
sum((wine.pca$sdev)^2)
# In this case, we see that the total variance is 13, which is equal to the number of standardised variables (13
# variables). This is because for standardised data, the variance of each standardised variable is 1. The total variance
# is equal to the sum of the variances of the individual variables, and since the variance of each standardised variable
# is 1, the total variance should be equal to the number of variables (13 here).


# In order to decide how many principal components should be retained, it is common to summarise the results of a
# principal components analysis by making a scree plot, which we can do in R using the "screeplot()" function:
screeplot(wine.pca, type="lines")
# The most obvious change in slope in the scree plot occurs at component 4, which is the "elbow" of the scree
# plot. Therefore, it cound be argued based on the basis of the scree plot that the first three components should be
# retained.
# Another way of deciding how many components to retain is to use Kaiser's criterion: that we should only retain
# principal components for which the variance is above 1 (when principal component analysis was applied to
# standardised data). We can check this by finding the variance of each of the principal components:
(wine.pca$sdev)^2
# We see that the variance is above 1 for principal components 1, 2, and 3 (which have variances 4.71, 2.50, and
# 1.45, respectively). Therefore, using Kaiser's criterion, we would retain the first three principal components.

# A third way to decide how many principal components to retain is to decide to keep the number of components
# required to explain at least some minimum amount of the total variance. For example, if it is important to explain
# at least 80% of the variance, we would retain the first five principal components, as we can see from the output of
# "summary(wine.pca)" that the first five principal components explain 80.2% of the variance (while the first four
# components explain just 73.6%, so are not sufficient).

wine.pca$rotation[,1]
# This means that the first principal component is a linear combination of the variables: -0.144*Z2 + 0.245*Z3 +
#   0.002*Z4 + 0.239*Z5 - 0.142*Z6 - 0.395*Z7 - 0.423*Z8 + 0.299*Z9 -0.313*Z10 + 0.089*Z11 - 0.297*Z12 -
#   0.376*Z13 - 0.287*Z14, where Z2, Z3, Z4...Z14 are the standardised versions of the variables V2, V3, V4...V14
# (that each have mean of 0 and variance of 1).
# Note that the square of the loadings sum to 1, as this is a constraint used in calculating the loadings:
sum((wine.pca$rotation[,1])^2)
# To calculate the values of the first principal component, we can define our own function to calculate a principal
# component given the loadings and the input variables' values:
calcpc <- function(variables,loadings)
{
  # find the number of samples in the data set
  as.data.frame(variables)
  numsamples <- nrow(variables)
  # make a vector to store the component
  pc <- numeric(numsamples)
  # find the number of variables
  numvariables <- length(variables)
  # calculate the value of the component for each sample
  for (i in 1:numsamples)
  {
    valuei <- 0
    for (j in 1:numvariables)
    {
      valueij <- variables[i,j]
      loadingj <- loadings[j]
      valuei <- valuei + (valueij * loadingj)
    }
    pc[i] <- valuei
  }
  return(pc)
}
calcpc(standardisedconcentrations, wine.pca$rotation[,1])
wine.pca$x[,1]

wine.pca$rotation[,2]
sum((wine.pca$rotation[,2])^2)

  
