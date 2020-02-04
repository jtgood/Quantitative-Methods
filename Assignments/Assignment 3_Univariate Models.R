trees <- read.csv('./Data/treedata_subset.csv')
trees
str(trees)#see structured data set. 
#Absense is improtant: 
#for single vector: tapply(thing you want to summrize, list(group1, group2), 
#function to use))
#for multiple coloumns: aggregate( ______, site, mean)
tree_cover<- tapply(trees$cover, list(trees$plotID, trees$species), mean)# changes 
#from long format to wide format
aggregate(trees$cover, trees$site, mean)
#How to change NA (absenses) to 0?
ifelse(is.na(tree_cover), 0, tree_cover)# absenses NA changed to 0 using "is.na" 
#function
install.packages('car')
library(car)
#1. What environmental conditions are contributuing to the "cover" 
head(trees)
summary(trees)
plot(trees)
plot(tree_cover)

columns <- c('cover', 'elev', 'tci', 'streamdist', 'disturb', 'beers')
Acer <- subset(trees, subset = species == 'Acer rubrum',
               select = columns)
Acer_prelim <- lm(cover ~ ., data = Acer)
anova(Acer_prelim)#Can't load the Anova from car?
summary(Acer_prelim)
plot(Acer_prelim)

Abies <- subset(trees, subset = species == 'Abies fraseri', 
                select = columns)
Abies_prelim <- lm(cover ~ ., data = Abies)
anova(Abies_prelim)
summary(Abies_prelim)
plot(Abies_prelim) # How do you see individual plots against each ofthe different 
#variables?
#These models do not show a strong correlation of each species. cover. 
#However, Abies fraseri (0.506) has a higher adjusted R^2 than 
#Acer rubrum (0.311).
#Elevation and beers have the most importance of influence on Acer cover
#Elevation is the most imporant influencer of Abies cover.
#Models may assume equal variance and normality and therefore may explain why there
#is a difference in Acer and Abies models. Since these are two different species,
#conditions that affect their coverage may have different responses from respective
#trees.

#2.
acer_poi <- glm(cover ~ tci + elev + tci + disturb + beers, data = Acer, 
               family='poisson')
acer_poi
plot(acer_poi)
pseudo_r2 <- function(glm_mod) {
  1 -  glm_mod$deviance / glm_mod$null.deviance
}
pseudo_r2
plot(pseudo_r2)
anova(acer_poi)
summary(acer_poi)#beers has the greatest has the greatest signficicance.

abies_poi <- glm(cover ~ tci + elev + tci + disturb + beers, data = Abies, 
               family='poisson')
plot(abies_poi)
anova(abies_poi)
summary(abies_poi)# elevation has the greatest signifcance.
#There seems to be similar results between the Gaussian models and the GLM models:
#Acer coverage is significant with mountain slope (beers), "water potential" (tci)
#and elevation.  Beers was most significant and differed from the preivous model's
#elevation significance when considering Acer cover. 
#Abies coverage is significant with elevation.  This was also seen in the original
#gaussian model as well in the glm.  These variables thus describe the influence on
#respective tree species coverage.


#4. Would much apprechiate some help with some of these optional functions
#specifically which "arguments" correspond to the data that I am working with.
?stepAIC
#stepAIC(object, scope, scale = 0,
  #direction = c("both", "backward", "forward"),
  #trace = 1, keep = NULL, steps = 1000, use.start = FALSE,
  #k = 2, ...)
AcerAIC <- stepAIC(cover ~ tci + elev + tci + disturb + beers, data = Acer)


#5. #probably need some help on this too.  
unique(trees$plotID) #734
columns <- c('cover', 'elev', 'tci', 'streamdist', 'disturb', 'beers')
sites <- subset(trees, subset = plotID == 'unique',
               select = columns)
summary(sites)
sites_poi <- glm(cover ~ tci + elev + tci + disturb + beers, data = sites, 
                 family='poisson')



