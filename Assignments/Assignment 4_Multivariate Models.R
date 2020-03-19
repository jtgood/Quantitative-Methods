#Assigment 4
#Multivariate Models
#Jeff Good

install.packages('vegan')
library(vegan)
?vegan
data(dune)
data(dune.env)
dune_mds = metaMDS(dune)

#1. 
plot(dune_mds, type = 'n')
text(dune_mds, 'sp', cex = .5)
#generate vectors of colors
color_vect = rev(terrain.colors(6)) [-1] # [-1] means: removes first element in 
#a vector
points(dune_mds, 'sites', pch = 19,
       col = color_vect[dune.env$Moisture])
legend('topright', paste("Moisture =", 1:5, sep = ''),
       col = color_vect, pch=19)

dune_fit=envfit(dune_mds, dune.env)
dune_fit#reviewed in class

#NMDS: non-metric multidimensional scaling (indirect): distance based ordination
#non eigenbased. Multi dimensional converted into 2D representation called a 
#"ordination"
#The goal of creating this NMDS plot is to show how related or dissimilar 
#the species are from each other in consideration to the variables that went 
#into the dataframe.  
#Moisture seems to be a deciding factor of relatidness to other variables at 
#each site.  More moisture is more prevvalent moving positively on the x axis,
#while more dry sites are around 0 or -1 on the x axis.

#2. 
dune_cca <- cca(dune ~ ., data = dune.env)# '.' is a shortcut to show all of the 
#environmental variables.
dune_cca
anova(dune_cca, permutations = 999) #entire model: is significant explaining 
#in explaining variance.
anova(dune_cca, by = 'margin', permutations = 999)# individual variables no 
#signficance? A1 most signficant? 
?RsquareAdj
RsquareAdj(dune_cca)
#R^2 = 0.71
#Adj R^2 = 0.22
plot(dune_cca)
ordiplot(dune_cca, display = 'sp')#what is 'sp'?
orditorp(dune_cca, display = 'sp')

#3. 
#The NMDS and CCA models are fairly different: In the NMDS, we can visually see
#the dissimilarity of species across multiple environmental variables.  The CCA
#uses regression between these variables and the species to determine a similar 
#relatedness that is backed with quantitative background.  In a way they are similar, 
#but have fundamentally different basises that contribute to different interpretations
#of analyses.  
#I find the CCA modeling to be more useful due to the visual represenation of a 
#regressions across multiple environmental variables in a 2D representation in addition
#the the quantitative analyses able to be performed with the ANOVAs.


# Other code reviewed in class: 
dune$Moisture = factor(dune$Moisture, levels = 1:5, ordered = FALSE)
contrasts(dune$Moisture)

