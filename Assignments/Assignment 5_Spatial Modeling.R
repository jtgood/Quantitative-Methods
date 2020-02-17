#Assignement 5: Spatial Modeling
#Jeff Good
#2/11/2020

library(vegan)
library(nlme)
data("BCI")
BCI_xy = data.frame(x = rep(seq(625754, 626654, by=100), each=5), 
                    y = rep(seq(1011569,  1011969, by=100), len=50))

#1. Evidence of spatial dependence:

plot(BCI_xy) 
head(BCI)
summary(BCI)
#BCI is abundance of species
#BCI_xy is cooridnates
#Common: coloumn 11: Alseis
#Rare: coloumn 225: Zuelania 
#Common and rare determined by approximate and relative presence by site
common <- BCI[,11]
rare <- BCI[,225]
plot(common)
plot(rare)
plot(common~rare)#need to integrate the coordiantes with the abundance
?colSums()
z <- colSums(BCI, na.rm = FALSE, dims = 1)
hist(z)# super skewed

col_brks = hist(common, plot = F)$breaks
col_indices = as.numeric(cut(common, col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(BCI_xy, cex=2, pch=19, col = cols[col_indices])

col_brks = hist(rare, plot = F)$breaks
col_indices = as.numeric(cut(rare, col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(BCI_xy, cex=2, pch=19, col = cols[col_indices])
#?? - Function example to do this for both rare and Common?)

common_dist <- dist(BCI[ ,common])
rare_dist <- dist(BCI[ , rare])
xy_dist <- dist(BCI_xy)
max_dist <- max(xy_dist) / 2

plot(xy_dist ~ common_dist)
abline(lm(xy_dist ~ common_dist), lwd=3, col='red')
lines(lowess(xy_dist ~ common_dist), lwd=3, col='pink')
lines(lowess(xy_dist ~ common_dist, f=0.1), lwd=3, col='blue')
abline(v = max_dist, col='red', lwd=3, lty=2)
#?? sometimes the line (abline = vmax) doesnt show up

plot(xy_dist ~ rare_dist)
abline(lm(xy_dist ~ rare_dist), lwd=3, col='red')
lines(lowess(xy_dist ~ rare_dist), lwd=3, col='pink')
lines(lowess(xy_dist ~ rare_dist, f=0.1), lwd=3, col='blue')
abline(v = max_dist, col='red', lwd=3, lty=2)

mantel(xy_dist, common_dist)
#The common species, Alseis sp., has a slight positive correlation 
#with abundance and spatial distance whereas the rare species, Zuelania, 
#exhibited a slightly negative correlation.


#2. 
#Drypetes standleyi
#Model 1. ds against single species predictor value
#Model 2. ds against all species
sp_ids = c("Cordia.lasiocalyx", "Hirtella.triandra",
           "Picramnia.latifolia", "Quassia.amara",
           "Tabernaemontana.arborea", "Trattinnickia.aspera", 
           "Xylopia.macrantha")
df <- data.frame(BCI, BCI_xy)
single <- as.formula(paste("Drypetes.standleyi ~ ",
                           paste(sp_ids[1], collapse= "+")))
all <- as.formula(paste("Drypetes.standleyi ~ ",
                         paste(sp_ids, collapse= "+")))
model1 <- gls(single, data=df)
model2 <- gls(all, data=df)

plot(Variogram(model1, form = ~ x + y))
plot(Variogram(model1, resType='normalized'))
plot(Variogram(model2, form = ~ x + y))
plot(Variogram(model2, resType = 'normalized'))
summary(model1)
summary(model2)

#exp
model1_exp <- update(model1, corr = corExp(form = ~ x+y))
plot(Variogram(model1_exp, maxDist = max_dist))
plot(Variogram(model1_exp, resType='normalized', maxDist = max_dist))
#normally distributed
model2_exp <- update(model2, corr = corExp(form = ~ x+y))
plot(Variogram(model2_exp, maxDist = max_dist))
plot(Variogram(model2_exp, resType = 'normalized', maxDist = max_dist))
#normally distributed 
summary(model1_exp)
summary(model2_exp)
#? Better models available? Maybe spherical? 

#with nugget
model1_exp_nug <- update(model1_exp, corr = corExp(form = ~ x + y, nugget = T))
plot(Variogram(model1_exp_nug, maxDist = max_dist)) #much better fit
model2_exp_nug <- update(model2_exp, corr = corExp(form = ~ x + y, nugget = T))
plot(Variogram(model2_exp_nug, maxDist = max_dist))# not so good of a fit
plot(Variogram(model2_exp_nug, resType = 'n', maxDist = max_dist))
summary(model1_exp_nug)
summary(model2_exp_nug)

anova(model1, model1_exp, model1_exp_nug)
anova(model2, model2_exp, model2_exp_nug)
#? not working due to forms not being two sided? 

#By including spatial error terms, the single species comparison 
#had a farily large impact on the coefficents of the model as seen
#from the very different outputs between the two graphs respective of 
#their data input.

#ANOVA not working? Why is my 'form' not a two-sided fromula?

#Adding a spatial error term should have influenced the models because
#some of the species actually were seen to be spatially distributed as seen 
#in part one of the assignment.  If these species were included
#in the modeling, their spatial structure would have lowered the error between
#the two models.









