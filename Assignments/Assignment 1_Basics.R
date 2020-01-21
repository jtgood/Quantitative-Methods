#Assignment 1: Basics
#Jeff Good
#January 16, 2020

getwd()
setwd('..')
getwd()
tgpp<-read.csv("./Basics/tgpp.csv")
tgpp#This dataset represents the vascular plant species richness that was 
#collected from the Tallgrass Prairie Preserve from 10 x 10 m quadrats. 
#Species richness is simply the number of species that occur within a quadrat

#Questions:
#1. 
tgpp
#Names of the columns: Plot, Year, Record ID, Corner, Scale, Richness, 
#Easting, Northing, Slope, pH, Yrsslb

#2.
nrow(tgpp)#count rows of selected data set
ncol(tgpp)#count columns of selected data set
#Number of rows and columns:[4080,11]

#3.
sapply(tgpp,class) # How to find the object type of each column
# Plot: interger, Year: integer, Corner: integer, Scale: numeric, Richness
#: integer, Easting: integer, Nothing(?): integer, Slope: integer, pH: numeric, 
#Yrsslb: numeric

#4.
tgpp[c(1,5,8), c(3,7,10)]#vlaues of selected rows and columns
#Values at rows 1,5,8 at columns 3,7,10: 
  #record_id easting  ph
  #1       187  727000 6.9
  #5       191  727000 6.9
  #8       194  727000 6.9

#5.
getwd()
pdf('./Basics/Scale_fig1.pdf')
plot(tgpp$scale, tgpp$richness, xlab='Scale', ylab='Richness', col='cyan3')
dev.off()
# plotted relationship between Scale and Richness and exported as PDF

#6.
plot(tgpp$scale, tgpp$richness, xlab='Scale', ylab='Richness', col='cyan3', 
     log='xy')
#X axis is converted to a log scale and is graphically represented

