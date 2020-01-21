#Jeff Good 
#R Intermediate Assignment
#1/21/2020

data("iris")
head(iris)

sp_ids = unique(iris$Species)#UNIQUE takes only one header from each title
# in this case: one of each species.  
?unique
output = matrix(0, nrow=length(sp_ids),ncol(iris)-1)#-1 because species will be
#column one
rownames(output) = sp_ids
colnames(output) = names(iris[ ,-ncol(iris)])
for (i in seq_along(sp_ids)) {
  iris_sp = subset(iris, subset=Species == sp_ids[i], select=-Species)
  for(j in 1:(ncol(iris_sp))) {
    x =0
    y =0
    if (nrow(iris_sp)>0) {
      for(k in 1:nrow(iris_sp)) {
        x = x+iris_sp[k,j]
        y = y+1
      }
      output[i,j] =x/y
    }
  }
}
output

##Excercises

#Iris Loops

#1. The loop created a matrix of average values of different variables (Sepal 
#Length, Sepal Width, Petal Length, and Petal Width) for each of the three species of 
#flowers.

#2. 
#Unique: use only one of each species name and save it as sp_ids.
  #when commanding an output, create a matrix with rows as the names of the unique
  #species ids, and columns of the iris data.
#loop for data columns:
  #define columns using a variable "iris_sp" that is subset with species specific
  #data.
  #If rows of iris_sp are greater than 0, then sum those numbers with respective
  #counts. 
#Output the mean of the designated species of that data column.

#3. x and y can be rewritten as the end values that make up the final output 
#data matrix.  For example, x <- iris_total, and y <- number_input (describes
# how to get to average clearer than simple variables).

#4. Using "mean" to calculate values should be able to achieve the same goal
# rather than defining each number and manually didiving those numbers to get an 
#average.  

#Sum of Sequence

#5. 
x <- 1:10
y <- NULL#Loop gives output the value. 
for(i in 1:length(x)) {
  y[i] = sum (x[1:i])
}
y

#6. 
x <- 1:10
y <- NULL
for(i in 1:length(x)) {
  y[i] = sum (x[1:i])
  if (y[i] > 10) {#Greater than 10 outputs as NA)
    y[i] = 'NA'
  } 
} 
y

#7. 
adv_fnx <- function (x) {
  for (i in 1:length(x)) {
    y[i] = sum(x[1:i])
    if(y[i]>10)
      y[i] = 'NA'
    else{
      (y[i]<-sum(x[1:i]))
    }
  }
  print(y)
}

adv_fnx(1:25)

