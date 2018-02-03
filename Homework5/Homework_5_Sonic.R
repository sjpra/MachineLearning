##################
###Question 1a####
##################

library(freqparcoord)
library(regtools)
data(mlb)

#Random Partitioning of Data:

xvalpart <- function(data, p) 
{
  n <- nrow(mlb)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train=data[trainidxs ,],
       valid=data[-trainidxs ,])
}

#Liner MOdel 
xvallm = function(data, ycol, predvars, p, meanabs=TRUE){
  tmp = xvalpart(data,p)
  train = tmp$train
  valid = tmp$valid
  trainy = train[ , ycol]
  trainpreds = train[ , predvars]
  trainpreds = as.matrix(trainpreds)
  lmout = lm(trainy ~ trainpreds)
  validpreds = as.matrix(valid[ , predvars])
  predy = cbind(1, validpreds)%*% coef(lmout)
  realy = valid[ , ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list( predy = predy , realy = realy)
}

#Checking
set.seed (1011)
Norm = xvallm(mlb, 5, c(4,6), 2/3)
#Answer Trial 1  - 12.45891


#Now Checking the Knn Case

xvalknn = function(data,ycol ,predvars ,k,p,meanabs=TRUE) 
{
  data = data[, c(predvars, ycol)] 
  ycol = length(predvars) + 1
  tmp = xvalpart(data,p)
  train = tmp$train
  valid = tmp$valid
  valid = as.matrix(valid)
  xd = preprocessx(train[,-ycol],k)
  kout = knnest(train[,ycol],xd,k)
  predy = predict(kout, valid[, -ycol], TRUE) 
  realy = valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy))) 
  list (predy = predy , realy = realy)
}

set.seed (1011)
knn = xvalknn(mlb, 5, c(4 ,6), 5, 2/3)

#Answer 2 = 13.96154

compare1 = as.matrix(rbind(Norm,knn))
colnames(compare1) = "Values1"
compare1

#########
#Lets run it again with another Seed
##########
xvalpart <- function(data, p) 
{
  n <- nrow(mlb)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train=data[trainidxs ,],
       valid=data[-trainidxs ,])
}

#Liner MOdel 
xvallm = function(data, ycol, predvars, p, meanabs=TRUE){
  tmp = xvalpart(data,p)
  train = tmp$train
  valid = tmp$valid
  trainy = train[ , ycol]
  trainpreds = train[ , predvars]
  trainpreds = as.matrix(trainpreds)
  lmout = lm(trainy ~ trainpreds)
  validpreds = as.matrix(valid[ , predvars])
  predy = cbind(1, validpreds)%*% coef(lmout)
  realy = valid[ , ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list( predy = predy , realy = realy)
}

#Checking
set.seed (1504)
Norm = xvallm(mlb, 5, c(4,6), 2/3)
#Answer Trial 1  - 12.45891


#Now Checking the Knn Case

xvalknn = function(data,ycol ,predvars ,k,p,meanabs=TRUE) 
{
  data = data[, c(predvars, ycol)] 
  ycol = length(predvars) + 1
  tmp = xvalpart(data,p)
  train = tmp$train
  valid = tmp$valid
  valid = as.matrix(valid)
  xd = preprocessx(train[,-ycol],k)
  kout = knnest(train[,ycol],xd,k)
  predy = predict(kout, valid[, -ycol], TRUE) 
  realy = valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy))) 
  list (predy = predy , realy = realy)
}

set.seed (1504)
knn = xvalknn(mlb, 5, c(4 ,6), 5, 2/3)

#Answer 2 = 13.96154

compare2 = as.matrix(rbind(Norm,knn))
colnames(compare2) = "Values2"
compare2

######
#Comparison between two runs
######

Final Compare
final = as.matrix(cbind(compare1,compare2))
final

######################
###Question 1.2#######
######################

library(freqparcoord)
data(prgeng)
prgeng$age2 = prgeng$age^2
edu = prgeng$educ
prgeng$ms = as.integer(edu == 14) 
prgeng$phd = as.integer(edu == 16) 
prgeng$fem = prgeng$sex - 1
tmp =  prgeng[edu >= 13,]
pe = tmp[ ,c(1 ,12 ,9 ,13 ,14 ,15 ,8)]
pe_new = data.frame(pe)
head(pe_new)
pe_new$age_fem = pe_new$age*pe_new$fem
pe_new$age2_fem = pe_new$age2*pe_new$fem
head(pe_new)

model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + age_fem + age2_fem, data = pe_new)
summary(model)

age=32
age2= 32 * 32
fem = 1
age_fem = 32
age2_fem = 32*32
ms = 1
phd = 0
wkswrkd = median(pe_new$wkswrkd)

pred =  data.frame(age, age2, ms, phd, wkswrkd, fem, age_fem, age2_fem)
specpred = as.matrix(predict(model, pred))
colnames(specpred) = "Wage in $"
rownames(specpred) = "Wage for a 32-year-old female with a Masters degree"
specpred

##############################################
## Questions 1.3 and 1.4 is in the python code
##############################################

############################
##Page Number 121 Querstions
############################

##################
###Question 1a####
##################

library(freqparcoord)
data(prgeng)
prgeng$age2 = prgeng$age^2
edu = prgeng$educ
prgeng$ms = as.integer(edu == 14) 
prgeng$phd = as.integer(edu == 16) 
prgeng$fem = prgeng$sex - 1
tmp =  prgeng[edu >= 13,]
pe_2 = tmp[ ,c(1 ,12 ,9 ,13 ,14 ,15 ,8)]
pe_new = data.frame(pe)
head(pe_new)
model_2_1 = as.matrix(lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = pe_new))
model_2_1

summary(model_2_1)

UCL = -11176.74 + 1.96 * 912.206
LCL = -11176.74 - 1.96 * 912.206

Result = as.matrix(cbind(LCL,UCL))
rownames(Result) = "Confidence Intervals"
Result

##################
###Question 1b####
##################

library(freqparcoord)
data(prgeng)
prgeng$age2 = prgeng$age^2
edu = prgeng$educ
prgeng$ms = as.integer(edu == 14) 
prgeng$phd = as.integer(edu == 16) 
prgeng$fem = prgeng$sex - 1
prgeng$msfem = prgeng$ms * prgeng$fem
prgeng$phdfem = prgeng$phd * prgeng$fem
tmp =  prgeng[edu >= 13,]
pe_1b = tmp[ ,c(1 ,12 ,9 ,13 ,14 , 15, 16, 17 ,8)]
b1b_model <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + msfem + phdfem, data = pe_2b)
summary(b1b_model)
LCL1b <- -5088.779 - 1.96 * 1975.841
UCL1b <- -5088.779 + 1.96 * 1975.841

Result_1b = as.matrix(cbind(LCL2b,UCL2b))
rownames(Result_1b) = "Confidence Intervals"
Result_1b

##################
###Question 2####
##################

bike = read.csv('day.csv')
head(bike)

bike$temp2 =  (bike$temp)^2
bike$clearday = as.integer(bike$weathersit == 1)

bike_model <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = bike)
summary(bike_model)

LCL_2 = 1716.25 - 1.96 * 56.68
UCL_2 = 1716.25 + 1.96 * 56.68

Result_2 = as.matrix(cbind(LCL_2,UCL_2))
rownames(Result_2) = "Confidence Intervals"
Result_2


##################
###Question 4#####
##################

#Given Details
# var(errors) = p ----------------------- 1
# var(Xi) = 1 --------------------------- 2
# Y = X1 + X2 + ..... + Xp + errors ----- 3 (Assuming all Beta = 1 and Beta0 = 0)

#Solution
# rho^2 = 1 - (var(errors) / var(Y))
# p = var(X1) + var(X2) + var(X3).... + var(Xp) 
# From 3 and 1 --> var(Y) = p + p = 2p
# From 1 --> rho^2 =  1 - (p / 2p)
#            = 1 - (1 / 2)
#            = 0.5
