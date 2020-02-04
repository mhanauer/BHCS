# STEM based analyses
library(mirt)
library(CTT)
library(lavaan)
library(psych)
library(ltm)
library(foreign)
library(dplyr)
library('GPArotation')
library(itemanalysis)
library(semTools)



#set up working directory
#working.directory<-"C:\\Users\\dsvet\\Box Sync\\____Since Tenure\\Research\\stem\\"
#working.directory<-"C:\\Users\\dsvet\\Box\\Matt_collaboration\\Data\\"
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale")
#setwd(working.directory)
#dir()

# read in entire data
#data<-read.csv("BAHCS_10_Demo.csv", header=TRUE)
n.data<-read.table("matt.dat", sep="\t", header=TRUE)
dim(n.data)
names(n.data)
names(n.data)

#basic reliability
clean.data<-na.omit(n.data)
dim(clean.data)

f1<-reliability(clean.data[,1:10], itemal = TRUE, NA.Delete = TRUE)
str(reliability(clean.data[,1:10],itemal=TRUE))
f1$pBis
f1$alphaIfDeleted

#split data into two for EFA and CFA analysis
## 50% of the sample size
smp_size <- floor(0.5 * nrow(n.data))


## set the seed to make your partition reproducible
set.seed(1233321)
train_ind <- sample(seq_len(nrow(n.data)), size = smp_size)

#train data is for EFA
#test data is for CFA
#n.data or clean.data for IRT

train <- n.data[train_ind, ]
test <- n.data[-train_ind, ]
dim(train)
dim(test)


# exploratory factor analsys using psych
efa_bahcs1 <- fa.poly(train[,1:10], nfactors=1, rotate="varimax")
efa_bahcs2 <- fa.poly(train[,1:10], nfactors=2, rotate="varimax")
efa_bahcs3 <- fa.poly(train[,1:10], nfactors=3, rotate="varimax")

efa_bahcs1$fa$loadings
fap <- fa.parallel.poly(train[,1:10])   # parallel analysis for dichotomous data
fap$pc.values
fap$fa.values
fa.diagram(fap)

vss(train[,1:10], n=3)
# CFA in lavaan
###CFA
BAHCS.model <- 'BAHCS  =~ manage_health + manage_mhealth + similar_goals + no_concern_side_effects+ has_money_food+health_friendly_home+ has_transport+social_activity+has_money_for_health+ed_level_satisfaction'
fit.BAHCS <- cfa(BAHCS.model, data=test[,1:10], estimator="WLSMV",
                 ordered=c( "manage_health","manage_mhealth", "similar_goals","no_concern_side_effects",
                            "has_money_food","health_friendly_home","has_transport","social_activity","has_money_for_health",    
                            "ed_level_satisfaction" ),std.lv = TRUE)
summary(fit.BAHCS, fit.measures=TRUE)
cfa.fit.results<-data.matrix(fitmeasures(fit.BAHCS, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

round(cfa.fit.results, digits=3)



#IRT on 6 items from BAHCS subscale
s <- 'F1 = 1-10'
model_BAHCS<-mirt.model(s)
model_BAHCS
#######################################################################
######################################################################
#fitting IRT model - graded
BAHCS.model.graded.c<-mirt(n.data[,1:10], model=model_BAHCS, itemtype = "graded", technical=list(removeEmptyRows=TRUE))



BAHCS.m2<-M2(BAHCS.model.graded.c,  na.rm=TRUE)
BAHCS.m2
BAHCS.i.fit.graded.c<-itemfit(BAHCS.model.graded.c, na.rm=TRUE)

BAHCS.i.fit.graded.c

irt.fit.results<-round(as.matrix(BAHCS.m2), digits=3)

################################################
#adjusted p values for item fit
p.val.BAHCS<-BAHCS.i.fit.graded.c[,5] #p values are stored in 5th column 
p.val.BAHCS
p.adj<-p.adjust(p.val.BAHCS, method="BH")
round(p.adj, digits=3)
#combined item fit
results<-cbind(BAHCS.i.fit.graded.c, round(p.adj, digits=3))
results

#getting out coefficients
BAHCS.coef.graded.c<-coef(BAHCS.model.graded.c, IRTpars=TRUE)
BAHCS.coef.graded.c<-coef(BAHCS.model.graded.c, IRTpars=TRUE, simplify = TRUE)


BAHCS.coef.graded.c

#itemplot(model.graded.c,item=3, type="infoSE")
plot(BAHCS.model.graded.c, type='infoSE', main="BAHCS Test Information and Standard Errors")







################################################







data.item<-clean.data[,1:10]

is.numeric(data.item)
is.data.frame(data.item)
head(data.item)
data.item<-as.numeric(data.item[,])
dim(data.item)
write.table(data.item, file="dataitem.dat", sep=",")
dat<-read.table("data_manual.txt", sep="\t", header=FALSE)
dim(dat)
head(dat)
as.numeric(dat)
is.data.frame(dat)




BAHCS.graded.c<-mirt(data.item, model=model_BAHCS, itemtype = "graded", technical=list(removeEmptyRows=TRUE))
BAHCS.gr.m2<-M2(BAHCS.graded.c, type='C2', na.rm=TRUE)
BAHCS.gr.m2
BAHCS.gradedfit<-itemfit(BAHCS.graded.c)
BAHCS.gradedfit



irt.fit.results<-round(as.matrix(BAHCS.m2), digits=3)




#in ltm, item.fit



x <- mirt(n.data[,1:10], 1, fit_stats= technical=list(removeEmptyRows=TRUE))
raschfit <- mirt(n.data[,1:10], 1, technical=list(removeEmptyRows=TRUE), itemtype='Rasch')
fit <- itemfit(x)
fit


#adjusted p values for item fit
p.val.BAHCS<-BAHCS.i.fit.graded.c[,5] #p values are stored in 5th column 
p.val.BAHCS
p.val.adjust.BAHCS<-p.adjust(p.val.BAHCS, method="BH")
round(p.val.adjust.BAHCS, digits=3)
#combined item fit
cbind(BAHCS.i.fit.graded.c, round(p.val.adjust.BAHCS, digits=3))

#getting out coefficients
BAHCS.coef.graded.c<-coef(BAHCS.model.graded.c, IRTpars=TRUE)
BAHCS.coef.graded.c<-coef(BAHCS.model.graded.c, IRTpars=TRUE, simplify = TRUE)


BAHCS.coef.graded.c

#itemplot(model.graded.c,item=3, type="infoSE")
plot(BAHCS.model.graded.c, type='infoSE', main="BAHCS Test Information and Standard Errors")



########### Compare different models
s <- 'F1 = 1-10'
model_comp<-mirt.model(s)
model_comp
#######################################################################
######################################################################
#fitting IRT model - pcm
results.pcm<-mirt(clean.data[,1:10], model=model_comp, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)
results.gpcm<-mirt(clean.data[,1:10], model=model_comp, itemtype="gpcm", SE=TRUE, verbose=FALSE)

BAHCS.model.graded.c

anova(results.pcm, results.gpcm)
############################################################

















#################################Rasch Analysis in R using eRm package for BAHCS data

s <- 'F1 = 1-10'
model_comp<-mirt.model(s)
model_comp
#######################################################################
######################################################################
#fitting IRT model - pcm
results.pcm<-mirt(clean.data[,1:10], model=model_comp, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)



results.gpcm<-mirt(clean.data[,1:10], model=model_comp, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
items.gpcm <- as.data.frame(coef.gpcm$items)
print(items.gpcm)


#check if partial credit or generalized partial credit is better
anova(results.gpcm, results.pcm, results.rsm)


#ratings scales model
#fitting IRT model - ratings scale
results.rsm<-mirt(clean.data[,1:10], model=model_comp, itemtype="rsm", SE=TRUE, verbose=FALSE)
coef.rsm <- coef(results.rsm, IRTpars=TRUE, simplify=TRUE)
items.rsm <- as.data.frame(coef.rsm$items)
print(items.rsm)

#graded response model
results.graded<-mirt(clean.data[,1:10], model=model_comp, itemtype="graded", SE=TRUE, verbose=FALSE)
coef.graded <- coef(results.graded, IRTpars=TRUE, simplify=TRUE)
items.graded <- as.data.frame(coef.graded$items)
print(items.graded)



plot(results.graded, type = 'trace', main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 
plot(results.graded, type='infoSE', main="BAHCS")





comp.m2<-M2(comp.model.graded.c, type='C2')
comp.m2

coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
#We can save the estimated item parameters as a new data frame called items.pcm and print the results using the following commands:
  
  items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)

itemtype="Rasch", SE=TRUE, verbose=FALSE


comp.i.fit.graded.c<-itemfit(comp.model.graded.c)#, na.rm=TRUE)
comp.i.fit.graded.c



#adjusted p values for item fit
p.val.comp<-comp.i.fit.graded.c[,5] #p values are stored in 5th column 
p.val.comp
p.val.adjust.comp<-p.adjust(p.val.comp, method="BH")
print(p.val.adjust.comp, scipen = 999)

p.val.adjust.comp(scientific=FALSE)

cbind(comp.i.fit.graded.c, p.val.adjust.comp)



comp.coef.graded.c<-coef(comp.model.graded.c, IRTpars=TRUE)
comp.coef.graded.c<-coef(comp.model.graded.c, IRTpars=TRUE, simplify = TRUE)


comp.coef.graded.c

#itemplot(model.graded.c,item=3, type="infoSE")
plot(comp.model.graded.c, type='infoSE', main="BAHCS")


# analysis using eRm 
library(eRm)
#Rasch model with missing values
res <- RSM(clean.data[,1:10])
res
summary(res)









library(eRm)

data.responses<-as.data.frame(clean.data[,1:10])
names(data.responses)

data.r<-n.data[,1:10]
names(data.r)


#clean data responses 

head(data.responses)

recoded.data<-data.frame(data.r)
recoded.data<-revalue(data.r , c(1,2,3,4,5), c(0,1,2,3,4))

c("1","2","3","4","5"), c("0","1","2","3","4")


summary(data.responses)
summary(recoded.data)

#estimate ratings scale model
rsm.est.F<-RSM(data.responses)
person.F<-person.parameter(rsm.est.F)
item.F<-itemfit(person.F)





#IRT on 10 items
s <- 'F1 = 1-10'
model_self<-mirt.model(s)
model_self

#######################################################################
######################################################################
#fitting IRT model - graded
model.graded.c<-mirt(data[,1:10], model=model_self, itemtype = "graded", technical=list(removeEmptyRows=TRUE))

model.graded.c.2<-mirt(data[,1:10], model=model_self, itemtype = "spline", technical=list(removeEmptyRows=TRUE))
i.fit.graded.c<-itemfit(model.graded.c)#, na.rm=TRUE)
i.fit.graded.c

m2<-M2(self.model.graded.c, type='C2', na.rm=TRUE)
m2


model.rasch<-mirt(data[,1:10],1, itemtype="Rasch",technical=list(removeEmptyRows=TRUE))
model.rasch

itemfit(model.rasch, method='ML')

itemfit(model.rating)#, empirical.plot = 1) #empirical item plot

  
  x <- mirt(data, 1)
raschfit <- mirt(data, 1, itemtype='Rasch')
fit <- itemfit(x)
fit


#adjusted p values for item fit
p.val<-i.fit.graded.c[,5] #p values are stored in 5th column 
p.val
p.val.adjust<-p.adjust(p.val, method="BH")
round(p.val.adjust, digits=3)
#combined item fit
cbind(self.i.fit.graded.c, round(p.val.adjust.self, digits=3))

#getting out coefficients
coef.graded.c<-coef(self.model.graded.c, IRTpars=TRUE)
coef.graded.c<-coef(self.model.graded.c, IRTpars=TRUE, simplify = TRUE)


coef.graded.c

#itemplot(model.graded.c,item=3, type="infoSE")
plot(self.model.graded.c, type='infoSE', main="BAHCS")



####################################################################
#######################DIF Analysis
####################################################################
library(lordif)
#for race, only take white and black since we shouldn't mix with others....need to make a comment that n's are very different
race_dif<-ifelse(n.data$Race=="WHITE OR CAUCASIAN", 1, ifelse(n.data$Race=="BLACK OR AFRICAN AMERICAN",2,NA))
raceDIF = lordif(resp.data = n.data[,1:10], group = race_dif, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(raceDIF)
plot(raceDIF)
# no items were flagged as DIF for race; still note unequal n

#DIF for Gender just 1 and 2 for males and females
sex_dif<-ifelse(n.data$Gender=="MALE", 1, ifelse(n.data$Gender=="FEMALE",2,NA))
summary(sex_dif)
count(sex_dif)
sexDIF = lordif(resp.data = n.data[,1:10], group = sex_dif, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(sexDIF)
plot(sexDIF)
#one item (2) was flagged with dIF


# DIF on AGE
#median age is 47 so split dif for age at 47
age_dif<-ifelse(n.data$Age<47, 1, ifelse(n.data$Age>=47,2,NA))
ageDIF = lordif(resp.data = n.data[,1:10], group = age_dif, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(ageDIF)
plot(ageDIF)
#1,6,9,10 items flagged as DIF for age

