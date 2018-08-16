---
---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
```


Load data.  Just get the actual data for now don't worry about sub group analyses.  
Add a state ID variable so we can differential them later on
```{r}
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale/7-13-18HCSData")
#CIL = read.csv("CIL HCS Dataset_07172018.csv", header = TRUE)
#CKY = read.csv("CKY HCS Dataset_07172018.csv", header = TRUE)
head(CIL)
CIL = cbind(CIL[c("SourceClient_ID", "good_health", "manage_health", "knows_conditions", "phys_activity", "manage_mhealth", "has_goals", "not_overwhelmed", "has_pcp", "similar_goals", "health_literacy", "no_future_hosp", "no_ED_use", "knows_meds", "takes_meds", "no_concern_side_effects", "can_cook", "access_nut_food", "has_money_food", "eats_nut_food", "health_friendly_home", "accessible_home", "living_sit_satisfaction", "has_home", "safe_neighborhood", "near_supports", "has_transport", "others_support_health", "social_activity", "no_one_opposes", "has_money_for_family", "manage_money", "has_money_for_health", "ed_level_satisfaction", "job_satisfaction", "able_to_not_smoke", "able_to_not_use")])
CIL$StateID = rep(0, dim(CIL)[1])

CKY = cbind(CKY[c("SourceClient_ID","good_health", "manage_health", "knows_conditions", "phys_activity", "manage_mhealth", "has_goals", "not_overwhelmed", "has_pcp", "similar_goals", "health_literacy", "no_future_hosp", "no_ED_use", "knows_meds", "takes_meds", "no_concern_side_effects", "can_cook", "access_nut_food", "has_money_food", "eats_nut_food", "health_friendly_home", "accessible_home", "living_sit_satisfaction", "has_home", "safe_neighborhood", "near_supports", "has_transport", "others_support_health", "social_activity", "no_one_opposes", "has_money_for_family", "manage_money", "has_money_for_health", "ed_level_satisfaction", "job_satisfaction", "able_to_not_smoke", "able_to_not_use")])
CKY$StateID = rep(1, dim(CKY)[1])


CIL_CKY = data.frame(rbind(CIL, CKY))
write.csv(CIL_CKY, "CIL_CKY.csv", row.names = FALSE)
CIL_CKY = read.csv("CIL_CKY.csv", header = TRUE)
head(CIL_CKY)
dim(CIL_CKY)
CIL_CKY$can_cook = as.integer(CIL_CKY$can_cook)

CIL_CKY_Complete = na.omit(CIL_CKY)
dim(CIL_CKY_Complete)

```
Now let us load in the demographics
Get rid of immigration status only Ill has it so we can rbind them
Then we can merge on SourceID for the full file then subset below for full analysis
Now merge them on 
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale/7-13-18HCSData")
CILDemo = read.csv("Brief HCS - IL - Demographics - 20180813.csv", header = TRUE)
CKYDemo = read.csv("Brief HCS - KY - Demographics - 20180813.csv", header = TRUE)
CILDemo$Immigration.Status = NULL
# need to rename without the period number 3
head(CKYDemo)
CIL_CKY_Demo = rbind(CILDemo, CKYDemo)
names(CIL_CKY_Demo)[3] = "SourceClient_ID"
CIL_CKY = merge(CIL_CKY, CIL_CKY_Demo, by = "SourceClient_ID", all = TRUE)
head(CIL_CKY)
```
Now let us see how much data is misisng 
```{r}
CIL_CKYComplete = na.omit(CIL_CKY)
dim(CIL_CKYComplete)
dim(CIL_CKY)
CIL_CKYDemo = CIL_CKY
```
Let us get some descriptives and get rid of those who are under 18
```{r}
# Get rid of client ID don't need it any more
CIL_CKYDemo$SourceClient_ID = NULL
CIL_CKYDemo$Last.Service.Date = NULL
CIL_CKYDemo$Source.System = NULL
CIL_CKYDemo$Data.Warehouse.Client.ID = NULL
CIL_CKYDemo$Ethnicity = NULL
CIL_CKYDemo = subset(CIL_CKYDemo, Age > 17)
demoCIL_CKY = apply(CIL_CKYDemo, 2, function(x){describe.factor(x)})
demoCIL_CKY
```


There is a rouge 0 and 7.  Probably should get rid of those.
```{r}
CIL_CKY= subset(CIL_CKY, Age > 17)
CIL_CKY = cbind(CIL_CKY[c("manage_health" , "manage_mhealth" ,"similar_goals" ,"no_concern_side_effects" ,"has_money_food" ,"health_friendly_home" , "has_transport" , "social_activity" , "has_money_for_health" , "ed_level_satisfaction")])

descriptivesCIL_CKY = apply(CIL_CKY, 2, function(x){describe.factor(x)})
descriptivesCIL_CKY
CIL_CKY = apply(CIL_CKY, 2, function(x){ifelse(x > 5, NA, ifelse(x < 1, NA, x))})
CIL_CKY = data.frame(CIL_CKY)
describe.factor(CIL_CKY$similar_goals)
describe.factor(CIL_CKY$has_money_for_health)
describe(CIL_CKY)

```
Get the percentage of missing data
```{r}
dim(CIL_CKY)
CIL_CKYComplete = na.omit(CIL_CKY)
dim(CIL_CKY_Complete)
1-(dim(CIL_CKYComplete)[1] / dim(CIL_CKY)[1])

CIL_CKYMissingDiag =  amelia(CIL_CKY)
summary(CIL_CKYMissingDiag)

```
CFA Full
```{r}
model1 = 'HCA =~ good_health+ manage_health+ knows_conditions+ phys_activity+ manage_mhealth+ has_goals+ not_overwhelmed+ has_pcp+ similar_goals+ health_literacy+ no_future_hosp+ no_ED_use+ knows_meds+ takes_meds+ no_concern_side_effects+ can_cook+ access_nut_food+ has_money_food+ eats_nut_food+ health_friendly_home+ accessible_home+ living_sit_satisfaction+ has_home+ safe_neighborhood+ near_supports+ has_transport+ others_support_health+ social_activity+ no_one_opposes+ has_money_for_family+ manage_money+ has_money_for_health+ ed_level_satisfaction+ job_satisfaction'
fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKYDemo)
summary(fit1, fit.measures = TRUE)
```


Final CFA model
```{r}
model1  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKYDemo)
summary(fit1, fit.measures = TRUE)


fit1Complete = cfa(model1, estimator = "MLR", std.lv = TRUE, data = CIL_CKYDemo)
summary(fit1Complete, fit.measures = TRUE)

```
Reliabiltiy
```{r}
omegaItems = omega(CIL_CKY); summary(omegaItems)
```
Parrell analyses
```{r}
parallel = fa.parallel(CIL_CKYItems, fa= "fa")
parallel$fa.values
```
Now let us try measurement invariance with gender then make race 
```{r}
modelGender = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = CIL_CKYDemo, group = "Gender", strict = TRUE)

describe.factor(CIL_CKYDemo$Race)
CIL_CKYDemoRace = subset(CIL_CKYDemo, Race != "UNKNOWN")
describe.factor(CIL_CKYDemoRace$Race)
CIL_CKYDemoRace$Race = ifelse(CIL_CKYDemoRace$Race == "WHITE OR CAUCASIAN", 1, 0)
describe.factor(CIL_CKYDemoRace$Race)

modelRace = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = CIL_CKYDemoRace, group = "Race", strict = TRUE)
```
Now that we have evidence of unidimentionsaility, let's do IRT analysis on the construct
Constrained, means the discrimination parameters are estimated but all the same.
```{r}
CIL_CKYCompleteAnalysis = na.omit(CIL_CKY)

fitOrd1 = grm(data = CIL_CKY, constrained = FALSE, Hessian  = TRUE)
fitOrd2 = grm(data = CIL_CKY_Complete)
fitOrd1Complete = grm(data = CIL_CKYCompleteAnalysis, constrained = TRUE, Hessian  = TRUE)
summary(fitOrd1Complete)

fitOrdPGRM = gpcm(data = CIL_CKY, constraint = "gpcm")
summary(fitOrdPGRM)
AIC(fitOrdPGRM)
AIC(fitOrd1)
BIC(fitOrdPGRM)
BIC(fitOrd1)
summary(fitOrd1)

GoF.gpcm(fitOrdPGRM)

margins(fitOrd1)
margins(fitOrd1, type = "three")
information(fitOrd1, c(-3, 2))

plot(fitOrd1, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1, type = "OCCu")


plot(fitOrd1, items = 1, type = "OCCu", zrange = c(-3,3))

vals <- plot(fitOrd1, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 
plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")

```

