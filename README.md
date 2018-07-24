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
```


Load data.  Just get the actual data for now don't worry about sub group analyses.  
```{r}
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale/7-13-18HCSData")
#CIL = read.csv("CIL HCS Dataset_07172018.csv", header = TRUE)
#CKY = read.csv("CKY HCS Dataset_07172018.csv", header = TRUE)
head(CIL)
CIL = cbind(CIL[c("good_health", "manage_health", "knows_conditions", "phys_activity", "manage_mhealth", "has_goals", "not_overwhelmed", "has_pcp", "similar_goals", "health_literacy", "no_future_hosp", "no_ED_use", "knows_meds", "takes_meds", "no_concern_side_effects", "can_cook", "access_nut_food", "has_money_food", "eats_nut_food", "health_friendly_home", "accessible_home", "living_sit_satisfaction", "has_home", "safe_neighborhood", "near_supports", "has_transport", "others_support_health", "social_activity", "no_one_opposes", "has_money_for_family", "manage_money", "has_money_for_health", "ed_level_satisfaction", "job_satisfaction", "able_to_not_smoke", "able_to_not_use")])
head(CILTest)

CKY = cbind(CKY[c("good_health", "manage_health", "knows_conditions", "phys_activity", "manage_mhealth", "has_goals", "not_overwhelmed", "has_pcp", "similar_goals", "health_literacy", "no_future_hosp", "no_ED_use", "knows_meds", "takes_meds", "no_concern_side_effects", "can_cook", "access_nut_food", "has_money_food", "eats_nut_food", "health_friendly_home", "accessible_home", "living_sit_satisfaction", "has_home", "safe_neighborhood", "near_supports", "has_transport", "others_support_health", "social_activity", "no_one_opposes", "has_money_for_family", "manage_money", "has_money_for_health", "ed_level_satisfaction", "job_satisfaction", "able_to_not_smoke", "able_to_not_use")])

CIL_CKY = data.frame(rbind(CIL, CKY))
write.csv(CIL_CKY, "CIL_CKY.csv", row.names = FALSE)
CIL_CKY = read.csv("CIL_CKY.csv", header = TRUE)
head(CIL_CKY)
dim(CIL_CKY)
CIL_CKY$can_cook = as.integer(CIL_CKY$can_cook)

CIL_CKY_Complete = na.omit(CIL_CKY)
dim(CIL_CKY_Complete)

```
Reliabiltiy
```{r}
omegaItems = omega(CIL_CKY); summary(omegaItems)
```
Parrell analyses
```{r}
parallel = fa.parallel(CIL_CKY, fa= "fa")
parallel$fa.values
```
Exploratory Factor Analysis
```{r}
unrotated4 <- efaUnrotate(CIL_CKY, nf=4, estimator="mlr", missing = "ML")
summary(unrotated4, std=TRUE)
inspect(unrotated4, "std")

unrotated1 <- efaUnrotate(CIL_CKY, nf=1, estimator="mlr", missing = "ML")

anova(unrotated1, unrotated4)
```
CFA Full
```{r}
model1 = 'HCA =~ good_health+ manage_health+ knows_conditions+ phys_activity+ manage_mhealth+ has_goals+ not_overwhelmed+ has_pcp+ similar_goals+ health_literacy+ no_future_hosp+ no_ED_use+ knows_meds+ takes_meds+ no_concern_side_effects+ can_cook+ access_nut_food+ has_money_food+ eats_nut_food+ health_friendly_home+ accessible_home+ living_sit_satisfaction+ has_home+ safe_neighborhood+ near_supports+ has_transport+ others_support_health+ social_activity+ no_one_opposes+ has_money_for_family+ manage_money+ has_money_for_health+ ed_level_satisfaction+ job_satisfaction'
fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit1, fit.measures = TRUE)
```

CFA
```{r}
model1 = 'HCA =~ manage_health+ manage_mhealth+ no_future_hosp+no_concern_side_effects+ can_cook+ has_home+ safe_neighborhood+ has_money_for_family+has_money_for_health+ job_satisfaction'
fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit1, fit.measures = TRUE)

model2  ='HCA =~ manage_health + manage_mhealth +no_ED_use +knows_meds +has_money_food +health_friendly_home + near_supports + social_activity + has_money_for_health + ed_level_satisfaction'

fit2 = cfa(model2, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit2, fit.measures = TRUE)


model3 = 'HCA =~ manage_health + manage_mhealth +no_ED_use +knows_meds +has_money_food +health_friendly_home + near_supports + social_activity + has_money_for_health + job_satisfaction'

fit3 = cfa(model3, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit3, fit.measures = TRUE)


model4  ='HCA =~ manage_health + manage_mhealth +has_pcp +knows_meds +has_money_food +health_friendly_home + near_supports + social_activity + has_money_for_health + ed_level_satisfaction'

fit4 = cfa(model4, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit4, fit.measures = TRUE)


model5  ='HCA =~ manage_health + manage_mhealth +has_pcp +no_concern_side_effects +has_money_food +health_friendly_home + near_supports + social_activity + has_money_for_health + ed_level_satisfaction'

fit5 = cfa(model5, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit5, fit.measures = TRUE)


model6  ='HCA =~ manage_health + manage_mhealth +has_pcp +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit6 = cfa(model6, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit6, fit.measures = TRUE)


model7  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit7 = cfa(model7, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit7, fit.measures = TRUE)



model8  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + others_support_health + social_activity + has_money_for_health + ed_level_satisfaction'

fit8 = cfa(model8, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit8, fit.measures = TRUE)


#
model9  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + no_one_opposes + has_money_for_health + ed_level_satisfaction'

fit9 = cfa(model9, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit9, fit.measures = TRUE)


model7  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit7 = cfa(model7, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit7, fit.measures = TRUE)



```
Final CFA model
```{r}
model7  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit7 = cfa(model7, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKY)
summary(fit7, fit.measures = TRUE)


fit7Complete = cfa(model7, estimator = "MLR", std.lv = TRUE, data = CIL_CKY)
summary(fit7Complete, fit.measures = TRUE)

```
Reliabiltiy
```{r}

CIL_CKYItems = cbind(CIL_CKY[c("manage_health" , "manage_mhealth" ,"similar_goals" ,"no_concern_side_effects" ,"has_money_food" ,"health_friendly_home" , "has_transport" , "social_activity" , "has_money_for_health" , "ed_level_satisfaction")])

omegaItems = omega(CIL_CKYItems); summary(omegaItems)
```
Parrell analyses
```{r}
parallel = fa.parallel(CIL_CKYItems, fa= "fa")
parallel$fa.values
```

