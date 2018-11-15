---
---
title: "BAHCS-10 Prelim Results"
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
library(paran)
library(caret)
```


Load data.  Just get the actual data for now don't worry about sub group analyses.  
Add a state ID variable so we can differential them later on
```{r, include=FALSE}
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
dim(CIL_CKY)
head(CIL_CKY)

```
Now let us see how much data is misisng 
```{r}
CIL_CKYComplete = na.omit(CIL_CKY)
dim(CIL_CKYComplete)
dim(CIL_CKY)
CIL_CKYDemo = CIL_CKY

describe.factor(CIL_CKYDemo$StateID)
describe.factor(CIL_CKYDemo$Age, decr.order = FALSE)
```
Let us get some descriptives and get rid of those who are under 18
```{r}
# Get rid of client ID don't need it any more
#CIL_CKYDemo$SourceClient_ID = NULL
CIL_CKYDemo$Last.Service.Date = NULL
CIL_CKYDemo$Source.System = NULL
CIL_CKYDemo$Data.Warehouse.Client.ID = NULL
CIL_CKYDemo$Ethnicity = NULL
demoCIL_CKY = apply(CIL_CKYDemo, 2, function(x){describe.factor(x)})

describe.factor(CIL_CKYDemo$StateID)
```
Get full data set and clean it
```{r}
CIL_CKYFull = CIL_CKY
CIL_CKYFull = CIL_CKYFull[c(1:37)]
SourceClient_ID =  CIL_CKYFull$SourceClient_ID
CIL_CKYFull$SourceClient_ID = NULL
CIL_CKYFull = apply(CIL_CKYFull, 2, function(x){ifelse(x > 5, NA, ifelse(x < 1, NA, x))})
CIL_CKYFull = data.frame(CIL_CKYFull)
CIL_CKYFull$SourceClient_ID = SourceClient_ID

```
Get psychometrics for full data set for each construct
First finish cleaning for the data set
```{r}
CIL_CKYFull = data.frame(CIL_CKYFull)
CIL_CKYFullUniqueComplete = na.omit(CIL_CKYFull[!duplicated(CIL_CKYFull$SourceClient_ID), ])
dim(CIL_CKYFullUniqueComplete)
CIL_CKYEachCon = CIL_CKYFullUniqueComplete

### Now get each construct

General_Health = CIL_CKYEachCon[c(1:4)]

Mental_Health = CIL_CKYEachCon[c(5:7)]

Formal_Healthcare_Support = CIL_CKYEachCon[c(8:12)]

Medications = CIL_CKYEachCon[c(13:15)]

Nutrition = CIL_CKYEachCon[c(16:19)]

Home_Environment = CIL_CKYEachCon[c(20:23)]

Neighborhood_Environment = CIL_CKYEachCon[c(24:26)]

Relationship_Social_Support = CIL_CKYEachCon[c(27:29)]

Fin_Independence = CIL_CKYEachCon[c(30:32)]

Career_Education = CIL_CKYEachCon[c(33:34)]

```
Omega, Parrallel, MAP, EFA, CFA

Omega first
```{r}
describe(General_Health)
General_Health_Omega = omega(General_Health)
summary(General_Health_Omega)

describe(Mental_Health)
Mental_Health_Omega = omega(Mental_Health)
summary(Mental_Health_Omega)

describe(Formal_Healthcare_Support)
Formal_Healthcare_Support_Omega = omega(Formal_Healthcare_Support)
summary(Formal_Healthcare_Support_Omega)

describe(Medications)
Medications_Omega = omega(Medications)
summary(Medications_Omega)

describe(Medications)
Medications_Omega = omega(Medications)
summary(Medications_Omega)

describe(Nutrition)
Nutrition_Omega = omega(Nutrition)
summary(Nutrition_Omega)

describe(Home_Environment)
Home_Environment_Omega = omega(Home_Environment)
summary(Home_Environment_Omega)

describe(Neighborhood_Environment)
Neighborhood_Environment_Omega = omega(Neighborhood_Environment)
summary(Neighborhood_Environment_Omega)

describe(Relationship_Social_Support)
Relationship_Social_Support_Omega = omega(Relationship_Social_Support)
summary(Relationship_Social_Support_Omega)


describe(Fin_Independence)
Fin_Independence_Omega = omega(Fin_Independence)
summary(Fin_Independence_Omega)


describe(Career_Education)
cronbach.alpha(Career_Education)

```
Parrell, map, EFA
```{r}
HCSList= list(General_Health, Mental_Health, Formal_Healthcare_Support, Medications, Nutrition, Home_Environment, Neighborhood_Environment, Relationship_Social_Support, Fin_Independence, Career_Education)

HCSList[[2]]


HCSListParOutput = NULL
for(i in 1:length(HCSList)){
 HCSListParOutput[[i]] = paran(HCSList[[i]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
}

### Map test

HCSListMap= list(General_Health, Mental_Health, Formal_Healthcare_Support, Medications, Nutrition, Home_Environment, Neighborhood_Environment, Relationship_Social_Support, Fin_Independence)

HCSListMapOutput = NULL
for(i in 1:length(HCSListMap)){
  HCSListMapOutput[[i]] = vss(HCSListMap[[i]], n = 3, rotate = "oblimin", fm = "mle", cor = "poly")
}

### EFA 2 factors

HCSListEFA2Output = NULL
for(i in 1:length(HCSListMap)){
 HCSListEFA2Output[[i]]  = fa(r = HCSListMap[[i]], nfactors = 2, fm = "gls", cor = "poly")
}

for(i in 1:length(HCSListEFA2Output)){
  fa.diagram(HCSListEFA2Output[[i]])
}

### EFA 1 factor
HCSListEFA1Output = NULL
for(i in 1:length(HCSListMap)){
 HCSListEFA1Output[[i]]  = fa(r = HCSListMap[[i]], nfactors = 1, fm = "gls", cor = "poly")
}

for(i in 1:length(HCSListEFA1Output)){
  fa.diagram(HCSListEFA1Output[[i]])
}

### Compare EFA results

EFA12AnovaResults = NULL
for(i in 1:length(HCSListEFA1Output)){
  EFA12AnovaResults[[i]] = anova(HCSListEFA2Output[[i]],HCSListEFA1Output[[i]])
}
EFA12AnovaResults


```
Now one big CFA
```{r}
model10 = '
General_Health =~ good_health+ manage_health+ knows_conditions+ phys_activity 

Mental_Health =~ manage_mhealth+ has_goals+ not_overwhelmed

Formal_Healthcare_Support =~ has_pcp+ similar_goals+ health_literacy+ no_future_hosp+ no_ED_use

Medications =~ knows_meds+ takes_meds+ no_concern_side_effects

Nutrition =~ can_cook+ access_nut_food+ has_money_food+ eats_nut_food

Home_Environment =~ health_friendly_home+ accessible_home+ living_sit_satisfaction+ has_home

Neighborhood_Enviornment =~ safe_neighborhood+ near_supports+ has_transport

Relationships =~ others_support_health+ social_activity+ no_one_opposes

Finanice_Indep =~ has_money_for_family+ manage_money+ has_money_for_health

Career_Education =~ ed_level_satisfaction + job_satisfaction
'

fit10 = cfa(model10, estimator = "MLR", data = CIL_CKYEachCon)
summary(fit10, fit.measures = TRUE, standardized = TRUE)



modelGeneral_Health = 'General_Health =~ good_health+ manage_health+ knows_conditions+ phys_activity '
fit_General_Health =cfa(modelGeneral_Health, estimator = "MLR", data = CIL_CKYEachCon)

summary(fit_General_Health, fit.measures = TRUE, standardized = TRUE)


model_Mental_Health  = 'Mental_Health =~ manage_mhealth+ has_goals+ not_overwhelmed'
fit_Mental_Health =cfa(model_Mental_Health , estimator = "MLR", data = CIL_CKYEachCon)
summary(fit_Mental_Health, fit.measures = TRUE, standardized = TRUE)


model_Formal_Healthcare_Support  = 'Formal_Healthcare_Support =~ has_pcp+ similar_goals+ health_literacy+ no_future_hosp+ no_ED_use'
fit_Formal_Healthcare_Support =cfa(model_Formal_Healthcare_Support , estimator = "MLR", data = CIL_CKYEachCon)
summary(fit_Formal_Healthcare_Support, fit.measures = TRUE, standardized = TRUE)


model_Medications  = 'Medications =~ knows_meds+ takes_meds+ no_concern_side_effects'
fit_Medications =cfa(model_Medications , estimator = "MLR", data = CIL_CKYEachCon)
summary(fit_Medications, fit.measures = TRUE, standardized = TRUE)

model_Home_Environment  = 'Home_Environment =~ health_friendly_home+ accessible_home+ living_sit_satisfaction+ has_home'
fit_Home_Environment =cfa(model_Home_Environment , estimator = "MLR", data = CIL_CKYEachCon)
summary(fit_Home_Environment, fit.measures = TRUE, standardized = TRUE)

model_Relationships  = 'Relationships =~ others_support_health+ social_activity+ no_one_opposes'
fit_Relationships =cfa(model_Relationships , estimator = "MLR", data = CIL_CKYEachCon)
summary(fit_Relationships, fit.measures = TRUE, standardized = TRUE)

model_Relationships  = 'Finanice_Indep =~ has_money_for_family+ manage_money+ has_money_for_health'
fit_Relationships =cfa(model_Relationships , estimator = "MLR", data = CIL_CKYEachCon)
summary(fit_Relationships, fit.measures = TRUE, standardized = TRUE)

model_Finanice_Indep  = 'Finanice_Indep =~ has_money_for_family+ manage_money+ has_money_for_health'
fit_Finanice_Indep =cfa(model_Finanice_Indep , estimator = "MLR", data = CIL_CKYEachCon)
summary(fit_Finanice_Indep, fit.measures = TRUE, standardized = TRUE)

```




There is a rouge 0 and 7.  Probably should get rid of those.
```{r, include=FALSE}
CIL_CKY = cbind(CIL_CKY[c("manage_health" , "manage_mhealth" ,"similar_goals" ,"no_concern_side_effects" ,"has_money_food" ,"health_friendly_home" , "has_transport" , "social_activity" , "has_money_for_health" , "ed_level_satisfaction")])

descriptivesCIL_CKY = apply(CIL_CKY, 2, function(x){describe.factor(x)})
descriptivesCIL_CKY
CIL_CKY = apply(CIL_CKY, 2, function(x){ifelse(x > 5, NA, ifelse(x < 1, NA, x))})
CIL_CKY = data.frame(CIL_CKY)
describe.factor(CIL_CKY$similar_goals)
describe.factor(CIL_CKY$has_money_for_health)
describe(CIL_CKY)

dim(CIL_CKY)
dim(CIL_CKYDemo)
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
Get the unique number of people in the sample
```{r}

CIL_CKYFullUniqueComplete = na.omit(CIL_CKYFull[!duplicated(CIL_CKYFull$SourceClient_ID), ])
dim(CIL_CKYFullUniqueComplete)
CIL_CKYDemoUniqueComplete = na.omit(CIL_CKYDemo[!duplicated(CIL_CKYDemo$SourceClient_ID), ])

BAHCS_10_Items = CIL_CKYDemoUniqueComplete[c("manage_health", "manage_mhealth", "similar_goals", "no_concern_side_effects", "has_money_food", "health_friendly_home", "has_transport", "social_activity" , "has_money_for_health", "ed_level_satisfaction")]

BAHCS_10_Items = data.frame(apply(BAHCS_10_Items, 2, function(x){ifelse(x > 5, NA, ifelse(x < 1, NA, x))}))

   
BAHCS_10_Demo = data.frame(BAHCS_10_Items, Age = CIL_CKYDemoUniqueComplete$Age, Race = CIL_CKYDemoUniqueComplete$Race, Gender = CIL_CKYDemoUniqueComplete$Gender, StateID = CIL_CKYDemoUniqueComplete$StateID, SourceClient_ID = CIL_CKYDemoUniqueComplete$SourceClient_ID)

BAHCS_10_Items = na.omit(BAHCS_10_Items)
BAHCS_10_Demo = na.omit(BAHCS_10_Demo)
dim(BAHCS_10_Demo)

```
Get descriptives
```{r}
BAHCS_10_Demo$StateID = as.factor(BAHCS_10_Demo$StateID)
describe(BAHCS_10_Demo)
data.frame(apply(BAHCS_10_Demo, 2, sd))
```


EFA with all items
Try to see if a large EFA and CFA makes sense
EFA too messy with this many dimensions
```{r}
efa3 = fa(r = BAHCS_10_Items, nfactors = 3, fm = "gls", cor = "poly")
efa3
fa.diagram(efa3)

efa2 = fa(r = BAHCS_10_Items, nfactors = 2, fm = "gls", cor = "poly")
efa2
fa.diagram(efa2)

efa1 = fa(r = BAHCS_10_Items, nfactors = 1, fm = "gls", cor = "poly")
efa1
fa.diagram(efa1)

anova(efa3,efa1)

# now try VSS
vss(BAHCS_10_Items, n = 3, rotate = "oblimin", fm = "mle", cor = "poly")

# now try paran

paran(BAHCS_10_Items, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
```
Final CFA model
```{r}
model1  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit1 = cfa(model1, estimator = "MLR", data = BAHCS_10_Items)
summary(fit1, fit.measures = TRUE, standardized = TRUE)
```



Reliabiltiy
```{r}
omegaItems = omega(BAHCS_10_Items); summary(omegaItems)
omegaItems
```

Graded response model with and without missing data
Constrained means the discrimination parametr is equal
```{r}
# With missing data
# Graded response model
describe(BAHCS_10_Items)
fitOrdGRM = grm(data = BAHCS_10_Items, constrained = FALSE)
fitOrdGRMCon = grm(data = BAHCS_10_Items, constrained = TRUE)
anova(fitOrdGRMCon, fitOrdGRM)

summary(fitOrdGRM)

information(fitOrdGRM, c(-3, 1))

plot(fitOrdGRM, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 2, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

vals <- plot(fitOrdGRM, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 

plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")

```
DIFF for items

Race reference: white
Gender refernce: male
State reference: Illinois
Only 74 observations from Kentucky who we were able to match with demographics, so that is why 
```{r}


BAHCS_10_Demo$OtherRace  = as.factor(ifelse(BAHCS_10_Demo$Race == "WHITE OR CAUCASIAN", 0, 1))

BAHCS_10_Demo$Female  = as.factor(ifelse(BAHCS_10_Demo$Gender == "MALE", 0, 1))


write.csv(BAHCS_10_Demo, "BAHCS_10_Demo.csv", row.names = FALSE)

BAHCS_10_Demo = read.csv("BAHCS_10_Demo.csv", header = TRUE)



raceDIF = lordif(resp.data = BAHCS_10_Items, group = BAHCS_10_Demo$OtherRace, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(raceDIF)
plot(raceDIF)

genderDIF = lordif(resp.data = BAHCS_10_Items, group = BAHCS_10_Demo$Female, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(genderDIF)
plot(genderDIF)

stateDIF = lordif(resp.data = CIL_CKY, group = CIL_CKYDemo$StateID, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(stateDIF)
plot(stateDIF)

```
See what is going on with item 6
Ok almost all the data is from Illinois
Looks like there is a lot of missing data for Kentucky.  This is resulting in not much data to work with (about 74 people.)
```{r}
dim(CIL_CKYDemo)
CIL_Check = subset(CIL_CKYDemo, StateID == "0")

describe.factor(CIL_Check$health_friendly_home)

CIL_Check = subset(CIL_CKYDemo, StateID == "1")

describe.factor(CKY_Check$health_friendly_home)

describe(CKY_Check)


dim(CIL_Check)
CIL_Check = na.omit(CIL_Check)

```
####################
Predictive validity
####################
Create total score for BAHCS-10
```{r}
BAHCS_10_DemoPred = BAHCS_10_Demo
BAHCS_10_DemoPred = BAHCS_10_DemoPred[c(1:10)]
head(BAHCS_10_DemoPred)
BAHCS_10_DemoPred = rowSums(BAHCS_10_DemoPred, na.rm = TRUE)
head(BAHCS_10_DemoPred)
BAHCS_10_DemoPred = data.frame(BAHCS_10TotalScore = BAHCS_10_DemoPred, AvatarClient_ID = BAHCS_10_Demo$SourceClient_ID)
head(BAHCS_10_DemoPred)
```
Tobac south
```{r}
head(CIL_South_HCS_tobacco_use)
describe(CIL_South_HCS_tobacco_use)
CIL_SouthTobac = CIL_South_HCS_tobacco_use[c("LegacyClientID", "FollowUpTimePoint", "UsesTobacco_IND")]
CIL_SouthTobac = subset(CIL_SouthTobac, FollowUpTimePoint == "Intake" | FollowUpTimePoint == "")
describe.factor(CIL_SouthTobac$FollowUpTimePoint)
CIL_SouthTobac$FollowUpTimePoint = NULL
names(CIL_SouthTobac)[1] = "AvatarClient_ID"


CIL_WestTobac = CIL_West_HCS_tobacco_use[c("AvatarClient_ID", "FollowUpTimePoint", "UsesTobacco_IND")]
CIL_WestTobac = subset(CIL_WestTobac, FollowUpTimePoint == "Intake" | FollowUpTimePoint == "")
describe.factor(CIL_WestTobac$FollowUpTimePoint)
CIL_WestTobac$FollowUpTimePoint = NULL
names(CIL_WestTobac)[1] = "AvatarClient_ID"


CKYTobac = CKY_HCS_tobacco_use[c("AvatarClient_ID", "FollowUpTimePoint", "UsesTobacco_IND")]
CKYTobac = subset(CKYTobac, FollowUpTimePoint == "Intake" | FollowUpTimePoint == "")
describe.factor(CKYTobac$FollowUpTimePoint)
CKYTobac$FollowUpTimePoint = NULL
names(CKYTobac)[1] = "AvatarClient_ID"

CIL_Kentucky_Tobac = rbind(CIL_SouthTobac, CIL_WestTobac, CKYTobac)

BAHCS_10_DemoPred_Tobac = merge(BAHCS_10_DemoPred, CIL_Kentucky_Tobac, by = "AvatarClient_ID", all.x = TRUE)
dim(BAHCS_10_DemoPred_Tobac)
describe(BAHCS_10_DemoPred_Tobac)
```


Vitals all
```{r}
head(CIL_South_HCS_vitals)

CIL_South_vitals = CIL_South_HCS_vitals[c("AvatarClient_ID", "BMI", "Timepoints_Vitals")]
describe.factor(CIL_South_vitals$Timepoints_Vitals)
CIL_South_vitals = subset(CIL_South_vitals, Timepoints_Vitals == "Intake" | Timepoints_Vitals == "")
dim(CIL_South_vitals)
CIL_South_vitals$Timepoints_Vitals = NULL

CIL_West_vitals = CIL_West_HCS_vitals[c("AvatarClient_ID", "BMI", "Timepoints_Vitals")]
describe.factor(CIL_West_vitals$Timepoints_Vitals)
CIL_West_vitals = subset(CIL_West_vitals, Timepoints_Vitals == "Intake" | Timepoints_Vitals == "")
dim(CIL_West_vitals)
CIL_West_vitals$Timepoints_Vitals = NULL

CIL_Kentucky_vitals = CKY_HCS_Vitals[c("AvatarClient_ID", "BMI", "Timepoints_Vitals")]
describe.factor(CIL_Kentucky_vitals$Timepoints_Vitals)
CIL_Kentucky_vitals = subset(CIL_Kentucky_vitals, Timepoints_Vitals == "Intake" | Timepoints_Vitals == "")
dim(CIL_Kentucky_vitals)
CIL_Kentucky_vitals$Timepoints_Vitals = NULL

CIL_CKY_Vitals = rbind(CIL_South_vitals, CIL_West_vitals, CIL_Kentucky_vitals)
describe(CIL_CKY_Vitals)

BAHCS_10_DemoPredVitals = merge(BAHCS_10_DemoPred, CIL_CKY_Vitals, by = "AvatarClient_ID", all.x = TRUE)
describe(BAHCS_10_DemoPredVitals)
```
Correlations with BMI and Tobacco Use and BAHCS-10
```{r}
library(Hmisc)
rcorr(BAHCS_10_DemoPredVitals$BAHCS_10TotalScore,BAHCS_10_DemoPredVitals$BMI, type="pearson")
BAHCS_10_DemoPredVitals

library(ltm)
biserial.cor(BAHCS_10_DemoPred_Tobac$BAHCS_10TotalScore,BAHCS_10_DemoPred_Tobac$UsesTobacco_IND, use = "complete.obs")

```
CIL_PHQ9 Now try getting PHQ9 scores
```{r}
head(CIL_South_HCS_PHQ9)
CIL_South_PHQ9 = CIL_South_HCS_PHQ9[c("LegacyClientID", "FollowUpTimePoint", "Total_PHQ9")]
names(CIL_South_PHQ9)[1] = "AvatarClient_ID"
CIL_South_PHQ9 = subset(CIL_South_PHQ9, FollowUpTimePoint == "Intake" | FollowUpTimePoint == "")

CIL_West_PHQ9 = CIL_West_HCS_PHQ9[c("LegacyClientID", "FollowUpTimePoint", "Total_PHQ9")]
names(CIL_West_PHQ9)[1] = "AvatarClient_ID"
CIL_West_PHQ9 = subset(CIL_West_PHQ9, FollowUpTimePoint == "Intake" | FollowUpTimePoint == "")
describe.factor(CIL_West_PHQ9$FollowUpTimePoint)


CIL_Kentucky_PHQ9 = CKY_HCS_PHQ9[c("AvatarClient_ID", "FollowUpTimePoint", "Total_PHQ9")]
CIL_Kentucky_PHQ9 = subset(CIL_Kentucky_PHQ9, FollowUpTimePoint == "Intake" | FollowUpTimePoint == "")

CIL_CKY_PHQ9 = rbind(CIL_South_PHQ9, CIL_West_PHQ9, CIL_Kentucky_PHQ9)

BAHCS_10_DemoPredPHQ9 = merge(BAHCS_10_DemoPred, CIL_CKY_PHQ9, by = "AvatarClient_ID", all.x = TRUE)
describe(BAHCS_10_DemoPredPHQ9)

rcorr(BAHCS_10_DemoPredPHQ9$BAHCS_10TotalScore, BAHCS_10_DemoPredPHQ9$Total_PHQ9, type = "pearson")

```
