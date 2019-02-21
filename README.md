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
dim(CIL_CKY)
CIL_CKY_Demo = merge(CIL_CKY, CIL_CKY_Demo, by = "SourceClient_ID", all.x = TRUE)
dim(CIL_CKY_Demo)
```
Now let us see how much data is misisng 

Getting rid of missing data that is 93 or higher, because that is planned missing data.

```{r}
apply(CIL_CKY_Demo, 2, function(col)sum(is.na(col))/length(col))


missing_data_person = apply(CIL_CKY_Demo, 1, function(row)sum(is.na(row))/length(row))
describe.factor(missing_data_person)

CIL_CKY_Demo$missing_data_person = missing_data_person

CIL_CKY_Demo = subset(CIL_CKY_Demo, missing_data_person < .9)
dim(CIL_CKY_Demo)
sum(is.na(CIL_CKY_Demo))
```
Clean up data 
```{r}
# Get rid of client ID don't need it any more
#CIL_CKY_Demo$SourceClient_ID = NULL
CIL_CKY_Demo$Last.Service.Date = NULL
CIL_CKY_Demo$Source.System = NULL
CIL_CKY_Demo$Data.Warehouse.Client.ID = NULL
CIL_CKY_Demo$Ethnicity = NULL


CIL_CKY_Demo$phys_activity = ifelse(CIL_CKY_Demo$phys_activity > 5, NA, ifelse(CIL_CKY_Demo$phys_activity < 1, NA, CIL_CKY_Demo$phys_activity))
describe.factor(CIL_CKY_Demo$phys_activity)

CIL_CKY_Demo$similar_goals = ifelse(CIL_CKY_Demo$similar_goals > 5, NA, ifelse(CIL_CKY_Demo$similar_goals < 1, NA, CIL_CKY_Demo$similar_goals))
describe.factor(CIL_CKY_Demo$similar_goals)

CIL_CKY_Demo$health_literacy = ifelse(CIL_CKY_Demo$health_literacy > 5, NA, ifelse(CIL_CKY_Demo$health_literacy < 1, NA, CIL_CKY_Demo$health_literacy))
describe.factor(CIL_CKY_Demo$health_literacy)

CIL_CKY_Demo$no_ED_use = ifelse(CIL_CKY_Demo$no_ED_use > 5, NA, ifelse(CIL_CKY_Demo$no_ED_use < 1, NA, CIL_CKY_Demo$no_ED_use))
describe.factor(CIL_CKY_Demo$no_ED_use)

#dropping can cook most people messed up
CIL_CKY_Demo$can_cook = NULL

CIL_CKY_Demo$has_money_for_health = ifelse(CIL_CKY_Demo$has_money_for_health > 5, NA, ifelse(CIL_CKY_Demo$has_money_for_health < 1, NA, CIL_CKY_Demo$has_money_for_health))
describe.factor(CIL_CKY_Demo$has_money_for_health)


CIL_CKY_Demo$job_satisfaction = ifelse(CIL_CKY_Demo $job_satisfaction > 5, NA, ifelse(CIL_CKY_Demo $job_satisfaction < 1, NA, CIL_CKY_Demo $job_satisfaction))
describe.factor(CIL_CKY_Demo $job_satisfaction)

CIL_CKY_Demo$able_to_not_smoke = ifelse(CIL_CKY_Demo$able_to_not_smoke > 5, NA, ifelse(CIL_CKY_Demo$able_to_not_smoke < 1, NA, CIL_CKY_Demo$able_to_not_smoke))
describe.factor(CIL_CKY_Demo$able_to_not_smoke)

CIL_CKY_Demo$able_to_not_use = ifelse(CIL_CKY_Demo$able_to_not_use > 5, NA, ifelse(CIL_CKY_Demo$able_to_not_use < 1, NA, CIL_CKY_Demo$able_to_not_use))
describe.factor(CIL_CKY_Demo$able_to_not_use)

CIL_CKY_Demo$similar_goals = ifelse(CIL_CKY_Demo$similar_goals < 1, NA, CIL_CKY_Demo$similar_goals)


##3 now get rid of additional missing values
dim(CIL_CKY_Demo)


```
Get the unique number of people in the sample
```{r}

CIL_CKYFullUnique = CIL_CKY_Demo[!duplicated(CIL_CKY_Demo$SourceClient_ID), ]
dim(CIL_CKYFullUnique)

BAHCS_10_Items = CIL_CKYFullUnique[c("manage_health", "manage_mhealth", "similar_goals", "no_concern_side_effects", "has_money_food", "health_friendly_home", "has_transport", "social_activity" , "has_money_for_health", "ed_level_satisfaction")]

   
BAHCS_10_Demo = data.frame(BAHCS_10_Items, Age = CIL_CKYFullUnique$Age, Race = CIL_CKYFullUnique$Race, Gender = CIL_CKYFullUnique$Gender, StateID = CIL_CKYFullUnique$StateID, SourceClient_ID = CIL_CKYFullUnique$SourceClient_ID)

dim(BAHCS_10_Demo)
sum(is.na(BAHCS_10_Demo))
```
Get the percentage of missing data with all < .9 excluded
```{r}
apply(BAHCS_10_Demo, 2, function(col)sum(is.na(col))/length(col))
missing_data_person = apply(CIL_CKY_Demo, 1, function(row)sum(is.na(row))/length(row))
describe.factor(missing_data_person)

BAHCS_10_Demo_Complete = na.omit(BAHCS_10_Demo)
dim(BAHCS_10_Demo_Complete)[1]/dim(BAHCS_10_Demo)[1]

```



Get descriptives
```{r}
BAHCS_10_Demo$StateID = as.factor(BAHCS_10_Demo$StateID)
describe(BAHCS_10_Demo)
data.frame(apply(BAHCS_10_Demo, 2, sd, na.rm = TRUE))
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

BAHCS_10_Items_Complete = na.omit(BAHCS_10_Items)

paran(BAHCS_10_Items_Complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
```
Final CFA model
```{r}
model1  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit1 = cfa(model1, estimator = "MLR", missing = "ML", data = BAHCS_10_Items)
summary(fit1, fit.measures = TRUE, standardized = TRUE)

```
Model without item ten
```{r}
model_no_10  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health'

fit_no_10 = cfa(model_no_10, estimator = "MLR", missing = "ML", data = BAHCS_10_Items)
summary(fit_no_10, fit.measures = TRUE, standardized = TRUE)
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
dim(BAHCS_10_Items)

fitOrdGRM = grm(data = BAHCS_10_Items, constrained = FALSE)
fitOrdGRMCon = grm(data = BAHCS_10_Items, constrained = TRUE)
anova(fitOrdGRMCon, fitOrdGRM)

summary(fitOrdGRM)

information(fitOrdGRM, c(-3, 1), items = c(10))

plot(fitOrdGRM, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 2, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

vals <- plot(fitOrdGRM, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 

sem = plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")

info = plot(vals[, "z"], vals[, "test.info"], type = "l", lwd = 2, xlab = "Ability", ylab = "Test Information", main = "Test Information")

fsc = factor.scores(fitOrdGRM)
plot(fsc, include.items =TRUE)

```
Get the margins
Observed is just the number of people who responded in category
Expected the (row total * column total) / n 

What the output gives us is the total residual.  So the total residual across all categories for items one and two is 65.91
```{r}
margins2 = margins(fitOrdGRM, "two")
margins2$margins
test
margins3 = margins(fitOrdGRM, "three")
margins3
```
Item 10 is not so good
```{r}
## Item ten

plot(fitOrdGRM, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1, items = c(10))

plot(fitOrdGRM, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1, items = c(10))

plot(fitOrdGRM, category = 2, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1, items = c(10))

plot(fitOrdGRM, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1, items = c(10))
```
Get the range for which the BACHS-10 is most accurate
Ok so total score does not exactly match up with ability level
```{r}
sum(is.na(BAHCS_10_Items))
fsc = factor.scores(fitOrdGRM)
dat_score =  fsc$score.dat
Total_Score = rowSums(dat_score[,1:10])

dat_score = data.frame(Total_Score, Ability = fsc$score.dat$z1)

dat_score = dat_score[order(dat_score$Ability),]

dat_score_complete = na.omit(dat_score)

tail(dat_score_complete, 90)

```
DIFF for items

Race reference: white
Gender refernce: male
State reference: Illinois
Only 74 observations from Kentucky who we were able to match with demographics, so that is why 
```{r}


BAHCS_10_Demo$OtherRace  = as.factor(ifelse(BAHCS_10_Demo$Race == "WHITE OR CAUCASIAN", 0, 1))

BAHCS_10_Demo$Female  = as.factor(ifelse(BAHCS_10_Demo$Gender == "MALE", 0, 1))

BAHCS_10_Items
dim(BAHCS_10_Demo)
dim(BAHCS_10_Items)
BAHCS_10_Items_DIF = BAHCS_10_Items
BAHCS_10_Items_DIF$TotalScore = NULL

raceDIF = lordif(resp.data = BAHCS_10_Items_DIF, group = BAHCS_10_Demo$OtherRace, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(raceDIF)
plot(raceDIF)

genderDIF = lordif(resp.data = BAHCS_10_Items_DIF, group = BAHCS_10_Demo$Female, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(genderDIF)
plot(genderDIF)

stateDIF = lordif(resp.data = BAHCS_10_Items_DIF, group = BAHCS_10_Demo$StateID, criterion = "Chisqr", alpha = .01, minCell = 5)
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
library(ltm)
biserial.cor(BAHCS_10_DemoPred_Tobac$BAHCS_10TotalScore,BAHCS_10_DemoPred_Tobac$UsesTobacco_IND, use = "complete.obs")

BAHCS_10_DemoPred_Tobac$Test = rnorm(mean = 100, sd = 2,n = dim(BAHCS_10_DemoPred_Tobac)[1])

library(psych)
biserial(BAHCS_10_DemoPred_Tobac$BAHCS_10TotalScore,BAHCS_10_DemoPred_Tobac$UsesTobacco_IND)
cor.test(BAHCS_10_DemoPred_Tobac$BAHCS_10TotalScore,BAHCS_10_DemoPred_Tobac$UsesTobacco_IND)

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
