library(readr)
library(dplyr)
library(tidyverse)


### Preprocess Members table
df_members <- read.csv("/Users/stevenzhou/Desktop/PBHLTH 244/Proj1data/Members.csv") #, show_col_types = FALSE)
df_members$AgeAtFirstClaim <- as.factor(df_members$AgeAtFirstClaim)
df_members$MemberID <- as.character(df_members$MemberID)
df_members$Sex <- as.factor(df_members$Sex)
names(which(colSums(is.na(df_members)) > 0)) # impute age, sex


### Preprocess Claims table
df_claims <- read.csv("/Users/stevenzhou/Desktop/PBHLTH 244/Proj1data/Claims.csv") #, show_col_types = FALSE)
names(which(colSums(is.na(df_claims)) > 0))

# missing values: sensitivity analysis, missing value proportions
# Specialty: encode missing as "Unknown"
# PlaceSvc: encode missing as "Unknown"
# PrimaryConditionGroup: encode missing as "Unknown"
# ProcedureGroup: encode missing as "Unknown"
df_claims$Specialty[is.na(df_claims$Specialty)] <- "Unknown"
df_claims$PlaceSvc[is.na(df_claims$PlaceSvc)] <- "Unknown"
df_claims$PrimaryConditionGroup[is.na(df_claims$PrimaryConditionGroup)] <- "Unknown"
df_claims$ProcedureGroup[is.na(df_claims$ProcedureGroup)] <- "Unknown"

df_claims$Year <- as.factor(df_claims$Year)
df_claims$MemberID <- as.character(df_claims$MemberID)
df_claims$Specialty <- as.factor(df_claims$Specialty)
df_claims$PlaceSvc <- as.factor(df_claims$PlaceSvc)
df_claims$DSFS <- as.factor(df_claims$DSFS)
df_claims$PrimaryConditionGroup <- as.factor(df_claims$PrimaryConditionGroup)
df_claims$CharlsonIndex <- as.factor(df_claims$CharlsonIndex)
df_claims$ProcedureGroup <- as.factor(df_claims$ProcedureGroup)

# recode PayDelay
df_claims$PayDelay[df_claims$PayDelay == "162+"] <- "162"
df_claims$PayDelay <- as.numeric(df_claims$PayDelay)

# drop columns
df_claims <- df_claims %>% 
  select(-c(ProviderID, Vendor, PCP, LengthOfStay, SupLOS, DSFS))

# Y1, Y2, Y3
df_claims_y1 <- filter(df_claims, Year == "Y1")
df_claims_y2 <- filter(df_claims, Year == "Y2")
df_claims_y3 <- filter(df_claims, Year == "Y3")
rm(df_claims)

claims_num_y1 <- as.data.frame(table(df_claims_y1$MemberID))
colnames(claims_num_y1) <- c("MemberID", "NumClaims")
claims_num_y1$MemberID <- as.character(claims_num_y1$MemberID)

claims_num_y2 <- as.data.frame(table(df_claims_y2$MemberID))
colnames(claims_num_y2) <- c("MemberID", "NumClaims")
claims_num_y2$MemberID <- as.character(claims_num_y2$MemberID)

# Y1
df_claims_y1 <- df_claims_y1 %>% 
  add_count(MemberID, Specialty) %>% 
  group_by(MemberID) %>% 
  mutate(Specialty = Specialty[n == max(n)][1]) %>%
  select(-n)

df_claims_y1 <- df_claims_y1 %>% 
  add_count(MemberID, PlaceSvc) %>% 
  group_by(MemberID) %>% 
  mutate(PlaceSvc = PlaceSvc[n == max(n)][1]) %>%
  select(-n)

df_claims_y1 <- df_claims_y1 %>% 
  add_count(MemberID, PrimaryConditionGroup) %>% 
  group_by(MemberID) %>% 
  mutate(PrimaryConditionGroup = PrimaryConditionGroup[n == max(n)][1]) %>%
  select(-n)

df_claims_y1 <- df_claims_y1 %>% 
  add_count(MemberID, CharlsonIndex) %>% 
  group_by(MemberID) %>% 
  mutate(CharlsonIndex = CharlsonIndex[n == max(n)][1]) %>%
  select(-n)

df_claims_y1 <- df_claims_y1 %>% 
  add_count(MemberID, ProcedureGroup) %>% 
  group_by(MemberID) %>% 
  mutate(ProcedureGroup = ProcedureGroup[n == max(n)][1]) %>%
  select(-n)

df_claims_y1 <- df_claims_y1 %>% 
  group_by(MemberID) %>% 
  mutate(PayDelayMean = mean(PayDelay)) %>% 
  select(-PayDelay) %>% 
  distinct()

# Y2
df_claims_y2 <- df_claims_y2 %>% 
  add_count(MemberID, Specialty) %>% 
  group_by(MemberID) %>% 
  mutate(Specialty = Specialty[n == max(n)][1]) %>%
  select(-n)

df_claims_y2 <- df_claims_y2 %>% 
  add_count(MemberID, PlaceSvc) %>% 
  group_by(MemberID) %>% 
  mutate(PlaceSvc = PlaceSvc[n == max(n)][1]) %>%
  select(-n)

df_claims_y2 <- df_claims_y2 %>% 
  add_count(MemberID, PrimaryConditionGroup) %>% 
  group_by(MemberID) %>% 
  mutate(PrimaryConditionGroup = PrimaryConditionGroup[n == max(n)][1]) %>%
  select(-n)

df_claims_y2 <- df_claims_y2 %>% 
  add_count(MemberID, CharlsonIndex) %>% 
  group_by(MemberID) %>% 
  mutate(CharlsonIndex = CharlsonIndex[n == max(n)][1]) %>%
  select(-n)

df_claims_y2 <- df_claims_y2 %>% 
  add_count(MemberID, ProcedureGroup) %>% 
  group_by(MemberID) %>% 
  mutate(ProcedureGroup = ProcedureGroup[n == max(n)][1]) %>%
  select(-n)

df_claims_y2 <- df_claims_y2 %>% 
  group_by(MemberID) %>% 
  mutate(PayDelayMean = mean(PayDelay)) %>% 
  select(-PayDelay) %>% 
  distinct()


### Preprocess DrugCount table
df_drugcount <- read.csv("/Users/stevenzhou/Desktop/PBHLTH 244/Proj1data/DrugCount.csv") #, show_col_types = FALSE)
names(which(colSums(is.na(df_drugcount)) > 0))

df_drugcount$Year <- as.factor(df_drugcount$Year)
df_drugcount$MemberID <- as.character(df_drugcount$MemberID)
df_drugcount <- select(df_drugcount, -DSFS)

# recode drugcount
df_drugcount$DrugCount[df_drugcount$DrugCount == "7+"] <- "7"
df_drugcount$DrugCount <- as.numeric(df_drugcount$DrugCount)

# Y1, Y2, Y3
df_drugcount_y1 <- filter(df_drugcount, Year == "Y1")
df_drugcount_y2 <- filter(df_drugcount, Year == "Y2")
df_drugcount_y3 <- filter(df_drugcount, Year == "Y3")
rm(df_drugcount)

df_drugcount_y1 <- df_drugcount_y1 %>% 
  group_by(MemberID) %>% 
  summarize(TotalDrugs = sum(DrugCount))

df_drugcount_y2 <- df_drugcount_y2 %>% 
  group_by(MemberID) %>% 
  summarize(TotalDrugs = sum(DrugCount))

df_drugcount_y3 <- df_drugcount_y3 %>% 
  group_by(MemberID) %>% 
  summarize(TotalDrugs = sum(DrugCount))


### Preprocess LabCount table
df_labcount <- read.csv("/Users/stevenzhou/Desktop/PBHLTH 244/Proj1data/LabCount.csv") #, show_col_types = FALSE)
names(which(colSums(is.na(df_labcount)) > 0))

df_labcount$Year <- as.factor(df_labcount$Year)
df_labcount$MemberID <- as.character(df_labcount$MemberID)
df_labcount <- select(df_labcount, -DSFS)

# recode labcount
df_labcount$LabCount[df_labcount$LabCount == "10+"] <- "10"
df_labcount$LabCount <- as.numeric(df_labcount$LabCount)

# Y1, Y2, Y3
df_labcount_y1 <- filter(df_labcount, Year == "Y1")
df_labcount_y2 <- filter(df_labcount, Year == "Y2")
df_labcount_y3 <- filter(df_labcount, Year == "Y3")
rm(df_labcount)

df_labcount_y1 <- df_labcount_y1 %>% 
  group_by(MemberID) %>% 
  summarize(TotalLabs = sum(LabCount))

df_labcount_y2 <- df_labcount_y2 %>% 
  group_by(MemberID) %>% 
  summarize(TotalLabs = sum(LabCount))

df_labcount_y3 <- df_labcount_y3 %>% 
  group_by(MemberID) %>% 
  summarize(TotalLabs = sum(LabCount))


### Preprocess Outcome table
df_y2 <- read.csv("/Users/stevenzhou/Desktop/PBHLTH 244/Proj1data/DaysInHospital_Y2.csv") #, show_col_types = FALSE)
names(which(colSums(is.na(df_y2)) > 0))

df_y2$DaysInHospital <- as.numeric(df_y2$DaysInHospital)
df_y2$MemberID <- as.character(df_y2$MemberID)
df_y2 <- select(df_y2, -ClaimsTruncated)

df_y3 <- read.csv("/Users/stevenzhou/Desktop/PBHLTH 244/Proj1data/DaysInHospital_Y3.csv") #, show_col_types = FALSE)
names(which(colSums(is.na(df_y3)) > 0))

df_y3$DaysInHospital <- as.numeric(df_y3$DaysInHospital)
df_y3$MemberID <- as.character(df_y3$MemberID)
df_y3 <- select(df_y3, -ClaimsTruncated)


### Join
# members with claims in Y1
df_train_y1 <- df_members %>% 
  left_join(claims_num_y1, by = "MemberID") %>% 
  left_join(df_claims_y1, by = "MemberID") %>% 
  left_join(df_labcount_y1, by = "MemberID") %>% 
  left_join(df_drugcount_y1, by = "MemberID") %>% 
  left_join(df_y2, by = "MemberID")

df_train_y1$NumClaims[is.na(df_train_y1$NumClaims)] <- 0
df_train_y1$TotalLabs[is.na(df_train_y1$TotalLabs)] <- 0
df_train_y1$TotalDrugs[is.na(df_train_y1$TotalDrugs)] <- 0

df_train_y1 <- drop_na(df_train_y1, Year)


# members with claims in Y2
df_train_y2 <- df_members %>% 
  left_join(claims_num_y2, by = "MemberID") %>% 
  left_join(df_claims_y2, by = "MemberID") %>% 
  left_join(df_labcount_y2, by = "MemberID") %>% 
  left_join(df_drugcount_y2, by = "MemberID") %>% 
  left_join(df_y3, by = "MemberID")

df_train_y2$NumClaims[is.na(df_train_y2$NumClaims)] <- 0
df_train_y2$TotalLabs[is.na(df_train_y2$TotalLabs)] <- 0
df_train_y2$TotalDrugs[is.na(df_train_y2$TotalDrugs)] <- 0

df_train_y2 <- drop_na(df_train_y2, Year)

rm(claims_num_y1)
rm(claims_num_y2)
rm(df_claims_y1)
rm(df_claims_y2)
rm(df_claims_y3)
rm(df_drugcount_y1)
rm(df_drugcount_y2)
rm(df_drugcount_y3)
rm(df_labcount_y1)
rm(df_labcount_y2)
rm(df_labcount_y3)
rm(df_members)

library(randomForest)
### Predict whether patients go to hospital or not
df_train_y1_new1 = df_train_y1[, -c(1, 5)]
df_train_y1_new1[df_train_y1_new1$DaysInHospital != 0, ]$DaysInHospital = 1
df_train_y1_new1$DaysInHospital = as.factor(df_train_y1_new1$DaysInHospital)

# tuning parameters
nt = c(400, 450, 500, 550, 600) #number of trees
Error_Rate = rep(0, length(nt))
for (i in 1:length(nt)){
  dat_train = df_train_y1_new1[1:60000, ]
  dat_valid = df_train_y1_new1[60001:dim(df_train_y1)[1], ]
  mod = randomForest(DaysInHospital~., data = dat_train, ntree = nt[i])
  mod_predict = predict(mod, newdata = dat_valid[, -12])
  error_rate = sum(mod_predict != dat_valid[, 12])/dim(dat_valid)[1]
  Error_Rate[i] = error_rate
}

# train binary classifier 
final_classifier = randomForest(DaysInHospital~., data = df_train_y1_new1, importance = TRUE, ntree = 400)
final_classifier$importance

# ROC and AUC
jpeg(file="/Users/stevenzhou/Desktop/PBHLTH 244/roc.jpeg")
library(pROC)
final_classifier_predict = data.frame(predict(final_classifier, newdata = df_train_y1_new1, type = "prob"))
final_classifier_roc = roc(df_train_y1_new1$DaysInHospital, final_classifier_predict[, 2])
auc(final_classifier_roc)
plot(final_classifier_roc)
dev.off()

### Predict how long they are gonna stay in the hospital 
df_train_y1_new2 = df_train_y1[df_train_y1$DaysInHospital != 0, -c(1, 5)]

# tuning parameters
nt = c(400, 450, 500, 550, 600) #number of trees
MSE = rep(0, length(nt))
for (i in 1:length(nt)){
  dat_train = df_train_y1_new2[1:9415, ]
  dat_valid = df_train_y1_new2[9416:dim(df_train_y1_new2)[1], ]
  mod = randomForest(DaysInHospital~., data = dat_train, ntree = nt[i])
  mod_predict = predict(mod, newdata = dat_valid[, -12])
  mse = sum((mod_predict-dat_valid[, 12])^2)/dim(dat_valid)[1]
  MSE[i] = mse
}

# train regression
final_regression = randomForest(DaysInHospital~., data = df_train_y1_new2, importance = TRUE, ntree = 550)
final_regression$importance

### testing
result = rep(0, dim(df_train_y2)[1])

# classification
result = predict(final_classifier, newdata = df_train_y2[, -c(1, 5, 14)])

# regression
reg_result = round(predict(final_regression, newdata = df_train_y2[result != 0, -c(1, 5, 14)]))

# final result
result = as.numeric(result) - 1
result[result != 0] = reg_result
sum((result-df_train_y2[, 14])^2)/dim(df_train_y2)[1]

### Sensitivity Analysis
df_train_y1 = df_train_y1[rowSums(df_train_y1 == "")==0, ]

### Predict whether patients go to hospital or not
df_train_y1_new1 = df_train_y1[, -c(1, 5)]
df_train_y1_new1[df_train_y1_new1$DaysInHospital != 0, ]$DaysInHospital = 1
df_train_y1_new1$DaysInHospital = as.factor(df_train_y1_new1$DaysInHospital)

# tuning parameters
nt = c(400, 450, 500, 550, 600) #number of trees
Error_Rate = rep(0, length(nt))
for (i in 1:length(nt)){
  dat_train = df_train_y1_new1[1:47928, ]
  dat_valid = df_train_y1_new1[47929:dim(df_train_y1)[1], ]
  mod = randomForest(DaysInHospital~., data = dat_train, ntree = nt[i])
  mod_predict = predict(mod, newdata = dat_valid[, -12])
  error_rate = sum(mod_predict != dat_valid[, 12])/dim(dat_valid)[1]
  Error_Rate[i] = error_rate
}

# train binary classifier 
final_classifier = randomForest(DaysInHospital~., data = df_train_y1_new1, importance = TRUE, ntree = 600)
final_classifier$importance
