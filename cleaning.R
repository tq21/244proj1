library(readr)
library(dplyr)
library(tidyverse)

### Preprocess Members table
df_members <- read_csv("data/Members.csv", show_col_types = FALSE)
df_members$AgeAtFirstClaim[is.na(df_members$AgeAtFirstClaim)] <- "U"
df_members$Sex[is.na(df_members$Sex)] <- "U"
df_members$AgeAtFirstClaim <- as.factor(df_members$AgeAtFirstClaim)
df_members$MemberID <- as.character(df_members$MemberID)
df_members$Sex <- as.factor(df_members$Sex)
names(which(colSums(is.na(df_members)) > 0)) # impute age, sex


### Preprocess Claims table
df_claims <- read_csv("data/Claims.csv", show_col_types = FALSE)
names(which(colSums(is.na(df_claims)) > 0))

# missing values: sensitivity analysis, missing value proportions
# Specialty: encode missing as "U"
# PlaceSvc: encode missing as "U"
# PrimaryConditionGroup: encode missing as "U"
# ProcedureGroup: encode missing as "U"
df_claims$Specialty[is.na(df_claims$Specialty)] <- "U"
df_claims$PlaceSvc[is.na(df_claims$PlaceSvc)] <- "U"
df_claims$PrimaryConditionGroup[is.na(df_claims$PrimaryConditionGroup)] <- "U"
df_claims$ProcedureGroup[is.na(df_claims$ProcedureGroup)] <- "U"

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
df_drugcount <- read_csv("data/DrugCount.csv", show_col_types = FALSE)
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
df_labcount <- read_csv("data/LabCount.csv", show_col_types = FALSE)
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
df_y2 <- read_csv("data/DaysInHospital_Y2.csv", show_col_types = FALSE)
names(which(colSums(is.na(df_y2)) > 0))

df_y2$DaysInHospital <- as.numeric(df_y2$DaysInHospital)
df_y2$MemberID <- as.character(df_y2$MemberID)
df_y2 <- select(df_y2, -ClaimsTruncated)

df_y3 <- read_csv("data/DaysInHospital_Y3.csv", show_col_types = FALSE)
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

df_train_no_missing <- df_train_y1 %>% filter(across(everything(), ~ . != "U"))
df_test_no_missing <- df_train_y2 %>% filter(across(everything(), ~ . != "U"))

summary(df_train_y1$PrimaryConditionGroup)
summary(df_train_no_missing$PrimaryConditionGroup)

tmp <- df_train_y1 %>% filter(PrimaryConditionGroup == "AMI",
                              AgeAtFirstClaim == "50-59" |
                              AgeAtFirstClaim == "60-69" |
                              AgeAtFirstClaim == "70-79")
tmp <- tmp %>% mutate(above4 = ifelse(TotalDrugs > 4, 1, 0))
tmp$above4 <- as.factor(tmp$above4)
tmp <- tmp %>% select(-c(MemberID, Year))

# median 4 drugs

# predict propensity scores
ps_mod <- glm(above4 ~ .,
              family = binomial(link = "logit"), 
              data = tmp %>% select(-c(PrimaryConditionGroup,
                                       DaysInHospital,
                                       TotalDrugs)))
ps <- as.vector(predict(ps_mod, type = "response"))

#ps <- ifelse(ps >= 0.5, 1, 0)
#sum(ps == tmp$above4) / nrow(tmp)

# assign weight
weight <- ifelse(tmp$above4 == 1, 1/ps, 1/(1-ps))
weighteddata <- svydesign(ids = ~ 1, data = tmp, weights = ~ weight)
xvars = names(tmp)[-c(6,11,12,13)]
weightedtable <- svyCreateTableOne(vars = xvars, strata = "above4",
                                   data = weighteddata, test = FALSE)

treat <- as.numeric(tmp$above4) - 1
Y <- tmp$DaysInHospital
est <- mean(treat*Y/ps)/mean(treat/ps) - 
  mean((1-treat)*Y/(1-ps))/mean((1-treat)/(1-ps))

B <- 1000
B_est <- rep(NA, B)
set.seed(100)
for (i in 1:B) {
  B_tmp <- tmp[sample(nrow(tmp), nrow(tmp), replace = TRUE),]
  ps_mod <- glm(above4 ~ .,
                family = binomial(link = "logit"), 
                data = B_tmp %>% select(-c(PrimaryConditionGroup,
                                         DaysInHospital,
                                         TotalDrugs)))
  ps <- as.vector(predict(ps_mod, type = "response"))
  ps <- pmax(0.05, pmin(0.95, ps))
  treat <- as.numeric(B_tmp$above4) - 1
  Y <- B_tmp$DaysInHospital
  B_est[i] <- mean(treat*Y/ps)/mean(treat/ps) - 
    mean((1-treat)*Y/(1-ps))/mean((1-treat)/(1-ps))
}

quantile(B_est, probs = c(0.025, 0.975))

lm.obj <- lm(DaysInHospital ~ above4, data = tmp, weights = weight)






