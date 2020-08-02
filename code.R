library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(lubridate)
library(gridExtra)
library(caret)
library(xlsx)
library(tree)
library(rpart)
library(rpart.plot)
library(party)
library(lift)
library(gains)
library(modelplotr) # Cumulative lift
library(rpart.utils)
library(irr)

library(MASS) # AIC
library(Hmisc) # imputation
library(DMwR) # kNN
library(randomForest)

library(connectR)

options(max.print=999999)


# Load data
options(java.parameters = "-Xmx8g")
con <- src_connectR(drv = TJDBC(),
                    url = "jdbc:teradata://dbccop1/")
data <- tbl(con, "nd_be_better_output") %>% collect()
data <- db_get_query(con, "select * from nd_be_better_output")

## The customers with a target_ind = 1 are the customers that had opt'd out but are now opt'd back in again (100,475)
## The customers with a target_ind = 0 are the customers that are still opt'd out (301,425)
## The base is a paired sample based on a 3 to 1 propensity (401,900)
## Here is a description of how I arrived at the base:
## Took all new customers that were opt'd out at the start of each week over the period Jan 2018 - Dec 2018
## Identified who had opt'd back in each week and included all of those customers in the modelling sample (Target_ind = 1)
## Took a random sample of the non-converted accounts in each week (Target = 0) and added them to the base table 
## The sample volume of the non-converted accounts was 3 times the volume of converts so we end up with 25% of the total base showing as having converted (3 non-targets to 1 target)


# Save the original data into excel
write.xlsx(oddt, "//homedrivestmp/homedisk1/emar719/Desktop/SD/data.xlsx")


# Data exploration


## Data separation depending on opt-in/out
optin <- data %>% filter(target_ind==1)
optout <- data %>% filter(target_ind==0)

## Opt-in vs.Opt-out
base <- data %>% 
  mutate(target_ind=ifelse(target_ind==0, "OptOut","OptIn")) %>% 
  group_by(tenure_months, target_ind) %>% 
  summarise(optnum=n())

## Opt-in month variable change - not considered
optmonth <- optin %>% mutate(model_end_dt=as.Date(model_end_dt, format="%Y-%m-%d")) %>%
  mutate(model_end_dt=ymd(model_end_dt)) %>% 
  mutate_at(vars(model_end_dt), funs(year, month)) 
optmonth <- optmonth %>% mutate(month=paste(optmonth$year, optmonth$month, ep="-")) %>% 
  select(-c(year)) %>% mutate(month=str_replace(month, "2018-1", "18-1")) %>% 
  mutate(month=str_replace(month, "2018-2", "18-2")) %>% 
  mutate(month=str_replace(month, "2018-3", "18-3")) %>% 
  mutate(month=str_replace(month, "2018-4", "18-4")) %>% 
  mutate(month=str_replace(month, "2018-5", "18-5")) %>% 
  mutate(month=str_replace(month, "2018-6", "18-6")) %>% 
  mutate(month=str_replace(month, "2018-7", "18-7")) %>% 
  mutate(month=str_replace(month, "2018-8", "18-8")) %>% 
  mutate(month=str_replace(month, "2018-9", "18-9")) %>% 
  mutate(month=str_replace(month, "2018-10", "18-10")) %>% 
  mutate(month=str_replace(month, "2018-11", "18-11")) %>% 
  mutate(month=str_replace(month, "2018-12", "18-12")) %>% 
  mutate(month=str_replace(month, "2019-1", "19-1")) %>% 
  mutate(month=str_replace(month, "2019-2", "19-2")) %>% 
  mutate(month=str_replace(month, "2019-3", "19-3")) %>% 
  mutate(month=str_replace(month, "2019-4", "19-4")) %>% 
  mutate(month=str_replace(month, "2019-5", "19-5")) %>% 
  mutate(month=str_replace(month, "2019-6", "19-6"))

## Opt-in by month - not considered
base1 <- base %>% filter(target_ind=="OptIn")

ggplot(base1) + 
  geom_line(aes(x=tenure_months, y=optnum)) + 
  geom_point(aes(x=tenure_months, y=optnum)) + 
  scale_x_discrete(limits=c(0:17)) + 
  labs(x="Tenure Month", y="The Number of Customer", title="Opt In Customers")

## Total/average/median gross demand by opt preference (mean)
dem <- data %>%
  select(target_ind, aov_pre_12m, median_ov_pre_12m, gdem_12m) %>%
  group_by(target_ind)

dem <- aggregate(dem, by=list(dem$target_ind), FUN=mean) %>%
  mutate(target_ind=ifelse(target_ind==0, "OptOut","OptIn"))

ttdm <- data %>%
  select(target_ind, gdem_12m) %>%
  group_by(target_ind)

ttdm <- aggregate(ttdm, by=list(ttdm$target_ind), FUN=sum) %>%
  mutate(target_ind=ifelse(target_ind==0, "OptOut","OptIn"))

## Total/average/median gross demand by month - not considered
aov <- merge(aggregate(.~tenure_months, data=arrange(data.table(select(optin, tenure_months, aov_pre_12m)), tenure_months), sum, na.rm=TRUE), 
             aggregate(.~tenure_months, data=arrange(data.table(select(optin, tenure_months, aov_post_12m)), tenure_months), sum, na.rm=TRUE), by="tenure_months")
med <- merge(aggregate(.~tenure_months, data=arrange(data.table(select(optin, tenure_months, median_ov_pre_12m)), tenure_months), sum, na.rm=TRUE), 
             aggregate(.~tenure_months, data=arrange(data.table(select(optin, tenure_months, median_ov_post_12m)), tenure_months), sum, na.rm=TRUE), by="tenure_months")
grdm <- merge(merge(aov, med, by="tenure_months"), merge(aggregate(.~tenure_months, data=arrange(data.table(select(optin, tenure_months, gdem_12m)), tenure_months), sum, na.rm=TRUE), 
              aggregate(.~tenure_months, data=arrange(data.table(select(optin, tenure_months, cadem_12m)), tenure_months), sum, na.rm=TRUE), by="tenure_months"), by="tenure_months")

grid.arrange(ggplot(data=grdm, aes(x=tenure_months)) + 
             geom_bar(aes(y=gdem_12m), stat = "identity") + 
             geom_line(aes(y=aov_pre_12m, colour="average")) + 
             geom_line(aes(y=median_ov_pre_12m, colour="median")) + 
             scale_x_discrete(limits=c(0:17)) + labs(x="The Months Needed to Opt In", y="Total Gross Pre Credit Demand", colour="Parameter"),
             ggplot(data=grdm, aes(x=tenure_months)) + 
             geom_bar(aes(y=cadem_12m), stat = "identity") + 
             geom_line(aes(y=aov_post_12m, colour="average")) + 
             geom_line(aes(y=median_ov_post_12m, colour="median")) + 
             scale_x_discrete(limits=c(0:17)) + 
             labs(x="The Months Needed to Opt In", y="Total Gross Post Credit Demand", colour="Parameter"))

## Gross demand by department (opt preference) 
pre = arrange(data.table(select(data, 
                                target_ind, 
                                ladies_dem_pre_12m, 
                                mens_dem_pre_12m, 
                                kids_dem_pre_12m, 
                                foot_dem_pre_12m,
                                elec_dem_pre_12m, 
                                furn_dem_pre_12m, 
                                home_dem_pre_12m, 
                                seas_dem_pre_12m)), 
              target_ind)
pre1 <- aggregate(.~target_ind, data=pre, mean, na.rm=TRUE)

pre <- aggregate(.~target_ind, data=pre, sum, na.rm=TRUE)
pre <- pre %>% mutate(Total=rowSums(select(pre, -target_ind)))

## Gross demand by department (Tenure) - not considered
pre = arrange(data.table(select(optin, 
                                tenure_months, 
                                ladies_dem_pre_12m, 
                                mens_dem_pre_12m, 
                                kids_dem_pre_12m, 
                                foot_dem_pre_12m,
                                elec_dem_pre_12m, 
                                furn_dem_pre_12m, 
                                home_dem_pre_12m, 
                                seas_dem_pre_12m)), 
              tenure_months)
pre <- aggregate(.~tenure_months, data=pre, sum, na.rm=TRUE)
pre <- pre %>% mutate(Total=rowSums(select(pre, -tenure_months)))
post = arrange(data.table(select(optin, tenure_months, 
                                 ladies_dem_post_12m, 
                                 mens_dem_post_12m, 
                                 kids_dem_post_12m, 
                                 foot_dem_post_12m, 
                                 elec_dem_post_12m, 
                                 furn_dem_post_12m, 
                                 home_dem_post_12m, 
                                 seas_dem_post_12m)), 
               tenure_months)
post <- aggregate(.~tenure_months, data=post, sum, na.rm=TRUE)
post <- post %>% mutate(Total=rowSums(select(post, -tenure_months)))

## Gross demand by department (Conversion month) - not considered
pre1 = data.table(select(optmonth, 
                         month, 
                         ladies_dem_pre_12m, 
                         mens_dem_pre_12m, 
                         kids_dem_pre_12m, 
                         foot_dem_pre_12m, 
                         elec_dem_pre_12m, 
                         furn_dem_pre_12m, 
                         home_dem_pre_12m, 
                         seas_dem_pre_12m))
pre1 <- aggregate(.~month, data=pre1, sum, na.rm=TRUE)
pre1 <- pre1 %>% mutate(Total=rowSums(select(pre1, -month)))
post1 = data.table(select(optmonth, 
                          month, 
                          ladies_dem_post_12m, 
                          mens_dem_post_12m, 
                          kids_dem_post_12m, 
                          foot_dem_post_12m, 
                          elec_dem_post_12m, 
                          furn_dem_post_12m, 
                          home_dem_post_12m, 
                          seas_dem_post_12m))
post1 <- aggregate(.~month, data=post1, sum, na.rm=TRUE)
post1 <- post1 %>% mutate(Total=rowSums(select(post1, -month)))    

## Gross demand by gender (preference)
data %>% 
  select(gender_code) %>%
  group_by(gender_code) %>% 
  summarise(n=n())

gend <- data %>% 
        select(gender_code, target_ind) %>% 
        group_by(gender_code, target_ind) %>% 
        summarise(total=n()) %>% 
        mutate(target_ind=ifelse(target_ind==0, "Opt Out", "Opt In"))

gend[c("gender_code")] <- lapply(gend[c("gender_code")], factor)
gend <- gend %>% 
        mutate(gender_code=ifelse(gender_code=="F", "Female", ifelse(gender_code=="M", "Male", "Unknown")))

ggplot(gend) + 
  geom_bar(aes(x=target_ind, y=total, fill=gender_code), stat="identity", position="dodge") + 
  labs(x="Preference", y="The Number of Customers", title="Preference by Gender", fill="Gender")

ggplot(gend) + 
  geom_bar(aes(x=gender_code, y=total, fill=target_ind), stat="identity", position="dodge") + 
  labs(x="Gender", y="The Number of Customers", title="Preference by Gender", fill="Preference")

## Gross demand by price (preference)
prc <- data %>% 
  select(full_price_dem_pre_12m,
         promotional_dem_pre_12m, 
         clearance_dem_pre_12m,
         target_ind) %>%
  group_by(target_ind)

full <- prc %>% 
  summarise(total.full=sum(full_price_dem_pre_12m))

pro <- prc %>% 
  summarise(total.pro=sum(promotional_dem_pre_12m))

cle <- prc %>% 
  summarise(total.cle=sum(clearance_dem_pre_12m))

price <- merge(full, pro, by="target_ind")
price.fn <- merge(price, cle, by="target_ind")


# Model


## Logistic regression 1

### Remove columns that are not meaningful (principa_brand -> only VERY)
data %>% 
  dplyr::select(
    -customer_account_id, 
    -model_end_dt,
    -principal_brand
  ) %>% 
  mutate(
    gender_code=ifelse(gender_code=="F", 0, (ifelse(gender_code=="M", 1, 2)))) -> oddt

### Remove variables with standarad deviation 0 (all same values)
oddt <- Filter(function(x) sd(x) != 0, oddt)

### Missing values
colnames(oddt)[colSums(is.na(oddt)) > 0] # outstanding_balance, trading_credit_risk_factor, past_due_amt
oddt <- oddt %>% cbind(data$outstanding_balance) %>% 
  setnames("data$outstanding_balance", "outstanding_balance") # trading_credit_risk_factor/past_due_amt -> credit_set/scheduled_payments_past_due (used instead -> can be removed)

### Select only meaningful variables
oddt <- oddt %>% dplyr::select(gender_code, 
                               age, 
                               apr, 
                               avg_days_bet_ords_pre_12m, 
                               cash, 
                               bnpl_purchase_pre_12m, 
                               bnpl_settle_pre_12m, 
                               multichannel_pre_12m, 
                               elec_browse_12m, 
                               elec_ord_ndesps_m_12m, 
                               elec_ord_ndesps_u_12m, 
                               foot_browse_12m, 
                               foot_ord_ndesps_m_12m, 
                               foot_ord_ndesps_u_12m, 
                               furn_browse_12m, 
                               furn_ord_ndesps_m_12m, 
                               furn_ord_ndesps_u_12m, 
                               home_ord_ndesps_m_12m, 
                               home_browse_12m, 
                               home_ord_ndesps_u_12m, 
                               kids_browse_12m, 
                               kids_ord_ndesps_m_12m, 
                               kids_ord_ndesps_u_12m, 
                               ladies_browse_12m, 
                               ladies_ord_ndesps_m_12m, 
                               ladies_ord_ndesps_u_12m, 
                               mens_browse_12m, 
                               mens_ord_ndesps_m_12m, 
                               mens_ord_ndesps_u_12m, 
                               used_pounds_off_12m, 
                               off_ord_pre_12m, 
                               onl_ord_pre_12m, 
                               used_perc_off_12m, 
                               elec_ord_pre_12m, 
                               foot_ord_pre_12m, 
                               furn_ord_pre_12m, 
                               home_ord_pre_12m, 
                               kids_ord_pre_12m, 
                               ladies_ord_pre_12m, 
                               mens_ord_pre_12m, 
                               seas_ord_pre_12m, 
                               seas_browse_12m, 
                               seas_ord_ndesps_m_12m, 
                               seas_ord_ndesps_u_12m, 
                               used_voucher_12m, 
                               clearance_ord_pre_12m, 
                               full_price_ord_pre_12m, 
                               promotional_ord_pre_12m, 
                               credit_limit, 
                               credit_set, 
                               days_since_pre_12m, 
                               elec_dem_ndesps_u_12m, 
                               elec_dem_ndesps_m_12m, 
                               bnpl_eligible, 
                               foot_dem_ndesps_u_12m, 
                               foot_dem_ndesps_m_12m, 
                               furn_dem_ndesps_u_12m, 
                               furn_dem_ndesps_m_12m, 
                               elec_dem_gdesps_m_12m, 
                               foot_dem_gdesps_m_12m, 
                               furn_dem_gdesps_m_12m, 
                               home_dem_gdesps_m_12m, 
                               kids_dem_gdesps_m_12m, 
                               ladies_dem_gdesps_m_12m, 
                               mens_dem_gdesps_m_12m, 
                               seas_dem_gdesps_m_12m, 
                               home_dem_ndesps_u_12m, 
                               home_dem_ndesps_m_12m, 
                               elec_items_ndesps_u_12m, 
                               foot_items_ndesps_u_12m, 
                               furn_items_ndesps_u_12m, 
                               home_items_ndesps_u_12m, 
                               kids_items_ndesps_u_12m, 
                               ladies_items_ndesps_u_12m, 
                               mens_items_ndesps_u_12m, 
                               seas_items_ndesps_u_12m, 
                               kids_dem_ndesps_u_12m, 
                               kids_dem_ndesps_m_12m, 
                               ladies_dem_ndesps_u_12m, 
                               ladies_dem_ndesps_m_12m, 
                               mens_dem_ndesps_u_12m, 
                               mens_dem_ndesps_m_12m, 
                               cash_ords_pre_12m, 
                               rejects_12m, 
                               n_dis_pv_12m, 
                               days_since_12m, 
                               depts_pre_12m, 
                               direct_bnpl_ords_pre_12m, 
                               dir_revolving_ords_pre_12m, 
                               items_ndesps_u_12m, 
                               returned_items_unmatch_12m, 
                               no_pounds_off_12m, 
                               tenure_months, 
                               orders_ndesps_u_12m, 
                               no_perc_off_12m, 
                               items_pre_12m, 
                               orders_pre_12m, 
                               product_views_12m, 
                               elec_views_12m, 
                               foot_views_12m, 
                               furn_views_12m, 
                               home_views_12m, 
                               kids_views_12m, 
                               ladies_views_12m, 
                               mens_views_12m, 
                               seas_views_12m, 
                               visits_12m, 
                               no_vouchers_12m, 
                               outstanding_balance, 
                               aov_pre_12m, 
                               elec_dem_pre_12m, 
                               foot_dem_pre_12m, 
                               furn_dem_pre_12m, 
                               home_dem_pre_12m, 
                               kids_dem_pre_12m, 
                               ladies_dem_pre_12m, 
                               mens_dem_pre_12m, 
                               seas_dem_pre_12m, 
                               elec_items_pre_12m, 
                               foot_items_pre_12m, 
                               furn_items_pre_12m, 
                               home_items_pre_12m, 
                               kids_items_pre_12m, 
                               ladies_items_pre_12m, 
                               mens_items_pre_12m, 
                               seas_items_pre_12m, 
                               median_ov_pre_12m, 
                               recruitment_credit_risk_factor, 
                               scheduled_payments_past_due, 
                               seas_dem_ndesps_u_12m, 
                               seas_dem_ndesps_m_12m, 
                               avg_lag_12m, 
                               days_since_return_unmatch_12m, 
                               aban_amt_12m, 
                               cash_dem_pre_12m, 
                               clearance_dem_pre_12m, 
                               direct_bnpl_dem_pre_12m, 
                               dir_revolving_dem_pre_12m, 
                               full_price_dem_ndesps_u_12m, 
                               gdem_12m, 
                               gdesps_unmatch_12m, 
                               margin_unmatch_12m, 
                               ndesps_unmatch_12m, 
                               fs_contrib_unmatch_12m, 
                               total_contrib_unmatch_12m, 
                               retail_contrib_unmatch_12m, 
                               off_dem_pre_12m, 
                               onl_dem_pre_12m, 
                               full_price_dem_pre_12m, 
                               promotional_dem_pre_12m, 
                               returned_dem_unmatch_12m, 
                               pounds_off_amt_12m, 
                               perc_off_amt_12m, 
                               prdt_view_val_12m, 
                               vouchers_amt_12m, 
                               target_ind)

### Factors - not considered
forfactor <- c("gender_code", 
               "cash", 
               "bnpl_purchase_pre_12m", 
               "bnpl_settle_pre_12m", 
               "multichannel_pre_12m", 
               "elec_browse_12m", 
               "elec_ord_ndesps_m_12m", 
               "elec_ord_ndesps_u_12m", 
               "foot_browse_12m", 
               "foot_ord_ndesps_m_12m", 
               "foot_ord_ndesps_u_12m", 
               "furn_browse_12m", 
               "furn_ord_ndesps_m_12m", 
               "furn_ord_ndesps_u_12m", 
               "home_ord_ndesps_m_12m", 
               "home_browse_12m", 
               "home_ord_ndesps_u_12m", 
               "kids_browse_12m", 
               "kids_ord_ndesps_m_12m", 
               "kids_ord_ndesps_u_12m", 
               "ladies_browse_12m", 
               "ladies_ord_ndesps_m_12m", 
               "ladies_ord_ndesps_u_12m", 
               "mens_browse_12m", 
               "mens_ord_ndesps_m_12m", 
               "mens_ord_ndesps_u_12m", 
               "used_pounds_off_12m", 
               "off_ord_pre_12m", 
               "onl_ord_pre_12m", 
               "used_perc_off_12m", 
               "elec_ord_pre_12m", 
               "foot_ord_pre_12m", 
               "furn_ord_pre_12m", 
               "home_ord_pre_12m", 
               "kids_ord_pre_12m", 
               "ladies_ord_pre_12m", 
               "mens_ord_pre_12m", 
               "seas_ord_pre_12m", 
               "seas_browse_12m", 
               "seas_ord_ndesps_m_12m", 
               "seas_ord_ndesps_u_12m", 
               "used_voucher_12m", 
               "clearance_ord_pre_12m", 
               "full_price_ord_pre_12m", 
               "promotional_ord_pre_12m", 
               "bnpl_eligible", 
               "credit_set", 
               "target_ind")
oddt[forfactor] <- lapply(oddt[forfactor], factor)
sapply(oddt, class)
dput(names(oddt))

### Missing values (remove, impute, kNN)

oddt[!complete.cases(oddt),]

oddt[!complete.cases(oddt$outstanding_balance),] %>% select(cash, outstanding_balance)
oddt[!complete.cases(oddt$trading_credit_risk_factor),] %>% select(cash, trading_credit_risk_factor)
oddt[!complete.cases(oddt$past_due_amt),] %>% select(cash, past_due_amt)

#### Remove (listwise) - removed 334 rows containing missing values
oddt <- oddt %>% remove_missing()
oddt <- oddt[complete.cases(oddt), ]

#### Data partitioning after removal
set.seed(1234)
datapartition <- createDataPartition(oddt$target_ind, p=0.7, list=FALSE)
train=oddt[datapartition,]
test=oddt[-datapartition,]

#### Impute - not considered
oddtMN <- oddt
impute(oddtMN$target_ind, mean)  # replace with mean
oddtMD <- oddt
impute(oddtMN$target_ind, median)  # replace with median

#### kNN - not considered
oddtKNN <- knnImputation(oddt[, !names(oddt) %in% "medv"])
anyNA(oddtKNN)

#### Multiple imputation - not considered
imp <- mice(oddt, seed=1234)
fit <- with(imp, glm(target_ind~., data=oddt, family=binomial))
pooled <- pool(fit)
summary(pooled)

#### Leave one out cross validation (missing value) - not considered
train_control <- trainControl(method="LOOCV")
print(train(target_ind~gdem_12m, data=oddt, trControl=train_control, method="nb")) # original
print(train(target_ind~gdem_12m, data=oddtRL, trControl=train_control, method="nb")) # remove

### Variable selection (individual simple logistic regression)
summary(glm(target_ind~gender_code, family=binomial, data=train))
summary(glm(target_ind~age, family=binomial, data=train))
summary(glm(target_ind~apr, family=binomial, data=train))
summary(glm(target_ind~avg_days_bet_ords_pre_12m, family=binomial, data=train))
summary(glm(target_ind~cash, family=binomial, data=train))
summary(glm(target_ind~bnpl_purchase_pre_12m, family=binomial, data=train))
summary(glm(target_ind~bnpl_settle_pre_12m, family=binomial, data=train))
summary(glm(target_ind~multichannel_pre_12m, family=binomial, data=train))
summary(glm(target_ind~elec_browse_12m, family=binomial, data=train))
summary(glm(target_ind~elec_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~elec_ord_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~foot_browse_12m, family=binomial, data=train))
summary(glm(target_ind~foot_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~furn_browse_12m, family=binomial, data=train))
summary(glm(target_ind~furn_ord_ndesps_m_12m, family=binomial, data=train)) #0.438
summary(glm(target_ind~home_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~home_browse_12m, family=binomial, data=train))
summary(glm(target_ind~home_ord_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~kids_browse_12m, family=binomial, data=train))
summary(glm(target_ind~kids_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~kids_ord_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_browse_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_ord_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~mens_browse_12m, family=binomial, data=train))
summary(glm(target_ind~mens_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~mens_ord_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~used_pounds_off_12m, family=binomial, data=train))
summary(glm(target_ind~off_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~onl_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~used_perc_off_12m, family=binomial, data=train))
summary(glm(target_ind~elec_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~foot_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~furn_ord_pre_12m, family=binomial, data=train)) #0.562
summary(glm(target_ind~home_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~kids_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~mens_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~seas_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~seas_browse_12m, family=binomial, data=train))
summary(glm(target_ind~seas_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~seas_ord_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~used_voucher_12m, family=binomial, data=train))
summary(glm(target_ind~clearance_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~full_price_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~promotional_ord_pre_12m, family=binomial, data=train))
summary(glm(target_ind~credit_limit, family=binomial, data=train))
summary(glm(target_ind~credit_set, family=binomial, data=train))
summary(glm(target_ind~days_since_pre_12m, family=binomial, data=train))
summary(glm(target_ind~elec_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~elec_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~bnpl_eligible, family=binomial, data=train))
summary(glm(target_ind~foot_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~foot_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~furn_dem_ndesps_u_12m, family=binomial, data=train)) #0.68
summary(glm(target_ind~furn_dem_ndesps_m_12m, family=binomial, data=train)) #0.774
summary(glm(target_ind~elec_dem_gdesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~foot_dem_gdesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~furn_dem_gdesps_m_12m, family=binomial, data=train)) #0.536
summary(glm(target_ind~home_dem_gdesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~kids_dem_gdesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_dem_gdesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~mens_dem_gdesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~seas_dem_gdesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~home_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~home_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~elec_items_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~foot_items_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~furn_items_ndesps_u_12m, family=binomial, data=train)) #0.182
summary(glm(target_ind~home_items_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_items_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~mens_items_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~seas_items_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~kids_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~kids_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~mens_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~mens_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~cash_ords_pre_12m, family=binomial, data=train))
summary(glm(target_ind~rejects_12m, family=binomial, data=train))
summary(glm(target_ind~n_dis_pv_12m, family=binomial, data=train))
summary(glm(target_ind~days_since_12m, family=binomial, data=train))
summary(glm(target_ind~depts_pre_12m, family=binomial, data=train))
summary(glm(target_ind~direct_bnpl_ords_pre_12m, family=binomial, data=train))
summary(glm(target_ind~dir_revolving_ords_pre_12m, family=binomial, data=train))
summary(glm(target_ind~items_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~returned_items_unmatch_12m, family=binomial, data=train)) #0.127
summary(glm(target_ind~no_pounds_off_12m, family=binomial, data=train))
summary(glm(target_ind~tenure_months, family=binomial, data=train)) #0.651
summary(glm(target_ind~orders_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~no_perc_off_12m, family=binomial, data=train))
summary(glm(target_ind~items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~orders_pre_12m, family=binomial, data=train))
summary(glm(target_ind~product_views_12m, family=binomial, data=train))
summary(glm(target_ind~elec_views_12m, family=binomial, data=train))
summary(glm(target_ind~foot_views_12m, family=binomial, data=train))
summary(glm(target_ind~furn_views_12m, family=binomial, data=train))
summary(glm(target_ind~home_views_12m, family=binomial, data=train))

summary(glm(target_ind~kids_views_12m, family=binomial, data=train)) # glm.fit: fitted probabilities numerically 0 or 1 occurred
glm(target_ind~kids_views_12m, family=binomial(link="logit"), data=train)
glm(target_ind~kids_views_12m, family=binomial(link="logit"), data=train, control=glm.control(maxit=50)) #increase the number of fisher scoring
bayesglm(target_ind~kids_views_12m, family=binomial(link="logit"), data=train)
bayesglm(target_ind~kids_views_12m, family=binomial(link="logit"), data=train, control=glm.control(maxit=50))

summary(glm(target_ind~ladies_views_12m, family=binomial, data=train))
summary(glm(target_ind~mens_views_12m, family=binomial, data=train))
summary(glm(target_ind~seas_views_12m, family=binomial, data=train))
summary(glm(target_ind~visits_12m, family=binomial, data=train))
summary(glm(target_ind~no_vouchers_12m, family=binomial, data=train))
summary(glm(target_ind~outstanding_balance, family=binomial, data=train))
summary(glm(target_ind~aov_pre_12m, family=binomial, data=train)) #0.046
summary(glm(target_ind~elec_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~foot_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~furn_dem_pre_12m, family=binomial, data=train)) #0.0205
summary(glm(target_ind~home_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~kids_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~mens_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~seas_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~elec_items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~foot_items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~furn_items_pre_12m, family=binomial, data=train)) #0.0508
summary(glm(target_ind~home_items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~kids_items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~mens_items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~seas_items_pre_12m, family=binomial, data=train))
summary(glm(target_ind~median_ov_pre_12m, family=binomial, data=train)) #0.167
summary(glm(target_ind~recruitment_credit_risk_factor, family=binomial, data=train))
summary(glm(target_ind~scheduled_payments_past_due, family=binomial, data=train))
summary(glm(target_ind~seas_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~seas_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~avg_lag_12m, family=binomial, data=train))
summary(glm(target_ind~days_since_return_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~aban_amt_12m, family=binomial, data=train))
summary(glm(target_ind~cash_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~clearance_dem_pre_12m, family=binomial, data=train))

summary(glm(target_ind~direct_bnpl_dem_pre_12m, family=binomial, data=train)) # glm.fit: fitted probabilities numerically 0 or 1 occurred
oddt %>% dplyr::select(direct_bnpl_dem_pre_12m) %>% class()
levels(oddt$direct_bnpl_dem_pre_12m)
class(oddt$direct_bnpl_dem_pre_12m)

summary(glm(target_ind~full_price_dem_ndesps_u_12m, family=binomial, data=train))
summary(glm(target_ind~gdem_12m, family=binomial, data=train))
summary(glm(target_ind~gdesps_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~margin_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~ndesps_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~fs_contrib_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~total_contrib_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~retail_contrib_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~off_dem_pre_12m, family=binomial, data=train)) #0.226
summary(glm(target_ind~onl_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~full_price_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~promotional_dem_pre_12m, family=binomial, data=train))
summary(glm(target_ind~returned_dem_unmatch_12m, family=binomial, data=train))
summary(glm(target_ind~pounds_off_amt_12m, family=binomial, data=train))
summary(glm(target_ind~perc_off_amt_12m, family=binomial, data=train)) #0.495
summary(glm(target_ind~prdt_view_val_12m, family=binomial, data=train))
summary(glm(target_ind~vouchers_amt_12m, family=binomial, data=train))

### Select significant variables only
oddtb <- oddt %>% dplyr::select(-furn_ord_ndesps_m_12m,
                                -furn_ord_pre_12m,
                                -furn_dem_ndesps_u_12m,
                                -furn_dem_ndesps_m_12m,
                                -furn_dem_gdesps_m_12m,
                                -furn_items_ndesps_u_12m,
                                -returned_items_unmatch_12m,
                                -tenure_months,
                                -aov_pre_12m,
                                -furn_dem_pre_12m,
                                -furn_items_pre_12m,
                                -median_ov_pre_12m,
                                -off_dem_pre_12m,
                                -perc_off_amt_12m)
set.seed(1234)
datapartitionb <- createDataPartition(oddtb$target_ind, p=0.7, list=FALSE)
trainb=oddtb[datapartitionb,]
testb=oddtb[-datapartitionb,]

### Remove correlated variables with weaker p-value - not considered
cor(oddtb) # with categorical variable -> use aov
findCorrelation(cor(cor), cutoff=0.99)

distinct(data.table(dplyr::select(oddt, elec_ord_ndesps_u_12m, elec_ord_ndesps_m_12m))) # check if duplicated
summary(glm(target_ind~elec_ord_ndesps_m_12m, family=binomial, data=train))  # same p-value & AIC
summary(glm(target_ind~elec_ord_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~foot_ord_ndesps_m_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)
summary(glm(target_ind~foot_ord_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~home_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~home_ord_ndesps_u_12m, family=binomial, data=train)) # smaller p-value

summary(glm(target_ind~kids_ord_ndesps_m_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)
summary(glm(target_ind~kids_ord_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~ladies_ord_ndesps_m_12m, family=binomial, data=train)) # smaller p-value
summary(glm(target_ind~ladies_ord_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~mens_ord_ndesps_m_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)
summary(glm(target_ind~mens_ord_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~days_since_pre_12m, family=binomial, data=train))
summary(glm(target_ind~avg_days_bet_ords_pre_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)

summary(glm(target_ind~seas_ord_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~seas_ord_ndesps_u_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)

summary(glm(target_ind~elec_dem_ndesps_m_12m, family=binomial, data=train)) # smaller p-value
summary(glm(target_ind~elec_dem_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~foot_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~foot_dem_ndesps_u_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)

summary(glm(target_ind~home_dem_ndesps_m_12m, family=binomial, data=train)) # smaller p-value
summary(glm(target_ind~home_dem_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~kids_dem_ndesps_m_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)
summary(glm(target_ind~kids_dem_ndesps_u_12m, family=binomial, data=train))

summary(glm(target_ind~ladies_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~ladies_dem_ndesps_u_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)

summary(glm(target_ind~mens_dem_ndesps_m_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)
summary(glm(target_ind~mens_dem_ndesps_u_12m, family=binomial, data=train)) 

summary(glm(target_ind~seas_dem_ndesps_m_12m, family=binomial, data=train))
summary(glm(target_ind~seas_dem_ndesps_u_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)

summary(glm(target_ind~onl_dem_pre_12m, family=binomial, data=train)) # same p-value, lower AIC (better model)
summary(glm(target_ind~gdem_12m, family=binomial, data=train))

### Find the best logistic regression (after choosing variables)
bbt1 <- glm(target_ind~., family=binomial, data=trainb) #NA - one or more variable is perfectly correlated with each other
summary(bbt1)

trainb <- trainb %>% dplyr::select(-elec_browse_12m,
                                   -elec_ord_ndesps_u_12m,
                                   -foot_ord_ndesps_u_12m,
                                   -furn_ord_ndesps_u_12m,
                                   -home_ord_ndesps_m_12m,
                                   -home_browse_12m,
                                   -home_ord_ndesps_u_12m,
                                   -kids_ord_ndesps_m_12m,
                                   -kids_ord_ndesps_u_12m,
                                   -ladies_ord_ndesps_u_12m,
                                   -mens_ord_ndesps_m_12m,
                                   -mens_ord_ndesps_u_12m,
                                   -used_pounds_off_12m,
                                   -onl_ord_pre_12m,
                                   -used_perc_off_12m,
                                   -elec_ord_pre_12m,
                                   -foot_ord_pre_12m,
                                   -home_ord_pre_12m,
                                   -kids_ord_pre_12m,
                                   -ladies_ord_pre_12m,
                                   -mens_ord_pre_12m,
                                   -seas_ord_pre_12m,
                                   -seas_ord_ndesps_m_12m,
                                   -seas_ord_ndesps_u_12m,
                                   -used_voucher_12m,
                                   -elec_dem_ndesps_u_12m,
                                   -elec_dem_ndesps_m_12m,
                                   -foot_dem_ndesps_u_12m,
                                   -foot_dem_ndesps_m_12m,
                                   -elec_dem_gdesps_m_12m,
                                   -foot_dem_gdesps_m_12m,
                                   -home_dem_gdesps_m_12m,
                                   -kids_dem_gdesps_m_12m,
                                   -ladies_dem_gdesps_m_12m,
                                   -mens_dem_gdesps_m_12m,
                                   -seas_dem_gdesps_m_12m,
                                   -home_dem_ndesps_u_12m,
                                   -home_dem_ndesps_m_12m,
                                   -elec_items_ndesps_u_12m,
                                   -foot_items_ndesps_u_12m,
                                   -home_items_ndesps_u_12m,
                                   -kids_items_ndesps_u_12m,
                                   -ladies_items_ndesps_u_12m,
                                   -mens_items_ndesps_u_12m,
                                   -seas_items_ndesps_u_12m,
                                   -kids_dem_ndesps_u_12m,
                                   -kids_dem_ndesps_m_12m,
                                   -ladies_dem_ndesps_m_12m,
                                   -mens_dem_ndesps_u_12m,
                                   -mens_dem_ndesps_m_12m,
                                   -direct_bnpl_ords_pre_12m,
                                   -items_ndesps_u_12m,
                                   -no_pounds_off_12m,
                                   -no_perc_off_12m,
                                   -items_pre_12m,
                                   -elec_views_12m,
                                   -foot_views_12m,
                                   -home_views_12m,
                                   -mens_views_12m,
                                   -seas_views_12m,
                                   -no_vouchers_12m,
                                   -elec_dem_pre_12m,
                                   -foot_dem_pre_12m,
                                   -home_dem_pre_12m,
                                   -kids_dem_pre_12m,
                                   -seas_dem_pre_12m,
                                   -elec_items_pre_12m,
                                   -foot_items_pre_12m,
                                   -home_items_pre_12m,
                                   -kids_items_pre_12m,
                                   -ladies_items_pre_12m,
                                   -mens_items_pre_12m,
                                   -seas_items_pre_12m,
                                   -scheduled_payments_past_due,
                                   -seas_dem_ndesps_u_12m,
                                   -seas_dem_ndesps_m_12m,
                                   -avg_lag_12m,
                                   -cash_dem_pre_12m,
                                   -clearance_dem_pre_12m,
                                   -direct_bnpl_dem_pre_12m,
                                   -dir_revolving_dem_pre_12m,
                                   -full_price_dem_ndesps_u_12m,
                                   -gdem_12m,
                                   -gdesps_unmatch_12m,
                                   -ndesps_unmatch_12m,
                                   -fs_contrib_unmatch_12m,
                                   -total_contrib_unmatch_12m,
                                   -retail_contrib_unmatch_12m,
                                   -full_price_dem_pre_12m,
                                   -promotional_dem_pre_12m,
                                   -returned_dem_unmatch_12m,
                                   -pounds_off_amt_12m,
                                   -prdt_view_val_12m,
                                   -vouchers_amt_12m)

bbt2 <- glm(target_ind~., family=binomial, data=trainb)

anova(bbt1, bbt2, test="Chisq")
# a very small p-value (< .001) means that removing some insignificant variables to the model did lead to a significantly improved fit over bbt1.

summary(bbt2)

trainb <- trainb %>% dplyr::select(-elec_ord_ndesps_m_12m,
                                   -foot_ord_ndesps_m_12m,
                                   -furn_browse_12m,
                                   -product_views_12m,
                                   -furn_views_12m,
                                   -mens_dem_pre_12m)

bbt3 <- glm(target_ind~., family=binomial, data=trainb)
bbt3.tr <- train(as.factor(target_ind)~., data=trainb, method="glm", family="binomial") # final logistic regression model (42) - select this modelling method for model comparison

anova(bbt2, bbt3, test="Chisq")
# p-value (< .05) means that removing some insignificant variables to the model did lead to a significantly improved fit over bbt2.

summary(bbt3)

exp(coef(bbt3.tr))
# every one unit increase in 'variable', the odds of becoming back to opt-in increases

### Test data
testb <- testb %>% dplyr::select(-elec_browse_12m,
                                 -elec_ord_ndesps_u_12m,
                                 -foot_ord_ndesps_u_12m,
                                 -furn_ord_ndesps_u_12m,
                                 -home_ord_ndesps_m_12m,
                                 -home_browse_12m,
                                 -home_ord_ndesps_u_12m,
                                 -kids_ord_ndesps_m_12m,
                                 -kids_ord_ndesps_u_12m,
                                 -ladies_ord_ndesps_u_12m,
                                 -mens_ord_ndesps_m_12m,
                                 -mens_ord_ndesps_u_12m,
                                 -used_pounds_off_12m,
                                 -onl_ord_pre_12m,
                                 -used_perc_off_12m,
                                 -elec_ord_pre_12m,
                                 -foot_ord_pre_12m,
                                 -home_ord_pre_12m,
                                 -kids_ord_pre_12m,
                                 -ladies_ord_pre_12m,
                                 -mens_ord_pre_12m,
                                 -seas_ord_pre_12m,
                                 -seas_ord_ndesps_m_12m,
                                 -seas_ord_ndesps_u_12m,
                                 -used_voucher_12m,
                                 -elec_dem_ndesps_u_12m,
                                 -elec_dem_ndesps_m_12m,
                                 -foot_dem_ndesps_u_12m,
                                 -foot_dem_ndesps_m_12m,
                                 -elec_dem_gdesps_m_12m,
                                 -foot_dem_gdesps_m_12m,
                                 -home_dem_gdesps_m_12m,
                                 -kids_dem_gdesps_m_12m,
                                 -ladies_dem_gdesps_m_12m,
                                 -mens_dem_gdesps_m_12m,
                                 -seas_dem_gdesps_m_12m,
                                 -home_dem_ndesps_u_12m,
                                 -home_dem_ndesps_m_12m,
                                 -elec_items_ndesps_u_12m,
                                 -foot_items_ndesps_u_12m,
                                 -home_items_ndesps_u_12m,
                                 -kids_items_ndesps_u_12m,
                                 -ladies_items_ndesps_u_12m,
                                 -mens_items_ndesps_u_12m,
                                 -seas_items_ndesps_u_12m,
                                 -kids_dem_ndesps_u_12m,
                                 -kids_dem_ndesps_m_12m,
                                 -ladies_dem_ndesps_m_12m,
                                 -mens_dem_ndesps_u_12m,
                                 -mens_dem_ndesps_m_12m,
                                 -direct_bnpl_ords_pre_12m,
                                 -items_ndesps_u_12m,
                                 -no_pounds_off_12m,
                                 -no_perc_off_12m,
                                 -items_pre_12m,
                                 -elec_views_12m,
                                 -foot_views_12m,
                                 -home_views_12m,
                                 -mens_views_12m,
                                 -seas_views_12m,
                                 -no_vouchers_12m,
                                 -elec_dem_pre_12m,
                                 -foot_dem_pre_12m,
                                 -home_dem_pre_12m,
                                 -kids_dem_pre_12m,
                                 -seas_dem_pre_12m,
                                 -elec_items_pre_12m,
                                 -foot_items_pre_12m,
                                 -home_items_pre_12m,
                                 -kids_items_pre_12m,
                                 -ladies_items_pre_12m,
                                 -mens_items_pre_12m,
                                 -seas_items_pre_12m,
                                 -scheduled_payments_past_due,
                                 -seas_dem_ndesps_u_12m,
                                 -seas_dem_ndesps_m_12m,
                                 -avg_lag_12m,
                                 -cash_dem_pre_12m,
                                 -clearance_dem_pre_12m,
                                 -direct_bnpl_dem_pre_12m,
                                 -dir_revolving_dem_pre_12m,
                                 -full_price_dem_ndesps_u_12m,
                                 -gdem_12m,
                                 -gdesps_unmatch_12m,
                                 -ndesps_unmatch_12m,
                                 -fs_contrib_unmatch_12m,
                                 -total_contrib_unmatch_12m,
                                 -retail_contrib_unmatch_12m,
                                 -full_price_dem_pre_12m,
                                 -promotional_dem_pre_12m,
                                 -returned_dem_unmatch_12m,
                                 -pounds_off_amt_12m,
                                 -prdt_view_val_12m,
                                 -vouchers_amt_12m,
                                 -elec_ord_ndesps_m_12m,
                                 -foot_ord_ndesps_m_12m,
                                 -furn_browse_12m,
                                 -product_views_12m,
                                 -furn_views_12m,
                                 -mens_dem_pre_12m)

### Accuracy (Logistic Regression 42)
lg.pred42 <- predict(bbt3, testb, type="response")
table(testb$target_ind, lg.pred42>0.5)
lg.accuracy42 <- (90208+216)/(90208+209+29836+216) # threshold check (0.7505997)

### Sensitivity 42
lgcf42 <- factor(predict(bbt3.tr, testb))
real42 <- factor(testb$target_ind)
data42 <- data.frame(data=lgcf42, type="prediction")
data42.1 <- data.frame(data=real42, type="real")
combine42 <- rbind(data42, data42.1)
confusionMatrix(combine42[combine42$type == "prediction", 1], combine42[combine42$type == "real", 1], dnn=c("Prediction", "Reference"))

lgcf42 <- lgcf42 %>% mutate()
lgpr42 <- factor(ifelse(lgcf42[, "1"] > 0.5, "1", "0"))
lgpr42 <- relevel(lgpr42, "1")
confusionMatrix(lgpr42, testb$target_ind)


## Logistic Regression 2

### xgbTree
fitCon1 <- trainControl(
  method = "repeatedcv",
  number = 4
)

oddtf <- oddt
oddtf[c("target_ind")] <- lapply(oddtf[c("target_ind")], factor)

KY <- train(
  target_ind ~., oddtf,
  method = "xgbTree",
  trControl = fitCon1
)

KY <- randomForest(target_ind~., data=oddt, trControl = fitCon1)

#### Variable importance
varImp(KY)$importance %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname)) %>% 
  tail(13) %>% 
  ggplot() +
  geom_col(aes(x = rowname, y= Overall)) +
  coord_flip() +
  theme_bw()

varImp(glm(target_ind~., family=binomial, data=oddtf))
importance(glm(target_ind~., family=binomial, data=oddtf))

### Find the best logistic regression (based on the variable importance) - 13 variables
vi <- oddtf %>% dplyr::select(outstanding_balance,
                              days_since_12m,
                              age,
                              credit_limit,
                              tenure_months,
                              items_ndesps_u_12m,
                              rejects_12m,
                              recruitment_credit_risk_factor,
                              credit_set,
                              days_since_pre_12m,
                              gender_code,
                              apr,
                              target_ind)

set.seed(1234)
datapartitions <- createDataPartition(vi$target_ind, p=0.7, list=FALSE)
trains=vi[datapartitions,]
tests=vi[-datapartitions,]

viglm <- glm(target_ind~., family=binomial, data=trains)
viglm.tr <- train(target_ind~., data=trains, method="glm", family="binomial") # final logistic regression model (13) - select this modelling method for model comparison

### Accuracy (Logistic Regression 13)
lg.pred13 <- predict(viglm, tests, type="response")
table(tests$target_ind, lg.pred13>0.5)
lg.accuracy13 <- (90342+23)/(90342+14+30090+23) # threshold check (0.75011)

### Sensitivity 13
lgcf13 <- factor(predict(viglm.tr, tests))
real13 <- factor(tests$target_ind)
data13 <- data.frame(data=lgcf13, type="prediction")
data13.1 <- data.frame(data=real13, type="real")
combine13 <- rbind(data13, data13.1)
confusionMatrix(combine13[combine13$type == "prediction", 1], combine13[combine13$type == "real", 1], dnn=c("Prediction", "Reference"))


## Logistic regerssion 3 - 156 variables
lr.tr <- train(as.factor(target_ind)~., data=traind, method="glm", family="binomial")
print(lr.tr)
table(testd$target_ind, predict(lr.tr, testd, type="prob")>0.5)

### Sensitivity 156
lgcf156 <- factor(predict(lr.tr, testd))
real156 <- factor(testd$target_ind)
data156 <- data.frame(data=lgcf156, type="prediction")
data156.1 <- data.frame(data=real156, type="real")
combine156 <- rbind(data156, data156.1)
confusionMatrix(combine156[combine156$type == "prediction", 1], combine156[combine156$type == "real", 1], dnn=c("Prediction", "Reference"))


## Decision Tree 1 - 156/42 variables
set.seed(1234)
datapartitiond <- createDataPartition(oddt$target_ind, p=0.7, list=FALSE)
traind=oddt[datapartitiond,]
testd=oddt[-datapartitiond,]

set.seed(1234)
dt156.tr <- train(as.factor(target_ind)~., data=traind, method="rpart") # use this (156 variables)
rpart.subrules.table(dt156.tr$finalModel)
prp(dt156.tr$finalModel)
prp(dt156.tr$finalModel, box.palette = "Blues", tweak = 1.2)
print(dt156.tr)

set.seed(2345)
dt42.tr <- train(as.factor(target_ind)~., data=trainb, method="rpart") # use this (42 variables)
rpart.subrules.table(dt42.tr$finalModel)
prp(dt42.tr$finalModel)
print(dt42.tr)

### Sensitivity 156/42 DT
dtcf156 <- factor(predict(dt156.tr, testd))
real156.dt <- factor(testd$target_ind)
data156.dt <- data.frame(data=dtcf156, type="prediction")
data156.1.dt <- data.frame(data=real156.dt, type="real")
combine156.dt <- rbind(data156.dt, data156.1.dt)
confusionMatrix(combine156.dt[combine156.dt$type == "prediction", 1], combine156.dt[combine156.dt$type == "real", 1], dnn=c("Prediction", "Reference"))

dtcf42 <- factor(predict(dt42.tr, testb))
real42.dt <- factor(testb$target_ind)
data42.dt <- data.frame(data=dtcf42, type="prediction")
data42.1.dt <- data.frame(data=real42.dt, type="real")
combine42.dt <- rbind(data42.dt, data42.1.dt)
confusionMatrix(combine42.dt[combine42.dt$type == "prediction", 1], combine42.dt[combine42.dt$type == "real", 1], dnn=c("Prediction", "Reference"))

## Decision tree 1 with rpart code - not considered
dt42 <- rpart(target_ind~., data = trainb, method = 'class') # only 1 root
set.seed(1234)
dt42 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(minsplit=112, maxdepth=8, cp=0))
dt42$cptable
dt.prune42 <- prune(dt42, cp=dt42$cptable[which.min(dt42$cptable[,"xerror"]),"CP"]) # prune (to prevent ovefitting)

### Predict for decision tree 1 - not considered
### a fully grown tree will overfit the training data and might lead to poor test set performance
dt.pred42 <- predict(dt.prune42, testb, type="class")
dt.perf42 <- table(testb$target_ind, dt.pred42, dnn=c("Actual", "Predicted"))
(dt.accuracy42 <- sum(diag(dt.perf42)) / sum(dt.perf42)) # 0.7556135

### Plot for decision tree 1 - not considered
rpart.plot(dt.prune42)
prp(dt.prune42)

## Decision Tree 2 -  13 variables
set.seed(3456)
dt13.tr <- train(as.factor(target_ind)~., data=trains, method="rpart") # use this (12 predictors)
rpart.subrules.table(dt13.tr$finalModel)
prp(dt13.tr$finalModel)
print(dt13.tr)

### Sensitivity 13 DT
dtcf13 <- factor(predict(dt13.tr, tests))
real13.dt <- factor(tests$target_ind)
data13.dt <- data.frame(data=dtcf13, type="prediction")
data13.1.dt <- data.frame(data=real13.dt, type="real")
combine13.dt <- rbind(data13.dt, data13.1.dt)
confusionMatrix(combine13.dt[combine13.dt$type == "prediction", 1], combine13.dt[combine13.dt$type == "real", 1], dnn=c("Prediction", "Reference"))

## Decision tree 1 with rpart code - not considered
dt13 <- rpart(target_ind~., data = trains, method = 'class') # only 1 root
set.seed(1234)
dt13 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(minsplit=120, maxdepth=9, cp=0))
dt13$cptable
dt.prune13 <- prune(dt13, cp=dt13$cptable[which.min(dt13$cptable[,"xerror"]),"CP"])

### Predict for decision tree 2 - not considered
dt.pred13 <- predict(dt.prune13, tests, type="class")
dt.perf13 <- table(tests$target_ind, dt.pred13, dnn=c("Actual", "Predicted"))
(dt.accuracy13 <- sum(diag(dt.perf13)) / sum(dt.perf13)) # 0.7550988

### Plot for decision tree 2 - not considered
rpart.plot(dt.prune13)
prp(dt.prune13)
prp(dt13.tr$finalModel, box.palette = "Blues", tweak = 1.2)

## Conditional inference tree - not considered
ct <- ctree(target_ind~., data=train)
plot(ct, main="Conditional Inference Tree")
ct.pred <- predict(ct, test, type="response")
(ct.perf <- table(test$target_ind, ct.pred, dnn = c("Actual", "Predicted")))
(accuracy <- sum(diag(dt.perf)) / sum(dt.perf))

## Random forest - not considered
set.seed(1234)
forest <- randomForest(target_ind~., data=train, importance=TRUE)
forest
importance(forest, type=2)
forest.pred <- predict(forest, train)
(forest.perf <- table(train$target_ind, forest.pred, dnn=c("Actual", "Predicted")))


# Model comparison

## Kappa value
print(bbt3.tr)
print(viglm.tr)
print(dt142.tr)
print(dt42.tr)
print(dt13.tr)

## Sesitivity, specificity, positive predictive value, negative predictive value, accuracy
install.packages("InformationValue")
library(InformationValue)
sensitivity(testb$target_ind, predict(bbt3.tr, testb, type="prob"), threshold=optimalCutoff(testb$target_ind, predicted)[1])

performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2x2 table")
  tn=table[1,1]
  fp=table[1,2]
  fn=table[2,1]
  tp=table[2,2]
  sensitivity=tp/(tp+fn)
  specificity=tn/(tn+fp)
  ppp=tp/(tp+fp)
  npp=tn/(tn+fn)
  hitrate=(tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n), 
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

## Cumulative lift - not considered
## using a predictive model to choose which customers to contact.
## The lift chart shows how much more likely we are to receive respondents than if we contact a random sample of customers. 
## By contacting only 10% of customers based on the predictive model we will reach 2.5 times as many respondents as if we use no model.

### Logistic regression with 42 variables (train vs. test) - not considered
lg42 <- prepare_scores_and_ntiles(datasets=list("trainb","testb"),
                                           dataset_labels = list("train","test"),
                                           models = list("bbt3.tr"),
                                           model_labels = list("Logistic Regression (42)"),
                                           target_column="target_ind",
                                           ntiles=100)
lg42.plot <- plotting_scope(prepared_input = lg42, scope="compare_datasets")
plot_cumgains(lg42.plot)
plot_cumlift(lg42.plot)
plot_multiplot(lg42.plot) # cumulative gains, cumualtive lift, response, cumulative response

### Logistic regression with 13 variables - not considered
lg13 <- prepare_scores_and_ntiles(datasets=list("trains","tests"),
                                  dataset_labels = list("train","test"),
                                  models = list("viglm.tr"),
                                  model_labels = list("Logistic Regression (13)"),
                                  target_column="target_ind",
                                  ntiles=100)
lg13.plot <- plotting_scope(prepared_input = lg30, scope="compare_datasets")
plot_cumgains(lg13.plot)
plot_cumlift(lg13.plot)
plot_multiplot(lg13.plot) # cumulative gains, cumualtive lift, response, cumulative response

### Decision tree with 42 variables - not considered
dt42 <- prepare_scores_and_ntiles(datasets=list("trainb","testb"),
                                  dataset_labels = list("train","test"),
                                  models = list("dt42.tr"),
                                  model_labels = list("Decision Tree (42)"),
                                  target_column="target_ind",
                                  ntiles=100)
dt42.plot <- plotting_scope(prepared_input = dt42, scope="compare_datasets")
plot_cumgains(dt42.plot)
plot_cumlift(dt42.plot)
plot_multiplot(dt42.plot) # cumulative gains, cumualtive lift, response, cumulative response

### Decision tree with 13 variables - not considered
dt13 <- prepare_scores_and_ntiles(datasets=list("trains","tests"),
                                  dataset_labels = list("train","test"),
                                  models = list("dt13.tr"),
                                  model_labels = list("Decision Tree (30)"),
                                  target_column="target_ind",
                                  ntiles=100)
dt13.plot <- plotting_scope(prepared_input = dt13, scope="compare_datasets")
plot_cumgains(dt13.plot)
plot_cumlift(dt31.plot)
plot_multiplot(dt13.plot) # cumulative gains, cumualtive lift, response, cumulative response

### Compare all models - not considered
oddtp <- oddt %>% dplyr::select(aban_amt_12m,
                                age,
                                apr,
                                avg_days_bet_ords_pre_12m,
                                bnpl_eligible,
                                bnpl_purchase_pre_12m,
                                bnpl_settle_pre_12m,
                                cash,
                                cash_ords_pre_12m,
                                clearance_ord_pre_12m,
                                credit_limit,
                                credit_set,
                                days_since_12m,
                                days_since_pre_12m,
                                days_since_return_unmatch_12m,
                                depts_pre_12m,
                                dir_revolving_ords_pre_12m,
                                foot_browse_12m,
                                full_price_ord_pre_12m,
                                gender_code,
                                items_ndesps_u_12m,
                                kids_browse_12m,
                                kids_views_12m,
                                ladies_browse_12m,
                                ladies_dem_ndesps_u_12m,
                                ladies_dem_pre_12m,
                                ladies_ord_ndesps_m_12m,
                                ladies_views_12m,
                                margin_unmatch_12m,
                                mens_browse_12m,
                                multichannel_pre_12m,
                                n_dis_pv_12m,
                                off_ord_pre_12m,
                                onl_dem_pre_12m,
                                orders_ndesps_u_12m,
                                orders_pre_12m,
                                outstanding_balance,
                                promotional_ord_pre_12m,
                                recruitment_credit_risk_factor,
                                rejects_12m,
                                seas_browse_12m,
                                target_ind,
                                tenure_months,
                                visits_12m
                                
)

set.seed(1234)
datapartitionp <- createDataPartition(oddtp$target_ind, p=0.7, list=FALSE)
trainp=oddtp[datapartitionp,]
testp=oddtp[-datapartitionp,]

ld <- prepare_scores_and_ntiles(datasets=list("trainp","testp"),
                                  dataset_labels = list("train","test"),
                                  models = list("bbt3.tr", "viglm.tr", "dt42.tr", "dt13.tr"),
                                  model_labels = list("Logistic Regression (42)", "Logistic Regression (13)", "Decision Tree (42)", "Decision Tree (13)"),
                                  target_column="target_ind",
                                  ntiles=100)
ld.plot <- plotting_scope(prepared_input = ld, scope="compare_models")
plot_cumgains(ld.plot)
plot_cumlift(ld.plot)
plot_multiplot(ld.plot) # cumulative gains, cumualtive lift, response, cumulative response

plotLift(predicted = lg.pred42, labels = testb$target_ind, cumulative = TRUE, col= "red", n.buckets = 100)
plotLift(predicted = lg.pred13, labels = tests$target_ind, cumulative = TRUE, col= "green", n.buckets = 100)
plotLift(predicted = dt.pred42, labels = testb$target_ind, cumulative = TRUE, col= "blue", n.buckets = 100)
plotLift(predicted = dt.pred13, labels = tests$target_ind, cumulative = TRUE, col= "orange", n.buckets = 100)

lg.pred42. <- predict(bbt3.tr, testb, type="prob")
lg.pred42. <- ifelse(lg.pred42.>0.5, 1, 0)
table(testb$target_ind, lg.pred42.>0.5)
lg.perf42. <- table(testb$target_ind, lg.pred42., dnn=c("Actual", "Predicted"))
(dt.accuracy42. <- sum(diag(dt.perf42.)) / sum(dt.perf42.)) 

lg.pred13. <- predict(viglm.tr, tests, type="prob")
dt.pred42. <- predict(dt42.tr, testb, type="prob")
dt.pred13. <- predict(dt13.tr, tests, type="prob")

plotLift(predicted = lg.pred42., labels = testb$target_ind, cumulative = TRUE, col= "red", n.buckets = 100)
plotLift(predicted = lg.pred13., labels = tests$target_ind, cumulative = TRUE, col= "green", n.buckets = 100)
plotLift(predicted = dt.pred42., labels = testb$target_ind, cumulative = TRUE, col= "blue", n.buckets = 100)
plotLift(predicted = dt.pred13., labels = tests$target_ind, cumulative = TRUE, col= "orange", n.buckets = 100)
