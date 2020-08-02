# Variable importance > 10
a <- oddtf %>% dplyr::select(outstanding_balance,
                             days_since_12m,
                             age,
                             credit_limit,
                             tenure_months,
                             items_ndesps_u_12m,
                             rejects_12m,
                             recruitment_credit_risk_factor,
                             credit_set,
                             target_ind)

set.seed(1234)
datapartitiona <- createDataPartition(a$target_ind, p=0.7, list=FALSE)
traina=a[datapartitiona,]
testa=a[-datapartitiona,]

ag <- train(target_ind~., data=traina, method="rpart")
print(ag)

# Variable importance > 8
b <- oddtf %>% dplyr::select(outstanding_balance,
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
datapartitionb <- createDataPartition(b$target_ind, p=0.7, list=FALSE)
trainb=b[datapartitionb,]
testb=b[-datapartitionb,]

bg <- train(target_ind~., data=trainb, method="rpart")
print(bg)