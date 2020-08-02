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
                             target_ind
)

set.seed(1234)
datapartitiona <- createDataPartition(a$target_ind, p=0.7, list=FALSE)
traina=a[datapartitiona,]
testa=a[-datapartitiona,]

ag <- glm(target_ind~., family=binomial, data=traina)

anova(bbt3, ag, test="Chisq")

lg.pred <- predict(ag, testa, type="response")
lg.pred <- ifelse(lg.pred > 0.5, "In", "Out")
lg.perf <- table(testa$target_ind, lg.pred, dnn=c("Actual", "Predicted"))
(lg.accuracy <- sum(diag(lg.perf)) / sum(lg.perf)) # 0.2499315

# Variable importance > 8 - select
v1 <- oddtf %>% dplyr::select(outstanding_balance,
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
                              target_ind
)

set.seed(1234)
datapartition1 <- createDataPartition(v1$target_ind, p=0.7, list=FALSE)
train1=v1[datapartition1,]
test1=v1[-datapartition1,]

v1g <- glm(target_ind~., family=binomial, data=train1)

anova(ag, v1g, test="Chisq")

lg.pred <- predict(v1g, test1, type="response")
lg.pred <- ifelse(lg.pred > 0.5, "In", "Out")
lg.perf <- table(test1$target_ind, lg.pred, dnn=c("Actual", "Predicted"))
(lg.accuracy <- sum(diag(lg.perf)) / sum(lg.perf)) # 0.24989

# Variable importance > 7
v2 <- oddtf %>% dplyr::select(outstanding_balance,
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
                              scheduled_payments_past_due,
                              target_ind
)

set.seed(1234)
datapartition2 <- createDataPartition(v2$target_ind, p=0.7, list=FALSE)
train2=v2[datapartition2,]
test2=v2[-datapartition2,]

v2g <- glm(target_ind~., family=binomial, data=train2)

anova(v1g, v2g, test="Chisq") # p-value = 0.8515

lg.pred <- predict(v2g, test2, type="response")
lg.pred <- ifelse(lg.pred > 0.5, "In", "Out")
lg.perf <- table(test2$target_ind, lg.pred, dnn=c("Actual", "Predicted"))
(lg.accuracy <- sum(diag(lg.perf)) / sum(lg.perf)) # 0.24989