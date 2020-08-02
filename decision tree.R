## Decision Tree 1 - 42 variables
set.seed(1234)
dt1 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=9, cp=0))
dtp1 <- prune(dt1, cp=dt1$cptable[which.min(dt1$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt2 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=8, cp=0))
dtp2 <- prune(dt2, cp=dt2$cptable[which.min(dt2$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt3 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=7, cp=0))
dtp3 <- prune(dt3, cp=dt1$cptable[which.min(dt3$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt4 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=6, cp=0))
dtp4 <- prune(dt4, cp=dt4$cptable[which.min(dt4$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt5 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=5, cp=0))
dtp5 <- prune(dt5, cp=dt5$cptable[which.min(dt5$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt6 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=4, cp=0))
dtp6 <- prune(dt6, cp=dt6$cptable[which.min(dt6$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt7 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=3, cp=0))
dtp7 <- prune(dt7, cp=dt7$cptable[which.min(dt7$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt8 <- rpart(target_ind~., data = trainb, method = "class", control = rpart.control(maxdepth=2, cp=0))
dtp8 <- prune(dt8, cp=dt8$cptable[which.min(dt8$cptable[,"xerror"]),"CP"])

d42 <- prepare_scores_and_ntiles(datasets=list("trainb","testb"),
                                dataset_labels = list("train","test"),
                                models = list("dt1", "dt2", "dt3", "dt4", "dt5", "dt6", "dt7", "dt8"),
                                model_labels = list("dt1", "dt2", "dt3", "dt4", "dt5", "dt6", "dt7", "dt8"),
                                target_column="target_ind",
                                ntiles=100)
plot42 <- plotting_scope(prepared_input = d42, scope="compare_models")
plot_multiplot(plot42)

## Decision Tree 2 -  13 variables
set.seed(1234)
dt1 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=9, cp=0))
dtp1 <- prune(dt1, cp=dt1$cptable[which.min(dt1$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt2 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=8, cp=0))
dtp2 <- prune(dt2, cp=dt2$cptable[which.min(dt2$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt3 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=7, cp=0))
dtp3 <- prune(dt3, cp=dt1$cptable[which.min(dt3$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt4 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=6, cp=0))
dtp4 <- prune(dt4, cp=dt4$cptable[which.min(dt4$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt5 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=5, cp=0))
dtp5 <- prune(dt5, cp=dt5$cptable[which.min(dt5$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt6 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=4, cp=0))
dtp6 <- prune(dt6, cp=dt6$cptable[which.min(dt6$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt7 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=3, cp=0))
dtp7 <- prune(dt7, cp=dt7$cptable[which.min(dt7$cptable[,"xerror"]),"CP"])

set.seed(1234)
dt8 <- rpart(target_ind~., data = trains, method = "class", control = rpart.control(maxdepth=2, cp=0))
dtp8 <- prune(dt8, cp=dt8$cptable[which.min(dt8$cptable[,"xerror"]),"CP"])

d13 <- prepare_scores_and_ntiles(datasets=list("trains","tests"),
                                 dataset_labels = list("train","test"),
                                 models = list("dt1", "dt2", "dt3", "dt4", "dt5", "dt6", "dt7", "dt8"),
                                 model_labels = list("dt1", "dt2", "dt3", "dt4", "dt5", "dt6", "dt7", "dt8"),
                                 target_column="target_ind",
                                 ntiles=100)
plot13 <- plotting_scope(prepared_input = d13, scope="compare_models")
plot_multiplot(plot13)