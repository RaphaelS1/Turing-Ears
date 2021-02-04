set.seed(42)
data <- read.csv("results.csv")
data$AvgLength <- apply(data[c(4,6)],1,mean)
data$AvgCirc <- apply(data[c(5,7)],1,mean)
levels(data$Ethnicity)[1] <- "Other"

#----------------------
# Demographics
#----------------------
summary(data$Age)
summary(data$Sex)
table(data$Ethnicity)
table(data$Ethnicity, data$Sex)

white = data$Ethnicity == "White"
bame = data$Ethnicity != "White"
male = data$Sex == "Male"
female = data$Sex == "Female"

#----------------------
# Circumference Summary
#----------------------
summary(data$Lcirc)
summary(data$Rcirc)
summary(data$AvgCirc)
ks_circ_stat = ks.test(data$Lcirc, data$Rcirc)$statistic
ks_circ_p = ks.test(data$Lcirc, data$Rcirc)$p.value
wrs_circ_stat = wilcox.test(data$Lcirc, data$Rcirc)$statistic
wrs_circ_p = wilcox.test(data$Lcirc, data$Rcirc)$p.value


summary(data$AvgCirc[white])
summary(data$AvgCirc[bame])
ks_circ_ethn_stat = ks.test(data$AvgCirc[white], data$AvgCirc[bame])$statistic
ks_circ_ethn_p = ks.test(data$AvgCirc[white], data$AvgCirc[bame])$p.value
wrs_circ_ethn_stat = wilcox.test(data$AvgCirc[white], data$AvgCirc[bame])$statistic
wrs_circ_ethn_p = wilcox.test(data$AvgCirc[white], data$AvgCirc[bame])$p.value

summary(data$AvgCirc[male])
summary(data$AvgCirc[female])
ks_circ_sex_stat = ks.test(data$AvgCirc[male], data$AvgCirc[female])$statistic
ks_circ_sex_p = ks.test(data$AvgCirc[male], data$AvgCirc[female])$p.value
wrs_circ_sex_stat = wilcox.test(data$AvgCirc[male], data$AvgCirc[female], alternative = "greater")$statistic
wrs_circ_sex_p = wilcox.test(data$AvgCirc[male], data$AvgCirc[female], alternative = "greater")$p.value

#----------------------
# Length Summary
#----------------------
summary(data$Llength)
summary(data$Rlength)
summary(data$AvgLength)
ks_length_stat = ks.test(data$Llength, data$Rlength)$statistic
ks_length_p = ks.test(data$Llength, data$Rlength)$p.value
wrs_length_stat = wilcox.test(data$Llength, data$Rlength)$statistic
wrs_length_p = wilcox.test(data$Llength, data$Rlength)$p.value


summary(data$AvgLength[white])
summary(data$AvgLength[bame])
ks_length_ethn_stat = ks.test(data$AvgLength[white], data$AvgLength[bame])$statistic
ks_length_ethn_p = ks.test(data$AvgLength[white], data$AvgLength[bame])$p.value
wrs_length_ethn_stat = wilcox.test(data$AvgLength[white], data$AvgLength[bame], alternative = "greater")$statistic
wrs_length_ethn_p = wilcox.test(data$AvgLength[white], data$AvgLength[bame], alternative = "greater")$p.value

summary(data$AvgLength[male])
summary(data$AvgLength[female])
ks_length_sex_stat = ks.test(data$AvgLength[male], data$AvgLength[female])$statistic
ks_length_sex_p = ks.test(data$AvgLength[male], data$AvgLength[female])$p.value
wrs_length_sex_stat = wilcox.test(data$AvgLength[male], data$AvgLength[female], alternative = "greater")$statistic
wrs_length_sex_p = wilcox.test(data$AvgLength[male], data$AvgLength[female], alternative = "greater")$p.value

#----------------------------------
# Test statistics and adjusted p
#----------------------------------
padj = signif(matrix(p.adjust(c(ks_length_p, ks_length_ethn_p,ks_length_sex_p,
            ks_circ_p, ks_circ_ethn_p, ks_circ_sex_p,
           wrs_length_p, wrs_length_ethn_p,wrs_length_sex_p,
            wrs_circ_p, wrs_circ_ethn_p, wrs_circ_sex_p), method = "BH"),
       ncol = 2),4)

write.csv(data.frame(Group = c("Intra Length","Eth Length","Sex Length","Intra Circ","Eth Length","Sex Length"),
           KS_Test = c(ks_length_stat, ks_length_ethn_stat,ks_length_sex_stat,
                       ks_circ_stat, ks_circ_ethn_stat, ks_circ_sex_stat),
           KS_P = signif(c(ks_length_p, ks_length_ethn_p,ks_length_sex_p,
                       ks_circ_p, ks_circ_ethn_p, ks_circ_sex_p),4),
           KS_PAdj = padj[,1],
           WRS_Test = c(wrs_length_stat, wrs_length_ethn_stat,wrs_length_sex_stat,
                        wrs_circ_stat, wrs_circ_ethn_stat, wrs_circ_sex_stat),
           WRS_P = signif(c(wrs_length_p, wrs_length_ethn_p,wrs_length_sex_p,
                     wrs_circ_p, wrs_circ_ethn_p, wrs_circ_sex_p),4),
           WRS_PAdj = padj[,2]
           ), "results/Tests and ps.csv")

#----------------------
# True vs TOT Circ
#----------------------
circ_mod <- function(age){
  return(age*0.51 + 88.1)
}
data$predCirc = circ_mod(data$Age)
data$uninf_circ = rep(103.6, 100)

Hmisc::rcorr(data$predCirc, data$AvgCirc)

p1 <- ggplot(data = data.frame(x = 1:length(data$AvgCirc), y = sort(data$AvgCirc - data$predCirc)), aes(x = x, y = y)) +
  geom_point() + xlab("Index") + ylab("Error") + ggtitle("True Ear Circumference - TOT Predicted Ear Circumference") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

summary(abs(data$AvgCirc - data$predCirc))

#----------------------
# True vs HC Length
#----------------------
length_mod <- function(age){
  return(age*0.22 + 55.9)
}
data$predLength = length_mod(data$Age)
data$uninf_length = rep(length_mod(42), 100)

Hmisc::rcorr(data$AvgLength, data$predLength)
filter_hc = !is.na(data$AvgLength)

p2 <- ggplot(data = data.frame(x = 1:length(data$AvgLength[filter_hc]),
                               y = sort(data$AvgLength[filter_hc] - data$predLength[filter_hc])),
             aes(x = x, y = y)) + geom_point() + xlab("Index") + ylab("Error") + ggtitle("True Ear Length - HC Predicted Ear Length") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

dev.copy(tiff, "plots/TOT_HC_Resids.tif",res=100,units="in",width=7,height=6)
dev.off()

summary(abs(data$AvgLength - data$predLength))

#------------------------
# External validation
#------------------------
uninf_circ_rmse = mlr::measureRMSE(truth = data$AvgCirc, response = data$uninf_circ)
uninf_circ_rmse.se = measureRMSE.SE(truth = data$AvgCirc, response = data$uninf_circ)
uninf_circ_mse = mlr::measureMSE(truth = data$AvgCirc, response = data$uninf_circ)
uninf_circ_mse.se = measureMSE.SE(truth = data$AvgCirc, response = data$uninf_circ)
uninf_circ_mae = mlr::measureMAE(truth = data$AvgCirc, response = data$uninf_circ)
uninf_circ_mae.se = measureMAE.SE(truth = data$AvgCirc, response = data$uninf_circ)

tot_rmse = mlr::measureRMSE(truth = data$AvgCirc, response = data$predCirc)
tot_rmse.se = measureRMSE.SE(truth = data$AvgCirc, response = data$predCirc)
tot_mse = mlr::measureMSE(truth = data$AvgCirc, response = data$predCirc)
tot_mse.se = measureMSE.SE(truth = data$AvgCirc, response = data$predCirc)
tot_mae = mlr::measureMAE(truth = data$AvgCirc, response = data$predCirc)
tot_mae.se = measureMAE.SE(truth = data$AvgCirc, response = data$predCirc)

uninf_length_rmse = mlr::measureRMSE(truth = data$AvgLength[filter_hc], response = data$uninf_length[filter_hc])
uninf_length_rmse.se = measureRMSE.SE(truth = data$AvgLength[filter_hc], response = data$uninf_length[filter_hc])
uninf_length_mse = mlr::measureMSE(truth = data$AvgLength[filter_hc], response = data$uninf_length[filter_hc])
uninf_length_mse.se = measureMSE.SE(truth = data$AvgLength[filter_hc], response = data$uninf_length[filter_hc])
uninf_length_mae = mlr::measureMAE(truth = data$AvgLength[filter_hc], response = data$uninf_length[filter_hc])
uninf_length_mae.se = measureMAE.SE(truth = data$AvgLength[filter_hc], response = data$uninf_length[filter_hc])

hc_rmse = mlr::measureRMSE(truth = data$AvgLength[filter_hc], response = data$predLength[filter_hc])
hc_rmse.se = measureRMSE.SE(truth = data$AvgLength[filter_hc], response = data$predLength[filter_hc])
hc_mse = mlr::measureMSE(truth = data$AvgLength[filter_hc], response = data$predLength[filter_hc])
hc_mse.se = measureMSE.SE(truth = data$AvgLength[filter_hc], response = data$predLength[filter_hc])
hc_mae = mlr::measureMAE(truth = data$AvgLength[filter_hc], response = data$predLength[filter_hc])
hc_mae.se = measureMAE.SE(truth = data$AvgLength[filter_hc], response = data$predLength[filter_hc])

x = data.frame(Model = c("Adams Circ","TOT","Adams Length","HC"),
               RMSE = c(uninf_circ_rmse, tot_rmse, uninf_length_rmse, hc_rmse),
               RMSE.se = c(uninf_circ_rmse.se, tot_rmse.se, uninf_length_rmse.se, hc_rmse.se),
               MSE = c(uninf_circ_mse, tot_mse, uninf_length_mse, hc_mse),
               MSE.se = c(uninf_circ_mse.se, tot_mse.se, uninf_length_mse.se, hc_mse.se),
               MAE = c(uninf_circ_mae, tot_mae, uninf_length_mae, hc_mae),
               MAE.se = c(uninf_circ_mae.se, tot_mae.se, uninf_length_mae.se, hc_mae.se))

# TOT vs Uninformed
totVsUninf <-  wilcox.test((data$AvgCirc-data$predCirc)^2, (data$AvgCirc-data$uninf_circ)^2, paired = TRUE)

# HC vs Uninformed
HCVsUninf <- wilcox.test((data$AvgLength-data$predLength)^2, (data$AvgLength-data$uninf_length)^2, paired = TRUE)

xp <- data.frame(Model = c("TOT","HC"), Wilcox = c(totVsUninf$statistic, HCVsUninf$statistic),
           p = round(p.adjust(c(totVsUninf$p.value,HCVsUninf$p.value),method="BH"),2))

write.csv(x, "results/ExternalValidation.csv")
write.csv(xp, "results/ExternalValidation_Wilcox.csv")

#------------------------
# Cross-Validation
#------------------------
library(mlr)
measures = list(rmse, rmse.se, mse, mse.se, mae, mae.se)
resamples = makeResampleDesc("CV", iters = 10)
ps <- makeParamSet(
  makeDiscreteParam("family",values = c("gaussian","Gamma","poisson")),
  makeDiscreteParam("gaussian.link",values = c("identity","log","inverse")),
  makeDiscreteParam("Gamma.link",values = c("identity","log","inverse")),
  makeDiscreteParam("poisson.link",values = c("identity","log","sqrt")),
  makeNumericParam("epsilon",lower = 1e-10, upper = 3)
)
glm <- makeTuneWrapper(makeLearner("regr.glm"),
                       makeResampleDesc("CV",iters = 3),
                       rmse, ps, makeTuneControlGrid())

ps <- makeParamSet(
  makeIntegerParam("ntree", lower = 10, upper = 1000),
  makeIntegerParam("nodesize", lower = 1, upper = 10),
  makeIntegerParam("maxnodes", lower = 1, upper = 100))
rf <- makeTuneWrapper(makeLearner("regr.randomForest"),
                      makeResampleDesc("CV",iters = 3),
                      rmse, ps, makeTuneControlIrace(maxExperiments = 250))

learners = c(makeLearners(c("regr.featureless","regr.lm")), regr.glm = list(glm), regr.randomForest = list(rf))

# Experiment 1: Circ ~ Age
task_data = data[,c("Age","AvgCirc")]
task = makeRegrTask("Experiment1", data = task_data, target = "AvgCirc")
set.seed(42)
bm1 = benchmark(learners, task, resamples, measures)

write.csv(t(as.data.frame(getBMRAggrPerformances(bm1))),"results/Circ_Age.csv")
write.csv(comparisontest(bm1)$Experiment1$comparison,"results/Circ_Age_Comparison.csv")
write.csv(p.adjustframe(comparisontest(bm1)$Experiment1$comparison),
          "results/Circ_Age_Adjusted.csv")

# Experiment 2: Length ~ Age
task_data = data[,c("Age","AvgLength")]
task_data = task_data[!is.na(task_data$AvgLength),]
task = makeRegrTask("Experiment2", data = task_data, target = "AvgLength")
set.seed(42)
bm2 = benchmark(learners, task, resamples, measures)

write.csv(t(as.data.frame(getBMRAggrPerformances(bm2))),"results/Length_Age.csv")
write.csv(comparisontest(bm2)$Experiment2$comparison,"results/Length_Age_Comparison.csv")
write.csv(p.adjustframe(comparisontest(bm2)$Experiment2$comparison),
          "results/Length_Age_Adjusted.csv")

# Experiment 3: Circ ~ .
task_data = data[,c("Age","Sex","Ethnicity","AvgCirc")]
levels(task_data$Ethnicity)[1:5] <- "BAME"
task = makeRegrTask("Experiment3", data = task_data, target = "AvgCirc")
set.seed(42)
bm3 = benchmark(learners, task, resamples, measures)

write.csv(t(as.data.frame(getBMRAggrPerformances(bm3))),"results/Circ_Age.csv")
write.csv(comparisontest(bm3)$Experiment3$comparison,"results/Circ_Age_Comparison.csv")
write.csv(p.adjustframe(comparisontest(bm3)$Experiment3$comparison),
          "results/Circ_Age_Adjusted.csv")

# Experiment 4: Length ~ .
task_data = data[,c("Age","Sex","Ethnicity","AvgLength")]
task_data = task_data[!is.na(task_data$AvgLength),]
levels(task_data$Ethnicity)[1:5] <- "BAME"
task = makeRegrTask("Experiment4", data = task_data, target = "AvgLength")
set.seed(42)
bm4 = benchmark(learners, task, resamples, measures)

write.csv(t(as.data.frame(getBMRAggrPerformances(bm4))),"results/Length_Age.csv")
write.csv(comparisontest(bm4)$Experiment4$comparison,"results/Length_Age_Comparison.csv")
write.csv(p.adjustframe(comparisontest(bm4)$Experiment4$comparison),
          "results/Length_Age_Adjusted.csv")
