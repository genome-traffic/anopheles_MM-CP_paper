library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(plyr)
library(dplyr)
library(survminer)
#library(survival)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#install.packages("survminer")

survb<-read.table("tib_survival.csv", header=T, sep=",")

fit <- survfit(Surv(day, outcome) ~ strain, data = survb)
pairwise_survdiff(Surv(day, outcome) ~ strain, survb, p.adjust.method = "BH", rho = 0)
surv_median(fit, combine = FALSE)


#log-rank test
require("survival")
survbTEST <- subset(survb, strain != "hom")
survdiff(Surv(day, outcome) ~ strain, data = survbTEST, rho = 0) # log-rank


ggsurvplot(fit, data = survb)

survb_plot <- ggsurvplot(fit, data= survb, size = 1,  # change line size
           #linetype = "strata", # change line type by groups
           #palette = "Dark2", # custom color palette
           palette = c("#fac525","#EE6A36","#000000"),
           conf.int = TRUE, # Add confidence interval
           pval = FALSE, # Add p-value
           axes.offset = TRUE,
           risk.table = FALSE
)

#ggsave(file = "survival_tib.pdf", print(survb_plot))

pdf("survival_backcross.pdf", width=4, height=4)
print(survb_plot, newpage = FALSE)
dev.off()


surv<-read.table("survival.csv", header=T, sep=",")
surv <- subset(surv, feeding == "sugar")

#log-rank test
require("survival")
survTEST <- subset(surv, sex != "female")
survdiff(Surv(day, outcome) ~ strain, data = survTEST, rho = 0) # log-rank


fit2 <- survfit(Surv(day, outcome) ~ strain + sex , data = surv)
pairwise_survdiff(Surv(day, outcome) ~ strain + sex, surv, p.adjust.method = "BH", rho = 0)
surv_median(fit2, combine = FALSE)

ggsurvplot(fit2, data = surv)

surv_plot <- ggsurvplot(fit2, data= surv, size = 1,  # change line size
           #linetype = "strata", # change line type by groups
           palette = "Dark2", # custom color palette
           #palette = c("#000000","#EE6A36","#5DC863"),
           conf.int = TRUE, # Add confidence interval
           pval = FALSE, # Add p-value
           xlim = c(0, 30),
           axes.offset = TRUE
)

pdf("survival.pdf", width=4, height=4)
print(surv_plot, newpage = FALSE)
dev.off()

