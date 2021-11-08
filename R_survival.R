library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(plyr)
library(dplyr)
library(survminer)
library(survival)
#require("survival")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# ---------- survival (backcross, bloodmeals) ----------

survb<-read.table("survival_backcross.csv", header=T, sep=",")

fit <- survfit(Surv(day, outcome) ~ strain, data = survb)
pairwise_survdiff(Surv(day, outcome) ~ strain, survb, p.adjust.method = "BH", rho = 0)
surv_median(fit, combine = FALSE)

ggsurvplot(fit, data = survb)

survb_plot <- ggsurvplot(fit, data= survb, size = 1,
           palette = "Dark2", 
           conf.int = TRUE,
           pval = FALSE,
           axes.offset = TRUE,
           risk.table = FALSE
)
pdf("panel_3E.pdf", width=4, height=4)
print(survb_plot, newpage = FALSE)
dev.off()

#log-rank test
survbTEST <- subset(survb, strain != "hom")
#survbTEST <- subset(survb, strain != "wt")
#survbTEST <- subset(survb, strain != "het")
survdiff(Surv(day, outcome) ~ strain, data = survbTEST, rho = 0) 


# ---------- survival (sugar only, both sexes) ----------

surv<-read.table("survival.csv", header=T, sep=",")

fit2 <- survfit(Surv(day, outcome) ~ strain + sex , data = surv)
pairwise_survdiff(Surv(day, outcome) ~ strain + sex, surv, p.adjust.method = "BH", rho = 0)
surv_median(fit2, combine = FALSE)

ggsurvplot(fit2, data = surv)

surv_plot <- ggsurvplot(fit2, data= surv, size = 1, 
           palette = "Dark2", 
           conf.int = TRUE, 
           pval = FALSE,
           xlim = c(0, 30),
           axes.offset = TRUE
)
pdf("panel_3D.pdf", width=4, height=4)
print(surv_plot, newpage = FALSE)
dev.off()

#log-rank test
survTEST <- subset(surv, sex != "female")
#survTEST <- subset(surv, strain == "WT")
survdiff(Surv(day, outcome) ~ strain, data = survTEST, rho = 0) 
#survdiff(Surv(day, outcome) ~ sex, data = survTEST, rho = 0)
