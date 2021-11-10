library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(plyr)
library("extrafont")
loadfonts()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# ---------- data input ----------

h<-read.table("homing.csv", header=T, sep=",")
h$total <- h$positive + h$negative
h$transmission <- (h$positive/h$total)*100

h_sum <- ddply(h, c("cross","transhemizygote"), summarise,
              mean = mean(transmission),
              sd   = sd(transmission),
              se   = sd / sqrt(3))

h_sum$condition <- paste(h_sum$cross,h_sum$transhemizygote)
h$condition <- paste(h$cross,h$transhemizygote)

# ---------- plot ----------

dev.off()
ph <- ggplot(data=h_sum, aes(x=condition, y=mean)) + geom_bar(stat='identity') +
geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2,position=position_dodge(.9)) + theme_classic() +
geom_jitter(data=h, width = 0.1,aes(x = condition, y = transmission))
plot(ph)
ggsave("panel_4A.pdf", width = 10, height = 10, units = "cm",family = "Arial")

# ---------- stats ----------

an <- aov(h$transmission ~ h$condition)
summary(an)
TukeyHSD(an)
