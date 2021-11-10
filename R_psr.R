library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(plyr)
library("extrafont")
loadfonts()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# ---------- data input ----------

psr<-read.table("psr.csv", header=T, sep=",")

psr$total <- psr$males + psr$females
psr$sr <- (psr$males/psr$total)*100

psr_sum <- ddply(psr, c("strain"), summarise,
              mean = mean(sr),
              sd   = sd(sr),
              se   = sd / sqrt(3))

# ---------- plot ----------

dev.off()
ppsr <- ggplot(data=psr_sum, aes(x=strain, y=mean)) + geom_bar(stat='identity') +
geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2,position=position_dodge(.9)) +
geom_jitter(data=psr, width = 0.1,aes(x = strain, y = sr)) +
theme_classic(base_size = 18) +
theme(legend.position="none")
plot(ppsr)
ggsave("panel_3C.pdf", width = 8, height = 10, units = "cm",family = "Arial")

# ---------- stats ----------

chisq.test(c(psr$males,psr$females))

