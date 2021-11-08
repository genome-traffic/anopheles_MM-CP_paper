library(ggplot2)
library(RColorBrewer)
library(dplyr)
library("extrafont")
loadfonts()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# ---------- data input ----------

os_all<-read.table("oocyst_size.csv", header=T, sep=",")
os_all$StrainSB <- paste(os_all$Strain,os_all$SB)

# ---------- P. falciparum ----------

os_Pf <- subset(os_all, Species == "Pf")

# ---------- plot ----------

pPf1 <- ggplot(os_Pf, aes(x = StrainSB, y = Size)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(limits = rev) +
  geom_jitter(width = 0.1, aes(col = StrainSB)) +
  facet_wrap(~dpi, scales="free_y") + theme_classic(base_size = 18) +
  scale_color_manual(values=c("#EE6A36","#EE6A36","#000000")) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(legend.position="none")
  plot(pPf1)
ggsave("panel_2D.pdf", width = 24, height = 10, units = "cm",family = "Arial")

# ---------- stats ----------

os_Pf <- subset(os_Pf, StrainSB != "MM-CP single bloodmeal")
os_Pf <- subset(os_Pf, dpi == "15")
sizeWT <- subset(os_Pf, Strain == "WT")
sizeCP <- subset(os_Pf, Strain == "MM-CP")
var.test(sizeWT$Size, sizeCP$Size)
sizetest <- t.test(sizeWT$Size, sizeCP$Size, var.equal = FALSE)
print(sizetest)


# ---------- P. berghei ----------

os_Pb <- subset(os_all, Species == "Pb")

# ---------- plot ----------

pPb1 <- ggplot(os_Pb, aes(x = StrainSB, y = Size)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(limits = rev) +
  geom_jitter(width = 0.1, aes(col = StrainSB)) +
  theme_classic(base_size = 18) +
  scale_color_manual(values=c("#EE6A36","#000000")) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  theme(legend.position="none")
plot(pPb1)
ggsave("panel_2E.pdf", width = 7, height = 10, units = "cm",family = "Arial")

# ---------- stats ----------

sizeWT <- subset(os_Pb, Strain == "WT")
sizeCP <- subset(os_Pb, Strain == "MM-CP")
var.test(sizeWT$Size, sizeCP$Size)
sizetest <- t.test(sizeWT$Size, sizeCP$Size, var.equal = FALSE)
print(sizetest)



