library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(plyr)
library(dplyr)
library(Hmisc)
#install.packages("extrafont")
library("extrafont")
#font_import()
#library(remotes)
#remotes::install_version("Rttf2pt1", version = "1.3.8")
#extrafont::font_import()
loadfonts()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



# ---------- data input ----------

ooi<-read.table("oocyst_intensity.csv", header=T, sep=",")
describe(ooi)

# ---------- plot ----------

ooip1 <- ggplot(ooi, aes(x = Strain, y = Count)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, aes(col = Strain)) +
  scale_x_discrete(limits = rev)+
  theme_classic(base_size = 18) +
  scale_color_manual(values=c("#EE6A36","#EE6A36","#000000")) +
  theme(legend.position="none")
plot(ooip1)
ggsave("panel_2B.pdf", width = 8, height = 10, units = "cm",family = "Arial")

# ---------- stats ----------

# independent 2-group Mann-Whitney U Test
ooi_MW <- subset(ooi, Strain != "Gam1-MM")
#ooi_MW <- subset(ooi, Strain != "MM-CP")
wilcox.test(data=ooi_MW,Count~Strain)

