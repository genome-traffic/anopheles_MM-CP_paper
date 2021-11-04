library(ggplot2)
library(RColorBrewer)
#library(Hmisc)
#library(plyr)
library(dplyr)
library("extrafont")
loadfonts()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# --- data input
os_all<-read.table("oocyst_size_backcross.csv", header=T, sep=",")


# ---------- summarize ----------
# 
# oos <- ddply(oo, c("Strain", "SB","dpi"), summarise,
#                N    = length(Size),
#                mean = mean(Size),
#               median = median(Size),
#                sd   = sd(Size),
#                se   = sd / sqrt(N))


# ---------- plot ----------

pPf1 <- ggplot(os_all, aes(x = Strain, y = Size)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_classic(base_size = 18) +
  scale_x_discrete() +
  geom_jitter(width = 0.1, aes(col = Strain)) +
  scale_color_manual(values=c("#000000","#EE6A36","#EE6A36")) +
  theme(legend.position="none")
plot(pPf1)
ggsave("Figure S2.pdf", width = 10, height = 10, units = "cm",family = "Arial")

# ---------- stats ----------

os <- subset(os_all, Strain != "MM-CP/MM-CP")
sizeWT <- subset(os, Strain == "+/+")
sizeCPhet <- subset(os, Strain == "MM-CP/+")
var.test(sizeWT$Size, sizeCPhet$Size)
sizetest <- t.test(sizeWT$Size, sizeCPhet$Size, var.equal = FALSE)
print(sizetest)


