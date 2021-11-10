library(ggplot2)
#library(RColorBrewer)
library(dplyr)
library(plyr)
library("extrafont")
loadfonts()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# ---------- data input ----------

f<-read.table("fertility.csv", header=T, sep=",")
f <- subset(f, fertilized != "no")
f$hatch <- (f$larvae/f$eggs)*100

# f_sum <- ddply(f, c("strain", "replicate"), summarise,
#               N    = length(eggs),
#               mean = mean(eggs),
#               sd   = sd(eggs),
#               se   = sd / sqrt(N))

# ---------- plot (eggs) ----------

pf <- ggplot(f, aes(x = strain, y = eggs)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(limits = rev) +
  geom_jitter(width = 0.1, aes(col = strain)) +
  theme_classic(base_size = 18) +
  scale_color_manual(values=c("#EE6A36","#000000"))  +
  theme(legend.position="none")
plot(pf)
ggsave("panel_3A.pdf", width = 10, height = 10, units = "cm",family = "Arial")

# ---------- stats (eggs) ----------

fWT <- subset(f, strain == "WT")
fCP <- subset(f, strain == "MM-CP")
ttest <- t.test(fWT$eggs, fCP$eggs, var.equal = FALSE)
print(ttest)
ks.test(fWT$eggs, fCP$eggs)

# ---------- plot (hatch) ----------

f <- subset(f, eggs != "0")

ph <- ggplot(f, aes(x = strain, y = hatch)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(limits = rev) +
  geom_jitter(width = 0.1, aes(col = strain)) +
  theme_classic(base_size = 18) +
  scale_color_manual(values=c("#EE6A36","#000000"))  +
  theme(legend.position="none")
plot(ph)
ggsave("panel_3B.pdf", width = 10, height = 10, units = "cm",family = "Arial")

# ---------- stats (hatch) ----------

hWT <- subset(f, strain == "WT")
hCP <- subset(f, strain == "MM-CP")
ttest <- t.test(hWT$hatch, hCP$hatch, var.equal = FALSE)
print(ttest)
ks.test(hWT$hatch, hCP$hatch)
