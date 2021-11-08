library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(plyr)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# ----------- data input ------------

sporo<-read.table("sporo.csv", header=T, sep=",")
sporo$infected <- ifelse(sporo$CytB>0,1,0)

#### Prevalence ####

prev <- ddply(sporo, c("Strain", "SB","Day"), summarise,
               N    = length(infected),
               mean = mean(infected),
               sd   = sd(infected),
               se   = sd / sqrt(N))

# ---------- stats ----------

qbm <- glm(data=prev,mean ~ Strain + Day + SB, family="quasibinomial")
summary(qbm)
plogis(confint(qbm))

qbm2 <- glm(data=prev,mean ~ Strain + Day, family="quasibinomial")
summary(qbm2)
plogis(confint(qbm2))

qbm2_res = residuals(qbm2)  
qqnorm(qbm2_res)  
qqline(qbm2_res)

# ---------- plot ----------

p1 <- ggplot(prev, aes(x = Day, y = mean, ymin = mean-se, ymax = mean+se, color=Strain)) + 
  geom_pointrange() +
  geom_smooth(aes(group=Strain, colour=Strain), method="glm",
             span=1,
             size = 1.1,
             method.args = list(family = "quasibinomial"),
             level=0.95,
             alpha=0.2,
             se=F) + 
  facet_wrap(~prev$SB) +
  scale_x_continuous(breaks = seq(10,16,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + theme_bw() +
  ylab("infection prevalence (%)") + scale_color_manual(values=c("#EE6A36", "#000000", "#56B4E9")) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size = 12, face="bold"))
plot(p1)
ggsave("panel_2G.pdf", width = 16, height = 8, units = "cm")
dev.off()



#### Intensity ####

int <- subset(sporo, CytB>0)

# ---------- plot ----------

ip <- ggplot(int, aes(x = Strain, y = log10(CytB))) + 
  geom_boxplot() +
  geom_jitter(width = 0.1, aes(col = Strain)) +
  theme_bw() +
  ylab("infection intensity \n log10(CytB ng/mosquito)") +
  scale_color_manual(values=c("#EE6A36", "#000000", "#56B4E9")) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12, face="bold"))+scale_x_discrete(limits = rev)+
  facet_wrap(~int$SB)
plot(ip)
ggsave("panel_2H.pdf", width = 14, height = 8, units = "cm")
dev.off()

# ---------- stats ----------

int <- subset(int, SB=="single bloodmeal")
#int <- subset(int, SB!="single bloodmeal")
intWT <- subset(int, Strain == "WT")
intCP <- subset(int, Strain == "MM-CP")
var.test(intWT$CytB, intCP$CytB)

#welsh test
tint <- t.test(intWT$CytB, intCP$CytB, var.equal = FALSE)
print(tint)
