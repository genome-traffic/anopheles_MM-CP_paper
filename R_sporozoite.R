library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(plyr)
library(dplyr)

#library(drc)
#library(nlme)
#install.packages("devtools")
#library(devtools)
#devtools::install_github("onofriAndreaPG/aomisc")
#library(aomisc)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# --- data input
sporo<-read.table("sporo.csv", header=T, sep=",")
sporo$infected <- ifelse(sporo$CytB>0,1,0)

# --- PREVALENCE
prev <- ddply(sporo, c("Strain", "SB","Day"), summarise,
               N    = length(infected),
               mean = mean(infected),
               sd   = sd(infected),
               se   = sd / sqrt(N))

prev2 <- ddply(sporo, c("Strain", "SB"), summarise,
              N    = length(infected),
              mean = mean(infected),
              sd   = sd(infected),
              se   = sd / sqrt(N))

#### Stats prev ####
hist(prev$mean)

bm <- glm(data=prev,mean ~ Strain + Day + SB, family="binomial")
summary(bm)
plogis(confint(bm))

qbm <- glm(data=prev,mean ~ Strain + Day + SB, family="quasibinomial")
summary(qbm)
plogis(confint(qbm))

qbm2 <- glm(data=prev,mean ~ Strain + Day, family="quasibinomial")
summary(qbm2)
plogis(confint(qbm2))

RES = residuals(m3)  
qqnorm(RES)  
qqline(RES)
#### end ####

#### Plot prev ####
group.colors <- c(A = "#EE6A36", B = "#000000")
plot1 <- ggplot(prev, aes(x = Day, y = mean, ymin = mean-se, ymax = mean+se, color=Strain)) + 
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
plot(plot1)
#ggsave("prevalence.pdf", width = 16, height = 8, units = "cm")
dev.off()

plot2 <- ggplot(prev, aes(x = Day, y = mean, ymin = mean-se, ymax = mean+se, color=Strain)) + 
  geom_pointrange() +
  geom_smooth(aes(group=Strain, colour=Strain), method="glm",
              span = 1,
              size = 1.1,
              method.args = list(family = "quasibinomial"),
              se=F) + 
  facet_wrap(~prev$SB) +
  scale_x_continuous(breaks = seq(10,16,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + theme_bw() +
  ylab("infection prevalence (%)") + scale_color_manual(values=c("#EE6A36", "#000000", "#56B4E9")) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size = 12, face="bold"))
plot(plot2)

group.colors <- c(A = "#EE6A36", B = "#000000")
plot3 <- ggplot(prev, aes(x = Day, y = mean, ymin = mean-se, ymax = mean+se, color=Strain)) + 
  geom_pointrange() +
  geom_line(aes(y=fitted(m3))) +
  facet_wrap(~prev$SB) +
  scale_x_continuous(breaks = seq(10,16,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + theme_bw() +
  ylab("infection prevalence (%)") + scale_color_manual(values=c("#EE6A36", "#000000", "#56B4E9")) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size = 12, face="bold"))
plot(plot3)
#### end ####



# --- INTENSITY

int <- subset(sporo, CytB>0)

#### stats int ####
hist(int)
#int <- subset(int, Day==16)
#int <- subset(int, SB=="single bloodmeal")

intWT <- subset(int, Strain == "WT")
intCP <- subset(int, Strain == "MM-CP")
var.test(intWT$CytB, intCP$CytB)
#t-test or welsh test
#If the F-test returns a p < 0.05, then you can assume that the variances of the two groups are different (heteroscedasticity).
#In this case, you can run a Welch t-statistic. Simply set var.equal = FALSE.
tint <- t.test(intWT$CytB, intCP$CytB, var.equal = FALSE)
print(tint)
print (median(intWT$CytB)/median(intCP$CytB))

intsum <- ddply(int, c("Strain", "SB"), summarise,
               N    = length(CytB),
               mean = mean(CytB),
               sd   = sd(CytB),
               se   = sd / sqrt(N))

print(intsum$mean[3]/intsum$mean[1])
print(intsum$mean[4]/intsum$mean[2])

print((intsum$mean[3]-intsum$mean[1])/intsum$mean[3])*100
print((intsum$mean[4]-intsum$mean[2])/intsum$mean[4])*100

#### end ####

#### Plot int ####
dev.off()
group.colors <- c(A = "#EE6A36", B = "#000000")

ip0 <- ggplot(int, aes(x = Strain, y = log10(CytB))) + 
  geom_boxplot() 
plot(ip0)

ip1 <- ggplot(int, aes(x = Strain, y = log10(CytB))) + 
  geom_boxplot() +
  facet_wrap(~int$SB) 
plot(ip1)

ip2 <- ggplot(int, aes(x = Strain, y = log10(CytB))) + 
  geom_boxplot() +
  geom_jitter(width = 0.1, aes(col = Strain)) +
  theme_bw() +
  ylab("infection intensity \n log10(CytB ng/mosquito)") +
  scale_color_manual(values=c("#EE6A36", "#000000", "#56B4E9")) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12, face="bold"))
plot(ip2)
ggsave("intensity.pdf", width = 10, height = 8, units = "cm")

ip3 <- ggplot(int, aes(x = Strain, y = log10(CytB))) + 
  geom_boxplot() +
  geom_jitter(width = 0.1, aes(col = Strain)) +
  theme_bw() +
  ylab("infection intensity \n log10(CytB ng/mosquito)") +
  scale_color_manual(values=c("#EE6A36", "#000000", "#56B4E9")) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12, face="bold"))+scale_x_discrete(limits = rev)+
  facet_wrap(~int$SB)
plot(ip3)
ggsave("intensitygroups.pdf", width = 14, height = 8, units = "cm")
#### end ####
