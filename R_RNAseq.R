library (ggplot2)
library(dplyr)
library(ggrepel)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---------- data input ----------

timepoint = "6" # "0","6" or "20"
data <- read.table("0_all.csv", sep=",", header=TRUE)
data <- read.table(paste0(timepoint, "_all.csv"), sep=",", header=TRUE)
golist <- read.table("GO.csv", sep=",", header=TRUE)

data$go <- is.element(data$gene_id,golist$gene)
threshold_OE <- data$padjust < 0.01
data$threshold <- threshold_OE
data[data$gene_chr=="AgamP4_Mt",]$go <- "D"
data[data$go=="TRUE",]$go <- "C"
data[data$go=="FALSE",]$go <- "B"
data[data$padjust > 0.05,]$go <- "A"
data$delabel <- data$gene_id
data[-log10(data$padjust) < 15,]$delabel <- NA
data <- with(data, data[order(go, padjust),])

# ---------- plot ----------

rhg_cols <- c("#cfcfcf", "#c29993", "#b51600", "#4d54db", "#F8A31B", "#E2C59F", "#B6C5CC", "#8E9CA3", "#556670", "#000000")
rsp <- ggplot(data, aes(x=log2foldchange, y=-log10(padjust), colour=go, label=delabel)) +
  geom_point(size=2) +
  geom_text_repel(size=3) +
  xlab("log2 fold change") + 
  ylab("-log10 adjusted p-value") + theme_bw() +  xlim(-10, 10) + #+ ylim(0, 100) +
  theme(legend.position = "none",
        plot.title = element_text(size = rel(1.5), hjust = 0.5),
        axis.title = element_text(size = rel(1.25)))  + scale_colour_manual(values = rhg_cols)

plot(rsp)
ggsave(paste0(timepoint, "_hours_plot.pdf"), width = 8, height = 8, units = "cm")
