library(data.table)
library(ggplot2)

data <- readRDS("data_recessions.RDS")
lag_nr <- 12
data <- setDT(data)[, paste0("SpreadLag", lag_nr) := shift(Spread, lag_nr)][]
data <- setDT(data)[, paste0("SP500RELag", lag_nr) := shift(SP500RE,lag_nr)][]
data <- setDT(data)[, paste0("MICSLag", lag_nr) := shift(MICS, lag_nr)][]
data <- setDT(data)[, paste0("PMILag", lag_nr) := shift(PMI, lag_nr)][]
data <- setDT(data)[, paste0("WTILag", lag_nr) := shift(WTI, lag_nr)][]
data <- setDT(data)[, paste0("FEDFUNDSLag", lag_nr) := shift(FEDFUNDS, lag_nr)][]
data <- data[13:694,]

data$Indicator <- ifelse(data$Indicator == "NoBust", "Nicht-Rezessions-Monat", "Rezessions-Monat")

SpreadDist <- ggplot(data, aes(x = SpreadLag12)) + geom_density(fill = "#4CA3DD", col = "white") + geom_vline(xintercept = 0, color = "grey") + 
  theme_classic() + theme(text = element_text(family = "Crimson", size = 15)) + theme(panel.spacing = unit(2, "lines")) + xlab("Spread") + ylab("Prozent der Beobachtungen") +
  scale_y_continuous(labels = scales::percent) + facet_grid(Indicator~.) + scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4))
SpreadDist
  

PMIDist <- ggplot(data, aes(x = PMILag12)) + geom_density(fill = "#4CA3DD", col = "white") + 
  theme_classic() + theme(text = element_text(family = "Crimson", size = 15)) + theme(panel.spacing = unit(2, "lines")) + xlab("PMI %-Änderung") + ylab("Prozent der Beobachtungen") +
  scale_y_continuous(labels = scales::percent) + facet_grid(Indicator~.)
PMIDist

MICSDist <- ggplot(data, aes(x = MICSLag12)) + geom_density(fill = "#4CA3DD", col = "white") + 
  theme_classic() + theme(text = element_text(family = "Crimson", size = 15)) + theme(panel.spacing = unit(2, "lines")) + xlab("MICS %-Änderung") + ylab("Prozent der Beobachtungen") +
  scale_y_continuous(labels = scales::percent) + facet_grid(Indicator~.)
MICSDist

FEDFUNDSDist <- ggplot(data, aes(x = FEDFUNDSLag12)) + geom_density(fill = "#4CA3DD", col = "white") + 
  theme_classic() + theme(text = element_text(family = "Crimson", size = 15)) + theme(panel.spacing = unit(2, "lines")) + xlab("Level der Federal Funds Rate") + ylab("Prozent der Beobachtungen") +
  scale_y_continuous(labels = scales::percent) + facet_grid(Indicator~.)
FEDFUNDSDist

SP500REDist <- ggplot(data, aes(x = SP500RELag12)) + geom_density(fill = "#4CA3DD", col = "white") + 
  theme_classic() + theme(text = element_text(family = "Crimson", size = 15)) + theme(panel.spacing = unit(2, "lines")) + xlab("1Y Aktienrendite") + ylab("Prozent der Beobachtungen") +
  scale_y_continuous(labels = scales::percent) + facet_grid(Indicator~.)
SP500REDist

WTIDist <- ggplot(data, aes(x = WTILag12)) + geom_density(fill = "#4CA3DD", col = "white") + 
  theme_classic() + theme(text = element_text(family = "Crimson", size = 15)) + theme(panel.spacing = unit(2, "lines")) + xlab("1M %-Änderung Ölpreis") + ylab("Prozent der Beobachtungen") + facet_grid(Indicator~.) + 
  scale_x_continuous(breaks = c(-0.2,0.2))
WTIDist