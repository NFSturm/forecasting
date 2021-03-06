library(plotly)
library(data.table)

data <- readRDS("data_recessions.RDS")
lag_nrs <- c(3,6, 12)
data <- setDT(data)[, paste0("SpreadLag", lag_nrs) := shift(Spread, lag_nrs)][]
data <- setDT(data)[, paste0("SP500RELag", lag_nrs) := shift(SP500RE,lag_nrs)][]
data <- setDT(data)[, paste0("MICSLag", lag_nrs) := shift(MICS, lag_nrs)][]
data <- setDT(data)[, paste0("PMILag", lag_nrs) := shift(PMI, lag_nrs)][]
data <- setDT(data)[, paste0("WTIDIFF1MLag", lag_nrs) := shift(WTIDIFF1M, lag_nrs)][]
data <- setDT(data)[, paste0("FEDFUNDSLag", lag_nrs) := shift(FEDFUNDS, lag_nrs)][]

data <- data[13:694,]

data$Indicator <- ifelse(data$Indicator == "Bust", "Recession", "No Recession")

p <- plot_ly(data, x = ~SP500RELag12, y = ~MICSLag12, z = ~PMILag12, color = ~Indicator, colors = c("#63ace5", "#2a4d69")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Annual Stock Returns'),
                      yaxis = list(title = 'Consumer Confidence (Michigan)'),
                      zaxis = list(title = 'Purchasing Manager Index')),
                      title = "Recessions vs. Growth")
p

q <- plot_ly(data, x = ~SP500RELag12, y = ~MICSLag12, z = ~PMILag12, color = ~Indicator, colors = c("#63ace5", "#2a4d69")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Annual Stock Returns'),
                      yaxis = list(title = 'Consumer Confidence (Michigan)'),
                      zaxis = list(title = 'Purchasing Manager Index')),
         title = "Recessions vs. Growth")
q

r <- plot_ly(data, x = ~SpreadLag12, y = ~SP500RELag12, color = ~Indicator, colors = c("#63ace5", "#2a4d69")) %>%
  add_markers() %>%
  layout(xaxis = list(title = 'Yield Spread 10Y-3M'), yaxis = list(title = 'S&P500 Annual Stock Returns'), title = 'Recessions vs. Growth')
r

s <- plot_ly(data, x = ~Spread, y = ~SP500RE, color = ~Indicator, colors = c("#63ace5", "#2a4d69")) %>%
  add_markers() %>%
  layout(xaxis = list(title = 'Yield Spread 10Y-3M'), yaxis = list(title = 'S&P500 Annual Stock Returns'), title = 'Recessions vs. Growth')