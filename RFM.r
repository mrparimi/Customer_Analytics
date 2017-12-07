rfm <- read.csv(file = "data/RFM.csv",header = TRUE)
summary(rfm)
rfm$diff_date <- Sys.Date() - as.Date(rfm$date,format="%Y%m%d")
