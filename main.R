rm(list = ls())
# Load libraries
library(dplyr)
library(data.table)

# Source functions
source("functions.R")

# Set parameters
rerun_bch <- FALSE # set TRUE to rerun BCH data extraction
rerun_btc <- FALSE # set TRUE to rerun BTC data extraction
rerun_xrp <- FALSE # set TRUE to rerun XRP data extraction
rerun_eth <- FALSE # set TRUE to rerun ETH data extraction
rerun_trx <- FALSE # set TRUE to rerun TRX data extraction
curr.to = c("EUR", "USD", "GBP", "SGD")

# Load main dataset ---------
path <- "CG_Data analyst task spreadsheet.xlsx"
dat <- path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(readxl::read_excel, path = path) %>% 
  purrr::map(data.table::as.data.table)

# Load USD exchange rates from previously created data
load("data/exc_usd.RData")

# Rerun clockchain information extraction
if (rerun_bch == TRUE) source("R/prep_bch.R")
if (rerun_btc == TRUE) source("R/prep_btc.R")
if (rerun_xrp == TRUE) source("R/prep_xrp.R")
if (rerun_eth == TRUE) source("R/prep_eth.R")
if (rerun_trx == TRUE) source("R/prep_trx.R")

# BCH ---------
# Load previously prepared data, select variables
load("data/dat_bch.RData")
dat.bch <- unique(dat.bch[ , .(txid, time, value, currency)])
# Merge value and transaction date (time)
dat$BCH <- unique(merge(dat$BCH, # not only unique values were provided
                 dat.bch,
                 by = "txid", all.x = T))
# Join exchange rates
dat$BCH %>% 
  .[ , date := as.Date(time)] %>% 
  .[ , hourly := lubridate::round_date(time, unit = "hours")]

exc.bch.usd <- fread("https://www.cryptodatadownload.com/cdd/Bitstamp_BCHUSD_1h.csv")

dat$BCH <- unique(merge(dat$BCH,
                        exc.bch.usd[ , .(hourly = date, open)],
                        by = "hourly", all.x = T))
dat$BCH[ , USD_value := value*open]

curr.to.2 <- setdiff(curr.to, "USD")
dat$BCH <- unique(merge(dat$BCH,
                        exc.usd[ , c("date", curr.to), with=F],
                        by = "date", all.x = T))
dat$BCH[ , paste0(curr.to.2, "_value") := lapply(.SD, function(x) USD_value*x), .SDcols = curr.to.2]
# All time high
dat$BCH$high.dt <- unique(dat$BCH[max(open) == open]$hourly)
dat$BCH[ , time.dev.h := difftime(hourly, high.dt, units = "days")]

# BTC ---------
# Load previously prepared data, select variables
load("data/dat_btc.RData")
dat.btc <- unique(dat.btc[ , .(txid = hash, time, value, currency)])
# Merge value and transaction date (time)
dat$BTC <- unique(merge(dat$BTC, # not only unique values were provided and not all values found
                        dat.btc,
                        by = "txid", all.x = T))
dat$BTC %>% 
  .[ , date := as.Date(time)] %>% 
  .[ , hourly := lubridate::round_date(time, unit = "hours")]# Join daily exchange rates
# exc.btc <- getExc(dat, "BTC")
# save(exc.btc, file = "data/exc_btc.RData")
load("data/exc_btc.RData")
curr.to.c <- paste0(curr.to, "_c") # corrected values
dat$BTC <- unique(merge(dat$BTC,
                        exc.btc[ , c("date", curr.to.c), with=F],
                        by = "date", all.x = T))
dat$BTC[ , paste0(gsub("_c", "", curr.to.c), "_value") := lapply(.SD, function(x) value*x), .SDcols = curr.to.c]

# All time high
exc.btc.usd <- fread("https://www.cryptodatadownload.com/cdd/Bitstamp_BTCUSD_1h.csv")
dat$BTC <- unique(merge(dat$BTC,
                        exc.btc.usd[ , .(hourly = date, open)],
                        by = "hourly", all.x = T))
dat$BTC[ , USD_value.h := value*open]
dat$BTC$high.dt <- unique(dat$BTC[max(open, na.rm = T) == open]$hourly)
dat$BTC[ , time.dev.h := difftime(hourly, high.dt, units = "days")]


# XRP ---------
# Load previously prepared data, select variables
load("data/dat_xrp.RData")
dat.xrp <- unique(dat.xrp[ , .(txid = hash, time, value, currency)])
# Merge value and transaction date (time)
dat$XRP <- unique(merge(dat$XRP,
                        dat.xrp,
                        by = "txid", all.x = T))
# Join exchange rates
dat$XRP %>% 
  .[ , date := as.Date(time)] %>% 
  .[ , hourly := lubridate::round_date(time, unit = "hours")]

exc.xrp.usd <- fread("https://www.cryptodatadownload.com/cdd/Bitstamp_XRPUSD_1h.csv")

dat$XRP <- unique(merge(dat$XRP,
                        exc.xrp.usd[ , .(hourly = date, open)],
                        by = "hourly", all.x = T))
dat$XRP[ , USD_value := value*open]

curr.to.2 <- setdiff(curr.to, "USD")
dat$XRP <- unique(merge(dat$XRP,
                        exc.usd[ , c("date", curr.to), with=F],
                        by = "date", all.x = T))
dat$XRP[ , paste0(curr.to.2, "_value") := lapply(.SD, function(x) USD_value*x), .SDcols = curr.to.2]
# All time high
dat$XRP$high.dt <- unique(dat$XRP[max(open) == open]$hourly)
dat$XRP[ , time.dev.h := difftime(hourly, high.dt, units = "days")]

# Export results
write.csv(dat$BCH,"data/results/res_BCH.csv", row.names = FALSE)
write.csv(dat$BTC,"data/results/res_BTC.csv", row.names = FALSE)
write.csv(dat$XRP,"data/results/res_XRP.csv", row.names = FALSE)
