# BTC ---------
ids <- unique(dat$BTC$txid)
dat.btc <- list()
for (i in seq_along(ids)) {
  
  id <- ids[i]
  url <- paste0("https://blockchain.info/rawtx/", id)
  dd.raw <- httr::GET(url)
  # cat(i, ":", dd.raw$status_code, "\n")
  
  # Limit exceed error handling
  if (dd.raw$status_code != 200) {
    cat("Sleep for a minute.\n")
    Sys.sleep(60)
    # Rerun for the same txid
    dd.raw <- httr::GET(url)
    # cat(i, ":", dd.raw$status_code, "\n")
    # Check again if limit reset
    if (dd.raw$status_code != 200) return("Limit breached. Maybe try later.")
  }
  
  dat.btc[[i]] <- jsonlite::fromJSON(rawToChar(dd.raw$content), flatten = T)
  
}

# Some data prep
dat.btc <- lapply(dat.btc, as.data.table)
dat.btc <- rbindlist(dat.btc, fill = TRUE)
dat.btc[ , currency := "BTC"]
dat.btc[ , time := as.POSIXct(time, origin = "1970-01-01")]
# All bitcoin values are in Satoshi i.e. divide by 100000000 to get the amount in BTC
dat.btc[ , value := sum(out.value/100000000), by=hash]

save(dat.btc, file = "data/dat_btc.RData")

# returns only transaction value
# link <- "https://blockchain.info/q/txtotalbtcoutput/4eefe3e2b66081dfb6a050b00dc5d88350b9cae0ad2344db90f503c9cf24f4f9"
# returns raw transaction info
# link <- "https://blockchain.info/rawtx/4eefe3e2b66081dfb6a050b00dc5d88350b9cae0ad2344db90f503c9cf24f4f9"
# gggg <- httr::GET(link)
# gggg2 <- jsonlite::fromJSON(rawToChar(gggg$content), flatten=T)
