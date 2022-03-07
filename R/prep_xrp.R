# XRP ---------
ids <- unique(dat$XRP$txid)
dat.xrp <- list()
for (i in seq_along(ids)) {
  
  id <- ids[i]
  dd.raw <- getXRP(id)
  cat(i, ":", dd.raw$status_code, "\n")
  # Limit exceed error handling
  if (dd.raw$status_code != 200) {
    cat("Sleep for a minute.\n")
    Sys.sleep(60)
    # Rerun for the same txid
    dd.raw <- getXRP(id)
    # cat(i, ":", dd.raw$status_code, "\n")
    # Check again if limit reset
    if (dd.raw$status_code != 200) return("Limit breached. Maybe try later.")
  }
  
  dat.xrp[[i]] <- jsonlite::fromJSON(rawToChar(dd.raw$content), flatten = T)$result
  
}

# Some data prep
dat.xrp <- lapply(dat.xrp, as.data.table)
dat.xrp <- rbindlist(dat.xrp, fill = TRUE)
dat.xrp[ , currency := "XRP"]
dat.xrp[ , time := as.POSIXct(date, origin = "2000-01-01")]
dat.xrp[ , value := sum(as.numeric(Amount)/1000000), by=hash]

save(dat.xrp, file = "data/dat_xrp.RData")

  
# curl -H 'Content-Type: application/json' -d '{"method":"tx", "params":[{"transaction":"E08D6E9754025BA2534A78707605E0601F03ACE063687A0CA1BDDACFCD1698C7", "binary":false}]}' https://s1.ripple.com:51234/
