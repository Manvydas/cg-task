# BCH ---------
tt1 <- Sys.time()
# API limited to 20 items per request and 20 requests per minute
ids.20 <- split2(unique(dat$BCH$txid), 20)
dat.bch <- data.table()
ind <- 0
t1 <- Sys.time()
# Call API for each ids split
for (id in ids.20) {
  ind <- ind + 1
  # Get data.
  dd <- getBCH(id)
  dat.bch <- rbind(dat.bch, dd)
  # Check if requests limit is not breached
  if ((ind %% 20) == 0) {
    t2 <- 60 - (Sys.time() - t1)
    t1 <- Sys.time() # reset timer
    if (t2 > 0) {
      cat("Sleeping for", t2, "seconds.\n")
      Sys.sleep(t2) # sleep due to limited requests per minute
    }
  }
}

# Variables preparation
nm.time <- grep("time", names(dat.bch), value = TRUE) # "time" is the one we need
nm.time.new <- gsub("details.", "", nm.time)
dat.bch %>% 
  .[ , (nm.time.new) := lapply(.SD, as.POSIXct, origin = "1970-01-01"), .SDcols = nm.time] %>% 
  .[ , value := lapply(details.vout, function(x) sum(x$value)), by = txid] %>% 
  .[ , currency := "BCH"]
tt2 <- Sys.time()
# Print some information and save
if (nrow(dat.bch) == length(unique(dat$BCH$txid))) {
  cat("BCH code completed in", tt2-tt1,", saving the data.\n")
  print(object.size(dat.bch), units = "Mb")
  save(dat.bch, file = "data/dat_bch2.RData")
} else cat("Please check the code, not all txids completed.\n")

