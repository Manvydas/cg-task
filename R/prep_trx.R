# TRX ---------
ids <- unique(dat$TRX$txid)
out <- list()
for (i in seq_along(ids)) {
  
  id <- ids[i]
  url <- paste0("https://apilist.tronscan.org/api/transaction-info?hash=", id)
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
    out[[i]] <- jsonlite::fromJSON(rawToChar(dd.raw$content), flatten = T)
  
}

out.f <- c(outt, outt2, out)

nm.time <- grep("time", names(dat.bch), value = TRUE) # "time" is the one we need
nm.time.new <- gsub("details.", "", nm.time)
dat.bch %>% 
  .[ , (nm.time.new) := lapply(.SD, as.POSIXct, origin = "1970-01-01"), .SDcols = nm.time] %>% 
  .[ , value := lapply(details.vout, function(x) sum(x$value)), by = txid] %>% 
  .[ , currency := "TRX"]


out.f <- Filter(function(x) {!"error" %in% names(x)}, out.f)
out.f <- lapply(out.f, as.data.table)
out.f <- rbindlist(out.f, fill = TRUE)
dat.trx <- out.f
save(out.f, file = "data/dat_trx.RData")

# https://apilist.tronscan.org/api/transaction-info?hash=750ae57e247ac342174f66677f8cf9f76044f745acc354d236efeeb2c8676924
