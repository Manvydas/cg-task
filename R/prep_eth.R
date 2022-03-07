# ETH ---------
ids <- unique(data.table(dat$ETH)[!txid %in% out.f$tx_hash]$txid)
out <- list()
for (i in seq_along(ids)) {
  
  id <- ids[i]
  url <- paste0("https://api.covalenthq.com/v1/1/transaction_v2/",
                id,
                "/?key=ckey_3e0910f1a58240a09f22859252c") # my key
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
  
  out[[i]] <- jsonlite::fromJSON(rawToChar(dd.raw$content), flatten = T)$data$items
  
}

outt <- copy(out)
out.f <- c(outt, outt2, out)

nm.time <- grep("time", names(dat.bch), value = TRUE) # "time" is the one we need
nm.time.new <- gsub("details.", "", nm.time)
dat.bch %>% 
  .[ , (nm.time.new) := lapply(.SD, as.POSIXct, origin = "1970-01-01"), .SDcols = nm.time] %>% 
  .[ , value := lapply(details.vout, function(x) sum(x$value)), by = txid] %>% 
  .[ , currency := "ETH"]


out.f <- Filter(function(x) {!"error" %in% names(x)}, out.f)
out.f <- lapply(out.f, as.data.table)
out.f <- rbindlist(out.f, fill = TRUE)
dat.eth <- out.f
save(dat.eth, file = "data/dat_eth.RData")



# https://api.covalenthq.com/v1/1/transaction_v2/0x8747cd9340f67246e0719167be7581b1cd4d498c606d50e52a7f0f2d2c830c16/?key=ckey_3e0910f1a58240a09f22859252c
# 
# https://apilist.tronscan.org/api/transaction-info?hash=750ae57e247ac342174f66677f8cf9f76044f745acc354d236efeeb2c8676924



