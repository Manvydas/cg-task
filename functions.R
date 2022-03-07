# BCH ---------
# Split data into n-sized lists, returns nested list
split2 <- function(dd, n) {
  splited <- split(dd, ceiling(seq_along(dd)/n))
  out <- lapply(splited, function(x) list(txids = as.list(x)))
  return(out)
}

# Get transaction details for an array of TXIDs
# Returns an array of objects with transaction details of an array of TXIDs.
# https://api.fullstack.cash/docs/#api-bcash-UTXOs_for_a_single_address
getBCH <- function(df, dd) {
  dat.json <- httr::POST(url = "https://api.fullstack.cash/v5/electrumx/tx/data",
                         body = jsonlite::toJSON(df, pretty = T, auto_unbox = T),
                         httr::add_headers(`accept` = 'application/json'), 
                         httr::content_type('application/json'))
  out <- jsonlite::fromJSON(rawToChar(dat.json$content), flatten = T)$transactions
  return(out)
}

# XRP ---------
# Get transaction details for a TXID
getXRP <- function (id) {
  link <- "https://s1.ripple.com:51234/"
  df <- paste0('{"method":"tx", "params":[{"transaction":"',
               id,
               # 'D03650D923DAD400EA638907D9AECC4E73CC1A193220B4BC9F3F26BFE4121EF2',
               '", "binary":false}]}')
  
  httr::POST(url = link,
             body =  df,
             httr::add_headers(`accept` = 'application/json'), 
             httr::content_type('application/json'))
}


# Exchange data ---------
getExc <- # Get exchange data
  function(dd, bc) {
    link <- paste0("https://freecurrencyapi.net/api/v2/historical?apikey=YOUR-APIKEY&base_currency=",
                   bc,
                   "&date_from=",
                   min(dat[[bc]]$date, na.rm = T),
                   # 2020-10-01,
                   "&date_to=",
                   max(dat[[bc]]$date, na.rm = T)
    )
    exc.json <- httr::GET(link)
    exc <- jsonlite::fromJSON(rawToChar(exc.json$content), flatten = T)$data
    
    exc <- lapply(exc, as.data.table)
    nms <- names(exc)
    for (i in 1:length(exc)) {
      exc[[i]]$date <- as.Date(nms[i])
    }
    exc <- rbindlist(exc, fill = TRUE)
    if (bc == "BTC") {
      # plot(exc$date, exc$EUR, type = "l") # something wrong with BTC values
      nms <- setdiff(names(exc), bc)
      exc[ , paste0(curr.to, "_c") := lapply(.SD, function(x) x), .SDcols = curr.to]
      exc[date > "2020-09-01" & EUR < 5000, paste0(curr.to, "_c") := lapply(.SD, function(x) x*1000), .SDcols = curr.to]
      # plot(exc$date, exc$EUR_c, type = "l") # looks good now
    }
    return(exc)
  }