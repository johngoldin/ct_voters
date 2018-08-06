# Geocoding a csv column of "addresses" in R
# from:  http://www.storybench.org/geocode-csv-addresses-r/
#test:  wrap_geocode("248 Sam Hill Rd, Guilford, CT", 28)
#test:  geocode("248 Sam Hill Rd, Guilford, CT", output = "latlona", source = "dsk")
#test:  xx <- wrap_geocode("ksdfjsdfaf",23)

wrap_geocode <- function(an_address, an_id) {
  result <- data_frame(in_address = an_address, id = an_id, lat = NA_real_, lon = NA_real_, out_address = "", rc = "")
  try_result = tryCatch({
    geo_rc <- 0
    geo_result <- geocode(an_address, output = "latlona", source = "dsk", messaging = FALSE)
    if (ncol(geo_result) <= 2) {
      result$rc[1] <- paste(nrow(geo_result), "rows", "and", ncol(geo_result), "columns returned from geocode.")
      return(result)
    } else if (nrow(geo_result) == 1) {
      geo_rc <- 1
      result$lon[1] <- geo_result$lon[1]
      result$lat[1] <- geo_result$lat[1]
      result$out_address[1] <- as.character(geo_result$address[1])  
      result$id[1] <- an_id
      result$in_address[1] <- an_address
      result$rc[1] <- "OK"
      return(result)
    } else {
      result$rc[1] <- paste(nrow(geo_result), "rows returned from geocode.")
      return(result)
    }
  }, error = function(e) {
    result$rc[1] <- as.character(e)
    return(result)
  })
  return(try_result)
}

#test: result <- geocode_addresses(addresses$address[1:15])
# geocode_addresses <- function(addresses, ids) {
#   library(ggmap)
#   lat_output <- vector("double", length(addresses)) 
#   lon_output <- vector("double", length(addresses)) 
#   addr_output <- vector("character", length(addresses)) 
#   for (i in seq_along(addresses)) {  
#     # Sys.sleep(0.2)
#       result <- wrap_geocode(addresses[[i]])
# 
#     if (nrow(result) >= 1) {
#       lon_output[[i]] <- result$lon[1]
#       lat_output[[i]] <- result$lat[1]
#       addr_output[[i]] <- result$address[1]
#     } else {
#       print(paste(i, nrow(result), "Problem."))
#     }
#   }
#   result <- data_frame(
#     lon = lon_output,
#     lat = lat_output,
#     address = addr_output
#   )
#   return(result)
# }

