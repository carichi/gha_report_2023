required.packages <- c("data.table","jsonlite","httr","readxl", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))
invisible(source("https://raw.githubusercontent.com/devinit/gha_automation/main/general/acapsAPI.R"))

dates <- as.POSIXlt("2022-01-01")
dates$mon <- seq(0,length(seq(as.Date("2022-01-01"), Sys.Date(), by = 'month'))-1)
dates <- format.Date(dates, "%b%Y")

pin.out <- data.table()
for(i in 1:length(dates)){
  message(dates[i])
  pin_temp <- acaps_get(database = "inform-severity-index", dataset = "conditions-of-people-affected", date = dates[i], token = token)
  pin_temp$mon <- substr(dates[i], 1, 3)
  pin_temp$year <- substr(dates[i], 4, 8)
  pin.out <- rbind(pin.out, pin_temp, fill = T)
  rm(pin_temp)
}

pin.out[, `:=` (Total.PiN = sum(c(`# of people in moderate conditions - level 3`, `# of people severe conditions - level 4`, `# of people extreme conditions - level 5`), na.rm = T)
            ,check = sum(c(`% of people in none/minimal conditions - Level 1`, `% of people in stressed conditions - level 2`, `% of people in moderate conditions - level 3`, `% of people severe conditions - level 4`, `% of people extreme conditions - level 5`), na.rm = T))
    , by = .(crisis_id, year, mon)]

pin.out[, `:=` (country = sapply(country, paste0, collapse = "; "), iso3 = sapply(iso3, paste0, collapse = "; "))]

pin.max <- pin.out[pin.out[floor(check) <= 110, .I[which.max(Total.PiN)], by = .(year, crisis_id)]$V1][order(year, crisis_id)]

pin.max[, seq := seq(1, .N), by = .(iso3, year)] #Establish hierarchy of crises based on listed order
pin.max[, full.parent := sum(Total.PiN[seq > 1]) >= Total.PiN[seq == 1], by = .(iso3, year)] #Examine whether the parent crisis (seq == 1) is likely to contain all children, or is a separate crisis itself

pin.max[, Adjusted.PiN := ifelse(.N > 1 & full.parent == FALSE & seq == 1, Total.PiN[seq == 1] - sum(Total.PiN[seq > 1]), Total.PiN), by = .(iso3, year)]

acaps_pin <- pin.max[seq == 1 | (seq != 1 & full.parent == FALSE), .(acaps_pin = sum(Adjusted.PiN, na.rm = T)), by = .(iso3, year)]

fwrite(acaps_pin, "acaps_pin.csv")
