required.packages <- c("data.table","jsonlite","httr", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8", na.strings = "")

## IPC ##
ipc_sub <- data.table(fromJSON("https://map.ipcinfo.org/api/public/population-tracking-tool/data/2017,2022/?page=1&limit=10000&condition=A"))

## CH ##
ch <- data.table(fromJSON("https://fsr2av3qi2.execute-api.us-east-1.amazonaws.com/ch/country?key=bac2a4d1-1274-4526-9065-0502ce9d4d5e"))
ch <- ch[!(country %in% ipc_sub$code)]

ch[, date := (as.Date(paste0("01 ", to), "%d %b %Y")-as.Date(paste0("01 ", from), "%d %b %Y"))/2 + as.Date(paste0("01 ", from), "%d %b %Y")]

ch_sub <- list()
for(i in 1:nrow(ch)){
  id <- ch[i]$id
  cc <- ch[i]$country
  link <- paste0("https://map.ipcinfo.org/api/export/geojson/", id, "/?country=", cc, "&condition=A")
  temp <- fromJSON((link))$features$properties
  temp$icons <- NULL
  ch_sub[[i]] <- as.data.table(temp)
}

ch_sub <- rbindlist(ch_sub, fill = T)

ipc_ch_sub <- rbind(ipc_sub, ch_sub, fill = T)

ipc_ch_sub[, `:=` (current_date = (as.Date(paste0("01 ", current_thru_date), "%d %b %Y")-as.Date(paste0("01 ", current_from_date), "%d %b %Y"))/2 + as.Date(paste0("01 ", current_from_date), "%d %b %Y"),
            projected_date = (as.Date(paste0("01 ", projected_thru_date), "%d %b %Y")-as.Date(paste0("01 ", projected_from_date), "%d %b %Y"))/2 + as.Date(paste0("01 ", projected_from_date), "%d %b %Y"),
            additional_date = (as.Date(paste0("01 ", second_projected_thru_date), "%d %b %Y")-as.Date(paste0("01 ", second_projected_from_date), "%d %b %Y"))/2 + as.Date(paste0("01 ", second_projected_from_date), "%d %b %Y"))]

## Tidy ##
phase_cols <- grep("^phase\\d_._population$", names(ipc_ch_sub), value = T)
phase_cols <- phase_cols[order(phase_cols)]
#pop_cols <- grep("^estimated_population_", names(ipc), value = T)

ipc_ch_sub <- ipc_ch_sub[, .(variable = gsub("_population|phase", "", phase_cols), population = sapply(.SD, function(x) sum(x, na.rm = T))), .SDcols = c(phase_cols), by = .(title, code, aar_id, anl_id, area, current_date, projected_date, additional_date)]
ipc_ch_sub[, c("phase", "type") := tstrsplit(variable, "_")]

ipc_ch_sub <- rbind(ipc_ch_sub[type == "C" & !is.na(current_date), .(title, code, anl_id, area, date = current_date, type, phase, population)],
                      ipc_ch_sub[type == "P" & !is.na(projected_date), .(title, code, anl_id, area, date = projected_date, type, phase, population)],
                      ipc_ch_sub[type == "A" & !is.na(additional_date), .(title, code, anl_id, area, date = additional_date, type, phase, population)])[order(code, area, date)][phase != "6"]

ipc_ch_sub[, year := year(date)]

ipc_ch_sub <- merge(isos[, .(iso3, iso2)], ipc_ch_sub, by.x = "iso2", by.y = "code", all.y = T)
ipc_ch_sub[iso2 == "LAC", iso3 := "LAC"]

fwrite(ipc_ch_sub, "project_data/ipc-ch_subnational.csv")
