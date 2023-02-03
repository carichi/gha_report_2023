required.packages <- c("data.table","jsonlite","httr", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))

isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8", na.strings = "")

## IPC ##
ipc <- data.table(fromJSON("https://map.ipcinfo.org/api/public/population-tracking-tool/data/2017,2023/?page=1&limit=10000&condition=A"))
ipc[, `:=` (current_date = (as.Date(paste0("01 ", current_thru_date), "%d %b %Y")-as.Date(paste0("01 ", current_from_date), "%d %b %Y"))/2 + as.Date(paste0("01 ", current_from_date), "%d %b %Y"),
            projected_date = (as.Date(paste0("01 ", projected_thru_date), "%d %b %Y")-as.Date(paste0("01 ", projected_from_date), "%d %b %Y"))/2 + as.Date(paste0("01 ", projected_from_date), "%d %b %Y"),
            additional_date = (as.Date(paste0("01 ", second_projected_thru_date), "%d %b %Y")-as.Date(paste0("01 ", second_projected_from_date), "%d %b %Y"))/2 + as.Date(paste0("01 ", second_projected_from_date), "%d %b %Y"))]
#ipc <- ipc[ipc[, .I[which.min(abs(as.Date("2020-06-30")-date))], by = country]$V1]

phase_cols <- grep("^phase\\d_._population$", names(ipc), value = T)
phase_cols <- phase_cols[order(phase_cols)]
#pop_cols <- grep("^estimated_population_", names(ipc), value = T)

ipc_national <- ipc[, .(variable = gsub("_population|phase", "", phase_cols), population = sapply(.SD, function(x) sum(x, na.rm = T))), .SDcols = c(phase_cols), by = .(title, code, anl_id, current_date, projected_date, additional_date)]
ipc_national[, c("phase", "type") := tstrsplit(variable, "_")]

ipc_national <- rbind(ipc_national[type == "C" & !is.na(current_date), .(title, code, date = current_date, type, phase, population)],
      ipc_national[type == "P" & !is.na(projected_date), .(title, code, date = projected_date, type, phase, population)],
      ipc_national[type == "A" & !is.na(additional_date), .(title, code, date = additional_date, type, phase, population)])[order(code, date)][phase != "6"]

ipc_national[, year := year(date)]

## CH ##
ch <- data.table(fromJSON("https://fsr2av3qi2.execute-api.us-east-1.amazonaws.com/ch/country?key=bac2a4d1-1274-4526-9065-0502ce9d4d5e"))
ch <- ch[!(country %in% ipc_national$code)]

ch[, date := (as.Date(paste0("01 ", to), "%d %b %Y")-as.Date(paste0("01 ", from), "%d %b %Y"))/2 + as.Date(paste0("01 ", from), "%d %b %Y")]

ch_national <- ch[, rbindlist(lapply(phases, function(x) lapply(x, function(y) as.numeric(y)))), by = .(title, country, condition, id, estimated_population, date, period, year)]

ipc_ch_national <- rbind(ipc_national, ch_national[, .(title, code = country, date, type = period, phase, population, year = year(date))])

ipc_ch_national <- merge(isos[, .(iso3, iso2)], ipc_ch_national, by.x = "iso2", by.y = "code", all.y = T)
ipc_ch_national[iso2 == "LAC", iso3 := "LAC"]

fwrite(ipc_ch_national, "ipc-ch_national.csv")
