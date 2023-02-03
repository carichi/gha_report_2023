suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
setwd(dirname(getActiveDocumentContext()$path))

#lapply(c("ipc-ch_data.R", "hno_data.R"), source)

ipc <- fread("ipc-ch_national.csv", encoding = "UTF-8")
hno <- fread("hno_data.csv", encoding = "UTF-8")

hno_ex <- hno[!is.na(value) & !(paste0(iso3, year) %in% ipc[, .(paste0(iso3, year))]$V1)]
hno_ex <- hno_ex[, .(`3+` = max(value[food_insecure == T & metric_id %in% c("inNeed", "target")], na.rm = T), N = max(value[metric_id %in% c("affected", "inNeed", "target")], na.rm = T), source = "HNO"), by = .(iso3, year)]

hno_ex <- hno_ex[!is.infinite(`3+`)]

ipc[, source := "IPC/CH"]

#survey_priority <- data.table(type = c("C", "A", "P"), priority = c(1, 2, 3))
#ipc <- merge(ipc, survey_priority)

#ipc[, `3+` := sum(population[phase >=3]), by = .(iso3, date)]

#ipc[, population := max(population), by = .(iso3, date, phase)]

#ipc_d <- ipc[ipc[, .I[which.min(priority)], by = .(iso3, year, phase)]$V1]

ipc_d <- dcast(ipc, title + iso3 + date + year + type + source ~ phase, value.var = "population")[, `:=` (N = `1` + `2` + `3` + `4` + `5`, `3+` = `3` + `4` + `5`)]
# 
# fi <- rbind(ipc_d[, .(fi = `3` + `4` + `5`), by = .(iso3, year, source)], hno_ex)[order(iso3, year)]


all_fi <- rbind(ipc_d[, .(iso3, year, source, `1`, `2`, `3`, `4`, `5`, N, `3+`)], hno_ex, fill = T)[order(iso3, year)]

all_fi[, `:=` (fi_0 = `3+`/N, fi_1 = ((`3` + `4`*2 + `5`*3)/3)/N, fi_2 = ((`3` + `4`*4 + `5`*9)/9)/N)]

all_fi_gaps <- all_fi[, .SD[which.max(fi_1)], by = .(iso3, year, source)]
all_fi_num <- all_fi[, .SD[which.max(`3+`)], by = .(iso3, year, source)]

fwrite(all_fi_gaps, "all_food_insecurity_gaps.csv")
fwrite(all_fi_num, "all_food_insecurity.csv")
