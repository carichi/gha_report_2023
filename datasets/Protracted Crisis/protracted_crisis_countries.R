lapply(c("data.table", "rstudioapi"), require, character.only = T)

setwd(dirname(getActiveDocumentContext()$path))

invisible(source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R"))
isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv")

historical <- fread("historical_pcc.csv")
historical <- historical[, .(iso2 = `Country ID`, year = Year)]

historical <- merge(isos[, .(iso3, iso2)], historical[!is.na(iso2)], by = "iso2")[, iso2 := NULL][]

appeals <- fts_get_appeals(2000:year(Sys.Date()))

appeals_dt <- appeals[, .(iso3 = unlist(lapply(locations, function(x) as.character(x$iso3[x$adminLevel == 0]))), year = unlist(lapply(years, function(x) x$year)), type = unlist(lapply(categories, function(x) x$name))), by = .(id, planVersion.name)]

appeals_dt <- unique(rbind(appeals_dt, historical, fill = T)[, .(iso3, year)])

appeals_long <- melt(dcast(appeals_dt[!is.na(iso3)], iso3 ~ year, value.var = "iso3", fun.aggregate = function(x) ifelse(length(x) > 0, 1, 0)), id.vars = "iso3")[order(iso3, variable)]

appeals_long[value == 1, crisis_run := cumsum(c(T, diff(as.numeric(as.character(variable))) != 1)), by = iso3][, consecutive_crisis := cumsum(value), by = .(iso3, crisis_run)][, `:=` (crisis_run = NULL, value = NULL)]

appeals_long[, crisis_class := ifelse(consecutive_crisis >= 5, "PC", ifelse(consecutive_crisis >= 2, "RC", ifelse(consecutive_crisis == 1, "C", NA_character_)))]

appeals_long <- merge(isos[, .(iso3, countryname)], appeals_long, all.y = T)
setnames(appeals_long, "variable", "year")

fwrite(appeals_long, "protracted_crisis_classifications.csv")
