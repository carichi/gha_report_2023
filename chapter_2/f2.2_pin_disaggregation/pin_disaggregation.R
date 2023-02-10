suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
invisible(lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/PiN/hpc_caseload_api.R")
       , source))

setwd(dirname(getActiveDocumentContext()$path))

plans <- data.table(fromJSON("https://api.hpc.tools/v2/public/plan")$data)
plans[, year := lapply(years, function(x) x$year), by = id]
plans[, type := lapply(categories, function(x) x$name), by = id]

plans <- plans[year %in% 2021:2023]

plan_caseloads <- list()
pb <- txtProgressBar(0, nrow(plans), style = 3)
for(i in 1:nrow(plans)){
  plan_id <- plans$id[[i]]
  plan_name <- plans$planVersion.name[[i]]
  location <- paste0(plans$locations[[i]][["name"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  iso <- paste0(plans$locations[[i]][["iso3"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  year <- paste0(plans$years[[i]][["year"]], collapse = "; ")
  
  metadata <- cbind(plan_id, plan_name, location, iso, year)
  
  caseload_temp <- hpc_api_all(plan_id, data_type = "caseLoad", by_sector = T, disaggregations = T)
  
  if(!is.null(caseload_temp)) plan_caseloads[[i]] <- data.table(cbind(metadata, caseload_temp))
  rm(plan_id)
  setTxtProgressBar(pb, i)
}
plan_caseloads <- rbindlist(plan_caseloads, fill = T)

plan_caseloads <- unique(plan_caseloads[metric_id %in% c("inNeed", "target", "expectedReach")])

#

splits <- plan_caseloads[!is.na(value), .(cat_splits =  paste0(unique(category_name), collapse = ", "), sec_splits = paste0(unique(sector), collapse = ", "), loc_splits = paste0(unique(location_name), collapse = ", "), total_pin = sum(value[category_name == "Total" & metric_id == "inNeed" & sector == "Total" & (location_name != location | is.na(location_name))], na.rm = T)), by = .(plan_id, plan_name, year)]

#Gender
gender_f <- c("women", "girl", "female", "fille", "femme", "féminin", "feminin", "niña", "nina", "mujere")
gender_m <- c("\\bmen\\b", "boy", "\\bmale\\b", "garçon", "garcon", "homme", "masculin", "niño", "\\bnino\\b", "hombre")
gender_terms <- paste0(paste0(gender_f, collapse = "|"), "|", paste0(gender_m, collapse = "|"))

splits[grepl(gender_terms, cat_splits, ignore.case = T), gender := T]

#IDPs
idps <- c("\\bidp", "\\bpdi", "displaced")
refugees <- c("refugee", "réfugié.s", "refugie.s")
refugee_terms <- paste0(paste0(idps, collapse = "|"), "|", paste0(refugees, collapse = "|"))

splits[grepl(refugee_terms, cat_splits, ignore.case = T), refugees := T]

#Children
children <- c("child", "girl", "boy", "fille", "garçon", "garcon", "niña", "niño", "\\b5\\b", "19", "17", "\\b1\\b")
children_terms <- paste0(children, collapse = "|")

splits[grepl(children_terms, cat_splits, ignore.case = T), children := T]

#Elderly
elderly <- c("elderly", "60")
elderly_terms <- paste0(elderly, collapse = "|")

splits[grepl(elderly_terms, cat_splits, ignore.case = T), elderly := T]

#Subnational
splits[loc_splits != "NA" & length(strsplit(loc_splits, ",")) > 2, subnational := T]

#Sectors
splits[sec_splits != "Total", sectors := T]

##
gender_caseloads <- plan_caseloads[plan_id %in% splits[gender == T]$plan_id]

#Fix data error for LBN
gender_caseloads[plan_id == 1099 & sector_id == "HEA" & category_id == "Men" & metric_id == "inNeed", value := 626923]

gender_caseloads[grepl(paste0(gender_f, collapse = "|"), category_name, ignore.case = T), female := T]
gender_caseloads[grepl(paste0(gender_m, collapse = "|"), category_name, ignore.case = T), male := T]

gender_sector_caseloads <- gender_caseloads[metric_id == "inNeed", .(female_pin = sum(value[female == T], na.rm = T), male_pin = sum(value[male == T], na.rm = T), total_pin = sum(value[category_name == "Total" & sector == "Total" & (location_name != location | is.na(location_name))], na.rm = T)), by = .(plan_id, plan_name, iso, year, sector_id, sector, description)]

gender_total_caseloads <- gender_sector_caseloads[, .(female_pin = max(female_pin, na.rm = T), male_pin = max(male_pin, na.rm = T), total_pin = max(total_pin)), by = .(plan_id, plan_name, iso, year)][, .(female_pin_share = female_pin/(female_pin + male_pin), male_pin_share = male_pin/(female_pin + male_pin), total_pin), by = .(plan_id, plan_name, iso, year)]

gender_total_average <- gender_total_caseloads[!is.na(female_pin_share), .(female_pin_share = sum(total_pin*female_pin_share)/sum(total_pin), total_pin = sum(total_pin)), by = year]

##
children_caseloads <- plan_caseloads[plan_id %in% splits[children == T]$plan_id]

children_caseloads[grepl(paste0(children_terms, collapse = "|"), category_name, ignore.case = T), children := T]

children_sector_caseloads <- children_caseloads[metric_id == "inNeed", .(child_pin = sum(value[children == T & (location_name != location | is.na(location_name))], na.rm = T), total_pin = sum(value[category_name == "Total" & (location_name != location | is.na(location_name))], na.rm = T)), by = .(plan_id, plan_name, iso, year, sector_id, sector, description)]

children_total_caseloads <- children_sector_caseloads[, .(child_pin = max(child_pin, na.rm = T), total_pin = max(total_pin[child_pin != 0])), by = .(plan_id, plan_name, iso, year)][, .(child_pin_share = child_pin/total_pin, child_pin, total_pin), by = .(plan_id, plan_name, iso, year)]

children_total_average <- children_total_caseloads[!is.na(child_pin_share) & child_pin_share != 0, .(child_pin_share = sum(total_pin*child_pin_share)/sum(total_pin), total_pin = sum(total_pin)), by = year]
