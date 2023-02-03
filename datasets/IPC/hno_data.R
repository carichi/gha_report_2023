suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
invisible(lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/PiN/hpc_caseload_api.R",
                   "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R")
                 , source))

setwd(dirname(getActiveDocumentContext()$path))

appeals <- fts_get_appeals(2017:2023)

appeals_dt <- appeals[, .(iso3 = unlist(lapply(locations, function(x) paste0(as.character(x$iso3[x$adminLevel == 0]), collapse="; "))), year = unlist(lapply(years, function(x) x$year)), type = unlist(lapply(categories, function(x) x$name))), by = .(id, planVersion.name)]

pin <- list()
pb <- txtProgressBar(0, nrow(appeals_dt), style = 3)
for(i in 1:nrow(appeals_dt)){
  
  id = appeals_dt[i]$id
  temp <- hpc_api_all(id, by_sector = T, disaggregations = F)
  pin[[i]] <- cbind(appeals_dt[i, .(id, plan_name = planVersion.name, iso3, year)], temp)
  setTxtProgressBar(pb, i)
}

pin <- rbindlist(pin, fill = T)

fi_terms <- c("food", "aliment")
disq_terms <- c("nfi", "non.food", "non.aliment")

fi_pin <- pin[!grepl(paste0(disq_terms, collapse = "|"), sector, ignore.case = T), food_insecure := (grepl(paste0(fi_terms, collapse = "|"), sector, ignore.case = T) | sector_id == "FSC")]

fwrite(fi_pin, "hno_data.csv")
