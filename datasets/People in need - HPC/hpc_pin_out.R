suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
invisible(lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/PiN/hpc_caseload_api.R")
                 , source))

setwd(dirname(getActiveDocumentContext()$path))

plans <- data.table(fromJSON("https://api.hpc.tools/v2/public/plan")$data)
plans[, year := lapply(years, function(x) x$year), by = id]
plans[, type := lapply(categories, function(x) x$name), by = id]

plans <- plans[year %in% 2022:2023]

#Total
plan_caseloads <- list()
pb <- txtProgressBar(0, nrow(plans), style = 3)
for(i in 1:nrow(plans)){
  plan_id <- plans$id[[i]]
  plan_name <- plans$planVersion.name[[i]]
  location <- paste0(plans$locations[[i]][["name"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  iso <- paste0(plans$locations[[i]][["iso3"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  year <- paste0(plans$years[[i]][["year"]], collapse = "; ")
  
  metadata <- cbind(plan_id, plan_name, location, iso, year)
  
  caseload_temp <- hpc_api_all(plan_id, data_type = "caseLoad", by_sector = F, disaggregations = F)
  
  if(!is.null(caseload_temp)) plan_caseloads[[i]] <- data.table(cbind(metadata, caseload_temp))
  rm(plan_id)
  setTxtProgressBar(pb, i)
}
plan_caseloads <- rbindlist(plan_caseloads, fill = T)

plan_caseloads <- unique(plan_caseloads[metric_id %in% c("inNeed", "target", "expectedReach")])

fwrite(plan_caseloads, "pin_total.csv")

#By sector
plan_caseloads <- list()
pb <- txtProgressBar(0, nrow(plans), style = 3)
for(i in 1:nrow(plans)){
  plan_id <- plans$id[[i]]
  plan_name <- plans$planVersion.name[[i]]
  location <- paste0(plans$locations[[i]][["name"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  iso <- paste0(plans$locations[[i]][["iso3"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  year <- paste0(plans$years[[i]][["year"]], collapse = "; ")
  
  metadata <- cbind(plan_id, plan_name, location, iso, year)
  
  caseload_temp <- hpc_api_all(plan_id, data_type = "caseLoad", by_sector = T, disaggregations = F)
  
  if(!is.null(caseload_temp)) plan_caseloads[[i]] <- data.table(cbind(metadata, caseload_temp))
  rm(plan_id)
  setTxtProgressBar(pb, i)
}
plan_caseloads <- rbindlist(plan_caseloads, fill = T)

plan_caseloads <- unique(plan_caseloads[metric_id %in% c("inNeed", "target", "expectedReach")])

fwrite(plan_caseloads, "pin_by_sector.csv")

#By sector
plan_caseloads <- list()
pb <- txtProgressBar(0, nrow(plans), style = 3)
for(i in 1:nrow(plans)){
  plan_id <- plans$id[[i]]
  plan_name <- plans$planVersion.name[[i]]
  location <- paste0(plans$locations[[i]][["name"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  iso <- paste0(plans$locations[[i]][["iso3"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  year <- paste0(plans$years[[i]][["year"]], collapse = "; ")
  
  metadata <- cbind(plan_id, plan_name, location, iso, year)
  
  caseload_temp <- hpc_api_all(plan_id, data_type = "caseLoad", by_sector = F, disaggregations = T)
  
  if(!is.null(caseload_temp)) plan_caseloads[[i]] <- data.table(cbind(metadata, caseload_temp))
  rm(plan_id)
  setTxtProgressBar(pb, i)
}
plan_caseloads <- rbindlist(plan_caseloads, fill = T)

plan_caseloads <- unique(plan_caseloads[metric_id %in% c("inNeed", "target", "expectedReach")])

fwrite(plan_caseloads, "pin_by_disaggregation.csv")