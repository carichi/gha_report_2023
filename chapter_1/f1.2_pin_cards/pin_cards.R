required.packages <- c("data.table","jsonlite","httr","readxl", "rstudioapi")
lapply(required.packages, require, character.only=T)

invisible(lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/general/emdatData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/hiikData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/informAPI.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/acapsAPI.R",
         "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R",
         "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R"
), source))

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

isos <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")
###This map shows:###
#- The total number of PiN by country for 2022 and 2023
#- The types of crises affecting each country (displacement, conflict, natural/technological hazard)
#- Climate risk
#- Food insecurity score
#- Protracted crisis countries
#- HRP requirements
#- RRP requirements

##Total PiN##
hpc_pin <- fread("datasets/People in need - HPC/pin_total.csv")
hpc_pin <- hpc_pin[metric_id == "inNeed", .(iso3 = iso, year, hpc_pin = value/1000000)]
acaps_pin <- fread("datasets/ACAPS/acaps_pin.csv")[, .(iso3, year, acaps_pin)]
all_pin <- merge(hpc_pin, acaps_pin, all = T)[!grepl(";", iso3), .(pin = max(c(acaps_pin, hpc_pin), na.rm = T)), by = .(iso3, year)]

all_pin <- dcast(all_pin, iso3 ~ paste0("pin_", year), value.var = "pin")

##Crises types##
crisis_types <- fread("datasets/Crisis types/crisis_types.csv")
crisis_types <- crisis_types[crisis_types[, .I[which.min(abs(year - 2022))], by = iso3]$V1]

crisis_types <- crisis_types[, .(iso3, Physical, Conflict, Displacement, Complex)]

## Climate vuln ##
ndgain <- fread("datasets/ND-GAIN/vulnerability/vulnerability.csv", header = T)
ndgain <- ndgain[, .(iso3 = ISO3, vuln = `2019`)]
vuln_thresholds <- data.table(threshold = quantile(ndgain$vuln, seq(0,0.8,0.2), na.rm = T), climate_vulnerability = c("very resilient", "resilient", "slightly vulnerable", "vulnerable", "very vulnerable"))
ndgain[, climate_vulnerability := ifelse(vuln < vuln_thresholds[2]$threshold, vuln_thresholds[1]$climate_vulnerability, ifelse(vuln < vuln_thresholds[3]$threshold, vuln_thresholds[2]$climate_vulnerability, ifelse(vuln < vuln_thresholds[4]$threshold, vuln_thresholds[3]$climate_vulnerability, ifelse(vuln < vuln_thresholds[5]$threshold, vuln_thresholds[4]$climate_vulnerability, vuln_thresholds[5]$climate_vulnerability))))]

## Food insecurity ##
ipc <- fread("datasets/IPC/all_food_insecurity.csv")
phase_cols <- as.character(c(1:5))
ipc <- ipc[, .(country_phase = max(c(1:5)[.SD[, ..phase_cols]/N > 0.2]), fi = `3+`/1000000, fig = fi_1), by = .(iso3, year)]

ipc <- ipc[ipc[, .I[which.min(abs(year - 2022))], by = iso3]$V1]

ipc <- ipc[, .(iso3, ipc_phase = country_phase)]

##Protracted crisis countries##
pccs <- fread("datasets/Protracted Crisis/protracted_crisis_classifications.csv")
pccs <- pccs[year == 2022, .(iso3, consecutive_crisis, crisis_class)]

##HRPs##
hrps <- fts_get_appeals(2022)
hrps[, `:=` (iso3 = unlist(lapply(locations, function(x) paste0(x$iso3[x$adminLevel == 0], collapse = ";"))), type = unlist(lapply(categories, function(x) x$name)))]
hrps <- hrps[type != "Regional response plan"]

hrps_list <- list()
for(i in 1:nrow(hrps)){
  id <- hrps[i]$id
  hrps_list[[i]] <- fts_get_appeal_totals(id, 2022)
}
hrps_dt <- rbindlist(hrps_list[unlist(lapply(hrps_list, function(x) is.data.frame(x)))], fill = T)

hrps <- merge(hrps[, .(id, iso3)], hrps_dt[, .(id = appeal_id, HRP_funding = `Total incoming funding:`, HRP_requirements = `Total current requirements:`)], by = "id", all = T)
hrps <- hrps[, .(HRP_funding = sum(HRP_funding, na.rm = T), HRP_requirements = sum(HRP_requirements, na.rm = T)), by = iso3]

##RRPs##
combined_rrps <- fread("datasets/UNHCR RRPs/combined_rrps.csv", encoding = "UTF-8")

unhcr_rrps <- fread("datasets/UNHCR RRPs/rrp_data.csv", encoding = "UTF-8")
unhcr_rrps[Country == "Iran, Islamic Republic of", Country := "Iran (Islamic Republic of)"]
unhcr_rrps[Country == "Burma", Country := "Myanmar"]
unhcr_rrps[Country == "Czech Republic", Country := "Czechia"]
unhcr_rrps[Country == "Moldova", Country := "Republic of Moldova"]
unhcr_rrps[Country == "Slovak Republic", Country := "Slovakia"]
unhcr_rrps[Country == "Curaçao", Country := "Curacao"]
unhcr_rrps[Country == "United States", Country := "United States of America"]

unhcr_rrps <- merge(unhcr_rrps[Year == 2022], isos[, .(iso3, Country = countryname_unhcr)], by = "Country", all.x = T)

unhcr_rrps <- unhcr_rrps[!(RRP %in% combined_rrps[`Use?` == "UNOCHA"]$`UNHCR name`)]
unhcr_rrps <- unhcr_rrps[, .(country = Country, RRP, req = as.numeric(gsub("[$]|,", "", `Funds Requested`)), funds = as.numeric(gsub("[$]|,", "", `Funds Received (From all agencies)`)))]

unocha_rrps <- fts_get_appeals(2022)
unocha_rrps <- unocha_rrps[planVersion.name %in% combined_rrps[`Use?` == "UNOCHA"]$`UNOCHA name`]

unocha_rrps_list <- list()
for(i in 1:nrow(unocha_rrps)){
  id <- unocha_rrps[i]$id
  unocha_rrps_list[[i]] <- fts_get_appeal_locations(id, 2022)
}
unocha_rrps <- rbindlist(unocha_rrps_list, fill = T)
unocha_rrps[plan_name == "Rohingya Humanitarian Crisis Joint Response Plan 2022", Location := "Bangladesh"]

unocha_rrps <- unocha_rrps[, .(country = Location, RRP = plan_name, req = as.numeric(gsub(",", "", `Current requirements US$`)), funds = as.numeric(gsub(",", "", `Funding US$`)))]

rrps <- rbind(unhcr_rrps, unocha_rrps)
rrps <- rrps[, .(RRP_requirements = sum(req, na.rm = T), RRP_funding = sum(funds, na.rm = T)), by = country]

rrps[country == "Bolivia, Plurinational State of", country := "Bolivia"]
rrps[country == "Curaçao", country := "Curacao"]
rrps[country == "Türkiye", country := "Turkey"]

rrps <- merge(rrps, isos[, .(iso3, country = countryname_unhcr)], by = "country", all.x = T)
rrps <- rrps[, .(iso3, RRP_requirements, RRP_funding)]

###Join all
pin_cards <- merge(all_pin, crisis_types, by = "iso3", all.x = T)
pin_cards <- merge(pin_cards, ndgain, by = "iso3", all.x = T)
pin_cards <- merge(pin_cards, ipc, by = "iso3", all.x = T)
pin_cards <- merge(pin_cards, pccs, by = "iso3", all.x = T)
pin_cards <- merge(pin_cards, hrps, by = "iso3", all.x = T)
pin_cards <- merge(pin_cards, rrps, by = "iso3", all.x = T)

setwd(dirname(getActiveDocumentContext()$path))
fwrite(pin_cards, "f1_pin_cards.csv")
