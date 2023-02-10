required.packages <- c("data.table","jsonlite","httr","readxl", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

source("https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R")

isos <- fread("datasets/Countrynames/isos.csv", encoding = "UTF-8")

##Total PiN##
hpc_pin <- fread("datasets/People in need - HPC/pin_total.csv")
hpc_pin <- hpc_pin[metric_id == "inNeed", .(iso3 = iso, year, hpc_pin = value/1000000)]
acaps_pin <- fread("datasets/ACAPS/acaps_pin.csv")[, .(iso3, year, acaps_pin)]
all_pin <- merge(hpc_pin, acaps_pin, all = T)[!grepl(";", iso3), .(pin = max(c(acaps_pin, hpc_pin), na.rm = T)), by = .(iso3, year)]

all_pin <- dcast(all_pin, iso3 ~ paste0("pin_", year), value.var = "pin")

##Protracted crisis countries##
pccs <- fread("datasets/Protracted Crisis/protracted_crisis_classifications.csv")
pccs <- pccs[year %in% c(2022, 2023), .(iso3, year, consecutive_crisis, crisis_class)]

map_pccs <- pccs[, .(appeal_2022 = (consecutive_crisis[year == 2022] > 0), appeal_2023 = (consecutive_crisis[year == 2023] > 0), protracted_crisis_2022 = (consecutive_crisis[year == 2022] >= 5)), by = iso3]

map_pccs <- merge(map_pccs, all_pin, by = "iso3",  all = T)

map_pccs[is.na(map_pccs)] <- F

#Population
wupPop <- wup_get()
wupPop <- wupPop[year %in% c(2022, 2023) & area == "total"]

wupPop <- dcast(wupPop, ISO3 ~ paste0("population_", year), value.var = "population")

map_pccs <- merge(map_pccs, wupPop, by.x = "iso3", by.y = "ISO3", all.x = T)

map_pccs[, `:=` (pin_share_2022 = 1000000*pin_2022/population_2022, pin_share_2023 = 1000000*pin_2023/population_2023)]

#Select countries
map_pccs_select <- map_pccs[(pin_2022 >= 1 & (appeal_2022 | pin_share_2022 >= 0.1)) | (pin_2023 >= 1 & (appeal_2023 | pin_share_2023 >= 0.1))]

map_pccs_key <- map_pccs_select[, .(crisis_2022 = appeal_2022 | pin_share_2022 >= 0.1 | pin_2022 >= 1, crisis_2023 = appeal_2023 | pin_share_2023 >= 0.1 | pin_2023 >= 1, protracted_crisis_2022), by = iso3]

fwrite(map_pccs_key, "chapter_1/f1.1_pin_map/f1_map_key.csv")
