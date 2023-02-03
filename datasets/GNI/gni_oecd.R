suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

setwd(dirname(getActiveDocumentContext()$path))
invisible(lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/general/tabulate_dac_api.R"), source))

years <- 2000:2022

gni_oecd <- tabulate_dac_api("TABLE1", list("", "1", "1", "1140", "D"), years[1], years[length(years)])

keep <- c("Donor", years)
gni_oecd <- melt(gni_oecd[, ..keep], id.vars = "Donor")

gni_oecd[gni_oecd == 0] <- NA

names(gni_oecd) <- c("Donor", "Year", "GNI_USD")

fwrite(gni_oecd, "GNI_OECD_20230203.csv")
