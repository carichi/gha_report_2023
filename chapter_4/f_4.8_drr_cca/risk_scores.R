require("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

required.packages <- c("data.table", "readxl")
lapply(required.packages, require, character.only = T)

gain <- fread("GAIN/gain.csv", encoding = "UTF-8", header = T)
inform <- data.table(read_excel("INFORM/inform.xlsx"))

ha_components <- c("HA.NAT.FL", "HA.NAT.TC", "HA.NAT.DR", "HA.NAT.EQ", "HA.NAT.TS", "HA.NAT.EPI")

inform_ph <- inform[, .(PH = round(10-exp(mean(log((10-IndicatorScore[IndicatorId %in% ha_components])/10*9+1), na.rm = T)), 1), 
                        CC = IndicatorScore[IndicatorId == "CC"]),
                    by = .(Iso3, INFORMYear)]

inform_ph[, inform_ph := round(PH^(1/2)*CC^(1/2),1)*10]

gain_ph <- melt(gain, id.vars = c("ISO3", "Name"), value.name = "gain", variable.factor = F)

risk <- merge(inform_ph[, .(iso3 = Iso3, Year = as.character(INFORMYear), inform_ph)], gain_ph[, .(iso3 = ISO3, Year = variable, gain)], by = c("iso3", "Year"), all = T)

risk[, `:=` (gain = nafill(nafill(gain, "locf"), "nocb"), inform_ph = nafill(nafill(inform_ph, "locf"), "nocb")), by = iso3]

risk[, inform_ph_class := NA_character_]
risk[inform_ph < 20, inform_ph_class := "Very low"][inform_ph >= 20, inform_ph_class := "Low"][inform_ph >= 35, inform_ph_class := "Medium"][inform_ph >= 50, inform_ph_class := "High"][inform_ph >= 65, inform_ph_class := "Very high"]
risk[, gain_class := NA_character_]
risk[gain < 40, gain_class := "Very high"][gain >= 40, gain_class := "High"][gain >= 45, gain_class := "Medium"][gain >= 50, gain_class := "Low"][gain >= 60, gain_class := "Very low"]

fwrite(risk, "risk_scores.csv")
