lapply(c("data.table", "rstudioapi", "XML", "httr"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))

#Establish donors types
donors <- rbindlist(lapply(xmlToList(htmlParse(GET("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TABLE2A")))$body$structure$codelists[[2]], function(x) data.frame(cbind(as.data.table(x)[1, ], as.data.table(x)[2, ]))), fill = T)
donors <- setnames(rbind(
  data.table("DAC donor", unlist(donors[.attrs.1 %in% 20001]$.attrs)),
  data.table("Non-DAC donor", c(unlist(donors[.attrs.1 %in% 20006]$.attrs), 87)),
  data.table("Multilateral donor", c(unlist(donors[.attrs.1 %in% c(20002, 20007:20034)]$.attrs), 1015)),
  data.table("Private donor", unlist(donors[.attrs.1 %in% c(20035, 21600)]$.attrs))), c("DonorType", "DonorCode"))

years <- 2016:2021

crs_list <- list()
for(i in 1:length(years)){
  
  year <- years[[i]]
  crs_list[[i]] <- fread(paste0("https://github.com/devinit/gha_automation/raw/main/IHA/datasets/crs_", year, ".gz"), showProgress = F)
  message(years[[i]])
}
crs_raw <- rbindlist(crs_list)
rm(crs_list)

oecd_isos <- fread("ISOs/oecd_isos.csv", encoding = "UTF-8")
population <- fread("Population/population.csv", encoding = "UTF-8")

population <- population[Time %in% years & Variant == "Medium" & LocID < 900]

major_keywords <- c(
  "anti-seismic adaption",
  "cbdrm",
  "climate protection",
  "climate resilience",
  "climate vulnerability",
  "coastal protection",
  "cyclone preparedness",
  "disaster management",
  "disaster reduction",
  "disaster resilience",
  "disaster risk mitigation",
  "disaster risk reduction",
  "disaster vulnerability reduction",
  "disaster risk management",
  "drm",
  "drr",
  "early warning",
  "earthquake-resistant",
  "earthquake resistant",
  "earthquake resistance",
  "embankment",
  "flood control",
  "flood mitigation plan",
  "flood prevention",
  "flood protection",
  "flood risk management",
  "fpi",
  "gfdrr",
  "hyogo framework of action",
  "lutte contre les inondations",
  "r.duction des risques de catastrophes",
  "resilience to disasters",
  "resilience to natural",
  "resilience to shock",
  "shock resilience",
  "storm warning",
  "vulnerability and capacity assessment",
  "disaster risk assessment",
  "multi-hazard risk mapping",
  "resilient infrastructure",
  "disaster insurance",
  "disaster risk insurance",
  "disaster risk analysis",
  "flood risk",
  "resilience to earthquakes",
  "seismically safe standards",
  "disaster preparedness plan",
  "disaster preparedness policy",
  "disaster preparedness",
  "disaster resistant construction",
  "disaster resilient building",
  "vulnerability to natural hazards",
  "disaster-resilient",
  "forest fire prevention",
  "hazard monitoring",
  "katastrophenvorsorge",
  "vorhersagebasiert",
  "fr.hwarnsystem",
  "klimaanpassung",
  "katastrophenrisik",
  "katastrophenvorbeugung",
  "evakuierungsplan",
  "r.duction des risques de catastrophe",
  "changement climatique",
  "r.silience climatique",
  "pr.paration aux catastrophes",
  "pr.vention des catastrophes",
  "r.sistante aux catastrophes",
  "cadre de sendai",
  "r.silience aux risques naturels",
  "vuln.rabilit. aux risques naturels",
  "construction r.sistantes aux catastrophes",
  "alerte pr.coce",
  "preparaci.n y prevenci.n desastre",
  "prevenci.n y preparaci.n en caso de desastre",
  "cambio clim.tico",
  "resiliencia a amenazas naturales",
  "reducci.n del riesgo de desastres",
  "resiliencia a los desastres",
  "vulnerabilidad frente a los desastres",
  "marco de sendai",
  "variabilidad clim.tica",
  "risk financing",
  "sendai framework"
)

disqualifying_keywords <- c(
  "serendipity",
  "domestic revenue mobilisation"
)

disqualifying_sectors <- c(
  "Domestic revenue mobilisation"
)

crs <- crs_raw[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  | 
    FlowName == "Private Development Finance"
]

crs <- merge(crs, oecd_isos[, .(RecipientName = countryname_oecd, RecipientISO = iso3)], by = "RecipientName", all.x = T)
crs <- merge(crs, oecd_isos[, .(DonorName = countryname_oecd, DonorISO = iso3)], by = "DonorName", all.x = T)
crs <- merge(crs, donors[, .(DonorType, DonorCode = as.integer(DonorCode))], by = "DonorCode", all.x = T)

#COVID identifier
covid_keywords <- c(
  "covid",
  "coronavirus",
  "covax",
  "corona",
  "\\bc19\\b"
)

crs[grepl(paste(covid_keywords, collapse = "|"), tolower(paste(LongDescription, ShortDescription, ProjectTitle, Keywords))) | PurposeName == "COVID-19 control", Covid_relevance := "COVID"]

#Assign relevance based on keywords
crs[, relevance := "None"]
crs[grepl(paste(major_keywords, collapse = "|"), tolower(paste(ShortDescription, ProjectTitle, Keywords))), relevance := "Keywords_Major"]

#Remove relevance based on disqualifying factors
crs[relevance != "None" & grepl(paste(disqualifying_keywords, collapse = "|"), tolower(paste(ProjectTitle, ShortDescription, LongDescription))), relevance := "Keywords_None"]
crs[relevance != "None" & PurposeName %in% disqualifying_sectors, relevance := "None"]

#Create Primary DRR identifier based on keyword relevance, DRR marker, and Purpose Name
crs[, Primary_DRR := ifelse(relevance == "Keywords_Major" | DRR == 2 | PurposeName == "Disaster Risk Reduction" | (PurposeName == "Multi-hazard response preparedness" & Year < 2018), "DRR", NA_character_)]

#Create CCA identifier based on CCA flag
crs[ClimateAdaptation == 2, Primary_CCA := "CCA"]

#Create humanitarian identifier based on sector
crs[, Humanitarian := ifelse(substr(SectorCode, 1, 1) == "7", "Humanitarian", NA_character_)]

#Create localisation identifier based on parent channel
crs[grepl("^23", ParentChannelCode), Localised := "Localised"]

comp <- crs[, .(USD_Disbursement_Defl = sum(USD_Disbursement_Defl, na.rm = T)),
                by = .(Year, DonorType, DonorName, DonorISO, RecipientName, RecipientISO, FlowName, Primary_DRR, Primary_CCA, Humanitarian, Covid_relevance)]

source("risk_scores.R")
risk <- fread("risk_scores.csv")
names(risk)[names(risk) == "iso3"] <- "RecipientISO"

comp <- merge(comp, risk[, .(RecipientISO, Year, inform_ph, inform_ph_class)], by = c("RecipientISO", "Year"), all.x = T)

comp <- merge(comp, pccs[, .(RecipientISO = iso3, Year = year, crisis_class)], by = c("RecipientISO", "Year"), all.x = T)

fwrite(comp, "f8_drr_cca_analysis.csv")
