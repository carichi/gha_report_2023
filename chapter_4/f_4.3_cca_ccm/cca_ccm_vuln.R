lapply(c("data.table", "rstudioapi", "XML", "httr"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))

#Establish donors types
donors <- rbindlist(lapply(xmlToList(htmlParse(GET("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TABLE2A")))$body$structure$codelists[[2]], function(x) data.frame(cbind(as.data.table(x)[1, ], as.data.table(x)[2, ]))), fill = T)
donors <- setnames(rbind(
  data.table("DAC donor", unlist(donors[.attrs.1 %in% 20001]$.attrs)),
  data.table("Non-DAC donor", c(unlist(donors[.attrs.1 %in% 20006]$.attrs), 87)),
  data.table("Multilateral donor", c(unlist(donors[.attrs.1 %in% c(20002, 20007:20034)]$.attrs), 1015)),
  data.table("Private donor", unlist(donors[.attrs.1 %in% c(20035, 21600)]$.attrs))), c("DonorType", "DonorCode"))

years <- 2017:2021

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
pccs <- fread("Protracted Crisis/protracted_crisis_classifications.csv")

population <- population[Time %in% years & Variant == "Medium" & LocID < 900]

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

#Create CCA identifier based on CCA flag
crs[ClimateAdaptation == 2, Primary_CCA := "CCA"]

#Create CCM identifier based on CCA flag
crs[ClimateMitigation == 2, Primary_CCM := "CCM"]

#Create humanitarian identifier based on sector
crs[, Humanitarian := ifelse(substr(SectorCode, 1, 1) == "7", "Humanitarian", NA_character_)]

#Create localisation identifier based on parent channel
crs[grepl("^23", ParentChannelCode), Localised := "Localised"]

comp <- crs[, .(USD_Disbursement_Defl = sum(USD_Disbursement_Defl, na.rm = T)),
                by = .(Year, DonorType, DonorName, DonorISO, RecipientName, RecipientISO, FlowName, Primary_CCA, Primary_CCM, Humanitarian, Covid_relevance)]

source("risk_scores.R")
risk <- fread("risk_scores.csv")
names(risk)[names(risk) == "iso3"] <- "RecipientISO"

comp <- merge(comp, risk[, .(RecipientISO, Year, gain, gain_class)], by = c("RecipientISO", "Year"), all.x = T)

comp <- merge(comp, pccs[, .(RecipientISO = iso3, Year = year, crisis_class)], by = c("RecipientISO", "Year"), all.x = T)

fwrite(comp, "f3_cca_ccm_analysis.csv")
