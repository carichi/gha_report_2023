required.packages <- c("data.table", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))

vac <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

utd.vac <- vac[!is.na(people_vaccinated), .SD[which.max(as.IDate(date))], by = iso_code]
utd.vac <- utd.vac[, .(iso3 = iso_code, vac_share = people_vaccinated_per_hundred, full_vac_share = people_fully_vaccinated_per_hundred, vac_num = people_vaccinated, total_vac_num = total_vaccinations)]

trend.vac <- vac[, .(iso3 = iso_code, date = as.IDate(date), vac_share = people_vaccinated_per_hundred, full_vac_share = people_fully_vaccinated_per_hundred, vac_num = people_vaccinated, total_vac_num = total_vaccinations )]
pd.vac <- vac[, .(iso3 = iso_code, date = as.IDate(date), vac_pd = daily_vaccinations, vac_pd_pm = daily_vaccinations_per_million)]

fwrite(trend.vac, "vaccination_trends.csv")
fwrite(utd.vac, "vaccination_shares.csv")
fwrite(pd.vac, "vaccination_per_day.csv")
