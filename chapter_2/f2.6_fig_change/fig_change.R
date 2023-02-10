required.packages <- c("data.table", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

ipc <- fread("datasets/IPC/all_food_insecurity.csv")

fig_first_last <- rbind(ipc[!is.na(fi_1), .SD[which.min(year), .(pos = "first", year, fi_1)], by = iso3], ipc[!is.na(fi_1), .SD[which.max(year), .(pos = "last", year, fi_1)], by = iso3])[order(iso3)]

fig_change <- dcast(fig_first_last, iso3 ~ pos, value.var = c("year", "fi_1"))

setwd(dirname(getActiveDocumentContext()$path))
fwrite(fig_change, "fig_change.csv")
