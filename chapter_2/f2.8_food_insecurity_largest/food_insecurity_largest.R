required.packages <- c("data.table", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

ipc <- fread("datasets/IPC/all_food_insecurity.csv")

food_insecure <- ipc[!is.na(fi_1), .SD[which.max(year), .(year, `3`, `4`, `5`, `3+`)], by = iso3]

setwd(dirname(getActiveDocumentContext()$path))
fwrite(food_insecure, "food_insecurity_largest.csv")
