suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
setwd(dirname(dirname(getActiveDocumentContext()$path)))

ipc_sub <- fread("project_data/ipc-ch_subnational.csv")

ipc_sub[, population := max(population), by = .(iso3, area, date, phase)]

ipc_sub_d <- dcast(ipc_sub, iso3 + area + year ~ phase, value.var = "population", fun.aggregate = max)[, `:=` (N = `1` + `2` + `3` + `4` + `5`, `3+` = `3` + `4` + `5`)]

ipc_sub_d[, `:=` (fi_0 = `3+`/N, fi_1 = ((`3` + `4`*2 + `5`*3)/3)/N, fi_2 = ((`3` + `4`*4 + `5`*9)/9)/N)]

fwrite(ipc_sub_d, "subnational_food_insecurity.csv")
