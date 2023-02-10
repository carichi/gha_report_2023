required.packages <- c("data.table", "rstudioapi")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

ipc <- fread("")