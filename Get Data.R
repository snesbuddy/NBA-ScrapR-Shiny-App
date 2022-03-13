#devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(writexl)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

seasons <- c(2001:2022)

i <- 0

for (i in seasons){
  junk <- game_logs(seasons = i)
  write_xlsx(junk, paste0(i, " NBA Season Logs.xlsx"))
}

for (i in seasons){
  assign(paste0("nbaLogs", i), read_xlsx(paste0(i, " NBA Season Logs.xlsx")))
}

logList <- mget(ls(pattern = "nbaLogs*"))

logs <- bind_rows(logList)
