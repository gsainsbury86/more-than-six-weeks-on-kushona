
#library(rsconnect)
#base_dir = "/Users/a1132077/development/personal/more-than-six-weeks-on-kushona/"
#base_dir = "C:\\Users\\George\\Google Drive\\D&D\\Kushona\\six-weeks-on-kushona"
#file.remove(paste0(base_dir,cache_files))
#deployApp(base_dir)


#list.of.packages <- c("markdown", "XML", "DT", "pool", "plotly","ggplot2","reshape2","tweenr","plyr","rvest","ndtv","RColorBrewer","network","RPostgreSQL","shiny")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)


killDbConnections <- function () {
  all_cons <- dbListConnections(drv = dbDriver("PostgreSQL"))
  print(all_cons)
  for (con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

killDbConnections()