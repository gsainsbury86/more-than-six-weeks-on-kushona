

library(RColorBrewer)
library(ggplot2)
library(plotly)
library(plyr)
library(RPostgreSQL)
library(pool)
library(stringr)

DB_PARAMS = "database_params"
load(file = paste0('/Users/a1132077/Google Drive/D&D/Kushona/six-weeks-on-kushona/',DB_PARAMS))
pool = dbPool(drv <- dbDriver("PostgreSQL"),   dbname = database_params["dbname"],  host = database_params["host"],  port = database_params["port"],  user = database_params["user"],  password = database_params["password"])

killDbConnections <- function () {
  all_cons <- dbListConnections(drv = dbDriver("PostgreSQL"))
print(all_cons)
  for (con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

killDbConnections()



query = "select character.player_name, sub.name, xp, adventure_id
from(select dm_id, week, adventure.name, adventure_id, xp, rank() over (partition by adventure_id order by xp asc )
from xp_record join adventure on adventure_id = adventure.id
where Bonus = False
and xp > 0) as sub
join character on dm_id = character.id
where rank = 1
group by character.player_name, sub.name, xp, adventure_id
order by xp asc, character.player_name asc"

weeks <- dbGetQuery(pool, query)


#http://robinlovelace.net/r/2013/12/27/coxcomb-plots-spiecharts-R.html

desat <- function(cols, sat = 0.7) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1, ], X[2, ], X[3, ])
}

colour_spectrum <- brewer.pal(11, "Spectral")


new_spec <- list()
adventures_by_dm <- sort(paste(weeks$player_name, str_pad(weeks$adventure_id,3,pad=0), weeks$name),decreasing = TRUE)
curr_DM <- ""
for (i in 1:length(adventures_by_dm)) {
  if (grepl("Ming", adventures_by_dm[i])) {
    if (curr_DM == "Ming") {
      new_spec[i] = desat(new_spec[i - 1])
    } else{
      curr_DM <- "Ming"
      new_spec[i] = desat(colour_spectrum[3])
    }
  }
  if (grepl("Ben", adventures_by_dm[i])) {
    if (curr_DM == "Ben") {
      new_spec[i] = desat(new_spec[i - 1])
    } else{
      curr_DM <- "Ben"
      new_spec[i] = desat(colour_spectrum[9])
    }
  }
  if (grepl("George", adventures_by_dm[i])) {
    if (curr_DM == "George") {
      new_spec[i] = desat(new_spec[i - 1])
    } else{
      curr_DM <- "George"
      new_spec[i] = desat(colour_spectrum[4])
    }
  }
  if (grepl("Tyson", adventures_by_dm[i])) {
    if (curr_DM == "Tyson") {
      new_spec[i] = desat(new_spec[i - 1])
    } else{
      curr_DM <- "Tyson"
      new_spec[i] = desat(colour_spectrum[8])
    }
  }
}

new_spec <- setNames(new_spec, adventures_by_dm)
dm_spec <- c(
  "Ming" = colour_spectrum[3],
  "Ben" = colour_spectrum[9],
  "George" = colour_spectrum[4],
  "Tyson" = colour_spectrum[8]
)
final_spec <- unlist(c(dm_spec, new_spec))

ddply(weeks,  ~ player_name, summarise, xp = sum(xp))


# https://github.com/ropensci/plotly/issues/878
# waiting on plotly to work with polar coords

plot = ggplot(weeks, order = xp) +  
  geom_bar(
    aes(x = 1.5,
        y = xp,
        fill = player_name)
    ,
    position = "fill",
    stat = "identity",
    width = 3
  )  +  
  geom_bar(
    aes(
      x = 3.5,
      y = xp,
      fill = paste(player_name, str_pad(adventure_id,3,pad=0), name)
    )
    ,
    colour = "black",
    position = "fill",
    stat = "identity"
  ) +
  coord_polar("y") +
  scale_fill_manual(values = final_spec)


ggplotly(plot)
