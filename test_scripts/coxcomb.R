

library(RColorBrewer)
library(ggplot2)
library(plotly)
library(plyr)


# Establish connection to PoststgreSQL using RPostgreSQL
drv <- dbDriver("PostgreSQL")
# Full version of connection seetting
con <- dbConnect(
  drv = drv,
  dbname = "Kushona",
  host = "www.kushona.xyz",
  port = 5432,
  user = "readonlyuser",
  password = "readonlyuser"
)


query = "select character.player_name, sub.name, xp
from(select dm_id, week, adventure.name, xp, rank() over (partition by adventure_id order by xp asc )
from xp_record join adventure on adventure_id = adventure.id
where Bonus = False
and xp > 0) as sub
join character on dm_id = character.id
where rank = 1
group by character.player_name, sub.name, xp
order by character.player_name asc, xp asc"

weeks_rs <- dbSendQuery(con, query)
weeks <- fetch(weeks_rs, n = -1)
dbClearResult(weeks_rs)



#http://robinlovelace.net/r/2013/12/27/coxcomb-plots-spiecharts-R.html

desat <- function(cols, sat = 0.7) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1, ], X[2, ], X[3, ])
}

colour_spectrum <- brewer.pal(11, "Spectral")


new_spec <- list()
adventures_by_dm <- paste(weeks$player_name, weeks$name)
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


ggplotly(
  ggplot(weeks, order = xp) +  
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
      fill = paste(player_name, name)
    )
    ,
    colour = "black",
    position = "fill",
    stat = "identity"
  ) +
    #coord_polar("y") +
    scale_fill_manual(values = final_spec)
)