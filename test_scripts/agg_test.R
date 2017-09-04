

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

query <- 
    'SELECT week as "Week",
    name as "Adventure",
    url as "Recounts",
player_name as "Author",
    min(string_agg) as "Adventurers"
    from
    adventure_recounts as sub
    group by player_name, url,week,
    name;'

weeks_rs <- dbSendQuery(con, query)
weeks <- fetch(weeks_rs, n = -1)
dbClearResult(weeks_rs)

aggregate(weeks[-3], by=list(weeks$Adventure), c)

