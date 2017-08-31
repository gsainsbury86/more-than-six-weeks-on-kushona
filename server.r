#TODO: plotly 
# remove Week and TRUE from tooltip.
# Fix background colour

#TODO: General
# background in grey

#TODO: Text
# highlight numbers using spans

# Establish connection to PoststgreSQL using RPostgreSQL

#database_params <- c(dbname = "********",
#                             host = "********",
#                             port = 0,
#                             user = "********",
#                             password = "********")
#save(... = database_params,file = "/Users/george/Dropbox/D&D/Kushona/six-weeks-on-kushona/database_params")

load(file = "/Users/George/Dropbox/D&D/Kushona/six-weeks-on-kushona/database_params")
pool <- dbPool(drv <- dbDriver("PostgreSQL"), 
               dbname = database_params["dbname"],
               host = database_params["host"],
               port = database_params["port"],
               user = database_params["user"],
               password = database_params["password"])

weeks <- dbGetQuery(pool, "select max(week) from adventure")
max_week <<- weeks[1, 1]
character_names <<- dbGetQuery(pool, "select name from character order by id")


colour_spectrum <- brewer.pal(length(character_names$name), "Spectral")

wordcount <- function(str) {
  sapply(gregexpr("\\b\\W+\\b", str, perl = TRUE), function(x)
    sum(x > 0)) + 1
}

killDbConnections <- function () {
  all_cons <- dbListConnections(drv = dbDriver("PostgreSQL"))
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

prepare_network_diagram <- function (){
  
  network_list <- list()
  
  # no edges "zeroth" week - as empty ajacency
  network_list[[1]] <- as.network(
    x = matrix(0, 11, 11),
    directed = FALSE,
    loops = FALSE,
    matrix.type = "adjacency"
  )
  
  network.vertex.names(x = network_list[[1]]) <- character_names$name
  
  for (w in 1:max_week) {
    # create network edgelist matrix for week w
    query = paste0(
      "select C1.id as c1id, 
        C2.id as c2id, 
        C1.name, 
        C2.name, 
        string_agg(adventure.name, ', ' ORDER by week) as adv_list, 
      count(adventure.name) as num_adv
      from xp_record T1
      join xp_record T2 on not T1.id = T2.id
      join adventure on T1.adventure_id = adventure.id
      join character C1 on T1.character_id = C1.id
      join character C2 on T2.character_id = C2.id
      where T1.bonus = False
      and T2.bonus = False
      and T1.adventure_id = T2.adventure_id
      and C1.id < C2.id
      and week <= ",
      w,
      "group by C1.id, C2.id
      order by C1.id, C2.id"
    )
    df <- dbGetQuery(pool, query)
    
    # convert to network - edgelist (simpler)
    network_w <- as.network(
      x = df,
      directed = FALSE,
      loops = FALSE,
      matrix.type = "edgelist"
    )

  # set names and attributes
    network.vertex.names(x = network_w) <- character_names$name
    set.edge.attribute(x = network_w,
                       attrname = "num_adventures",
                       value = df$num_adv)
    set.edge.attribute(x = network_w,
                       attrname = "adventure_list",
                       value = df$adv_list)
    
    network_list[[w + 1]] <- network_w
  }
  
  # create dynamic network from list of networks
  net.dyn <-
    networkDynamic(
      base.net = network_list[[1]],
      network.list = network_list,
      create.TEAs = TRUE,
      edge.TEA.names = c("num_adventures", "adventure_list")
    )
  
  compute.animation(
    net.dyn,
    animation.mode = "kamadakawai",
    slice.par = list(
      start = 0,
      end = max_week,
      interval = 1,
      aggregate.dur = 1,
      rule = 'any'
    )
  )
  
  network_diagram <<- render.d3movie(
    net.dyn,
    usearrows = F,
    label = function(slice) {
      substr(get.vertex.attribute(slice, "vertex.names"), 1, 1)
    },
    displaylabels = T,
    bg = "#ffffff",
    vertex.border = "#333333",
    vertex.cex = 3,
    label.col = "black",
    vertex.col = colour_spectrum,
    edge.lwd = function(slice) {
      get.edge.value(slice, "num_adventures") * 3
    },
    edge.col = function(slice) {
      v = (1 - get.edge.value(slice, "num_adventures") / (max_week))
      rgb(v, v, v)
    },
    vertex.tooltip = function(slice) {
      paste("<b>",
            get.vertex.attribute(slice, "vertex.names"),
            "</b>")
    }  ,
    edge.tooltip = function(slice) {
      paste("<b>Adventures:</b>",
            get.edge.value(slice, "adventure_list"))
    },
    render.par = list(tween.frames = 10, show.time = T),
    #n_frames/(max_week+1), show.time = F),
    plot.par = list(mar = c(0, 0, 0, 0))
    ,
    output.mode = 'htmlWidget'
    #,d3.options = list(playControls=FALSE,animateOnLoad=TRUE,slider=FALSE,durationControl=FALSE)
  )
}


prepare_adventure_plot <- function(){

  n_frames <- (max_week + 1) * 1
  
  # definitely a better way to do this but it works.
  query_week_zero = "
  SELECT character.NAME,
  0 AS \"Ming\",
  0 AS \"George\",
  0 AS \"Tyson\",
  0 AS \"Ben\",
  0 AS week
  FROM character"
  
  query = query_week_zero
  for (i in 1:max_week) {
    query = paste(
      query,
      "UNION
      SELECT character.NAME,
      COALESCE(Sum(\"Ming\"), 0) AS \"Ming\",
      COALESCE(Sum(\"George\"), 0) AS \"George\",
      COALESCE(Sum(\"Tyson\"), 0) AS \"Tyson\",
      COALESCE(Sum(\"Ben\"), 0) AS \"Ben\",
      ",
      i,
      " AS week
      FROM
      (SELECT *
      FROM adventures_by_player_and_dm) AS T
      RIGHT JOIN character ON T.NAME = character.NAME
      WHERE T.week <= ",
      i,
      "
      GROUP BY character.NAME"
      )
  }
  query = paste(query, "ORDER BY NAME, week")
  
  df <- dbGetQuery(pool, query)
  
  # create tweens and melt DM columns
  query_framed <- data.frame(df)
#  tween_data <-
#    tween_elements(
#      data = query_framed,
#      time = 'week',
#      group = 'name',
#      ease = 'ease',
#      nframes = n_frames - 1
#    )
#  melted <-melt(tween_data, id.vars = c("week", ".frame", ".group"))
  
#  mapping = aes(
#    x = .group,
#    y = value,
#    fill = variable,
#    frame = .frame,
#    cumulative = TRUE,
#    text = paste(.group)
#  )
  
    melted <- melt(query_framed, id.vars = c("name", "week"))
    colnames(melted) <- c("Name", "Week", "DM", "Sessions")
    
  plot_with_melted_tween <-
    (
      ggplot(
        data = melted,
        mapping = aes(
          x = Name,
          y = Sessions,
          fill = DM,
          frame = Week,
          cumulative = TRUE
          #text = paste(.group)
        )
      ) + geom_bar(
        stat = "identity",
        position = "dodge",
        width = 0.9
      ) + scale_fill_manual(
        values = c(
          "Ming" = colour_spectrum[3],
          "Ben" = colour_spectrum[9],
          "George" = colour_spectrum[4],
          "Tyson" = colour_spectrum[8]
        )
      )
      + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        )
      ) + labs(fill = "DM", x= "", y = "Number of Adventures", frame = "Week")
    )
  adventure_plot <<- ggplotly(plot_with_melted_tween, height = 395, width = 550) %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))  %>% layout(showlegend=FALSE)

  
}


prepare_recount_word_count <- function(){
  recount_info <-
    dbGetQuery(
      pool,
      "select count(distinct recount.id) as num_recounts,
      count(distinct recount.character_id) as num_authors,
      sum(xp)
      from recount
      join xp_record on recount.adventure_id = xp_record.adventure_id
      where xp_record.Bonus = True"
    )
  
  urls <- dbGetQuery(pool, "select url from recount")
  
  
  word_count = 0
  
  for (i in 1:length(urls[, 1])) {
    blog <- read_html(urls[i, 1])
    content_html <- html_nodes(blog, '.post-body')
    content_text <- html_text(content_html)
    word_count = word_count + (wordcount(content_text))
  }
  
  
  recount_word_count <<- paste0('<span id = "num_recounts">',
    recount_info[1, 1],
    '</span> recounts with over <span id = "num_words">',
    round_any(word_count, 1000, floor),
    '</span> words from <span id = "num_authors">',
    recount_info[1, 2],
    '</span> authors, earning <span id = "bonus_xp">',
    recount_info[1, 3],
    '</span> Bonus XP.'
  )
}

prepare_network_diagram()
prepare_adventure_plot()
prepare_recount_word_count()

shinyServer(function(input, output) {
  output$player_experience <- renderDataTable({
    datatable(df <-
      dbGetQuery(pool, 'select name as "Name", 
                 total_xp as "XP", 
                 level as "Level",
                 race as "Race",
                 class as "Class"
                 from character_xp
                 order by name'),
      selection = 'single',
      options = list(dom = 't', pageLength = -1),
      rownames = FALSE
    ) %>% formatStyle(
      'Name',
      target='row',
      backgroundColor = styleEqual(df$"Name",values = colour_spectrum)
    )
  })
  
  
  output$list_of_adventures <- renderDataTable({
    
    df <-
      dbGetQuery(
        pool,
        'SELECT week as "Week",
        name as "Adventure",
        string_agg(\'<a href=\"\' || url || \'\"> \' || player_name || \'</a>\',\', \'::text) as "Recounts",
        min(string_agg) as "Adventurers"
        from
        adventure_recounts as sub
        group by week,
        name;'
        )

    gradients = list()
    
    #background: linear-gradient(red, yellow, green);
    
    for(i in 1:length(df$Adventurers)){
      advs <- strsplit(x = df$Adventurers[i],split = ",")
      advs <- lapply(advs,trimws)
      gradient_func <- "linear-gradient( to right, "
      for(a in 1:length(advs[[1]])){
        index <- which(character_names$name == advs[[1]][a])
        c2r <- col2rgb(colour_spectrum[index])
        gradient_func <- paste0(gradient_func,paste0('rgb(',c2r[1],',',c2r[2],',',c2r[3],')'),',')
      }
      gradient_func <- substr(gradient_func, 1, nchar(gradient_func)-1)
      gradient_func <- paste0(gradient_func,')')
      gradients[[i]] <- gradient_func
    }
    
    sel <- input$player_experience_rows_selected
    if(length(sel)){
      datatable(df[grepl(character_names[sel,],df$Adventurers),],
                selection = 'none',
                escape = FALSE,
                options = list(dom = 't', pageLength = -1),
                rownames = FALSE
      )  %>% formatStyle(
        'Adventurers',
        #target='row',
        background = styleEqual(df$"Adventurers",values = gradients)
      )
    }else{
      datatable(df,
                selection = 'none',
                escape = FALSE,
                options = list(dom = 't', pageLength = -1),
                rownames = FALSE
      )  %>% formatStyle(
        'Adventurers',
        #target='row',
        background = styleEqual(df$"Adventurers",values = gradients)
      )
    }
},server = FALSE)
  
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    network_diagram
  })
  
  output$plotlyBarPlot <- renderPlotly({
    adventure_plot
    })

  output$recount_word_count <- renderText({
    recount_word_count
  })
  
  output$character_count <- renderText({
    char_info <-
      dbGetQuery(
        pool,
        "select count(distinct player_name) as num_players,
        count(distinct name) as num_characters,
        count(distinct name) - count(distinct player_name) as num_deaths
        from character"
      )
    
    
    char_string = paste0('<span id = "num_players">',
                         char_info[1, 1],
           '</span> players, <span id = "num_chars">',
           char_info[1, 2],
           '</span> characters and <span id = "num_deaths">',
           char_info[1, 3],
           '</span> deaths!')
    char_string
    
  })
  
  output$session_count <- renderText({
    session_info <-
      dbGetQuery(
        pool,
        "select count(distinct adventure.id) as num_adventures,
        count(distinct dm_id) as num_dms,
        sum(xp)
        from adventure
        join xp_record on adventure_id = adventure.id
        where Bonus = False
        and xp > 0"
      )
    
    out_string <- paste0('<span id = "num_adventures">',
      session_info[1, 1],
      '</span> adventures by <span id = "num_DMs">',
      session_info[1, 2],
      '</span> DMs, earning a total of <span id = "session_xp">',
      session_info[1, 3],
      '</span> XP.'
    )
    out_string
    
  })
  
})
