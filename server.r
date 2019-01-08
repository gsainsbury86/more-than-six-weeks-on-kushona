#database_params <- c(dbname = "********",
#                             host = "********",
#                             port = 0,
#                             user = "********",
#                             password = "********")
#save(... = database_params,file = DB_PARAMS)

DB_PARAMS = "database_params"
CACHE_DIR = "cache_files"
DATA_FILE = paste(CACHE_DIR,"data_file.cache", sep="/")
ADVEN_LST = paste(CACHE_DIR,"adventure_list.cache", sep="/")
MAGIC_LST = paste(CACHE_DIR,"magic_item_list.cache", sep="/")
NETWORK_D = paste(CACHE_DIR,"network_diagram.cache", sep="/")
EXP_TABLE = paste(CACHE_DIR,"experience_table.cache", sep="/")
ADVEN_PLT = paste(CACHE_DIR,"adventure_plot.cache", sep="/")
cache_files = c(DATA_FILE,ADVEN_LST,MAGIC_LST,NETWORK_D,EXP_TABLE,ADVEN_PLT)

load(file = DB_PARAMS)
pool = dbPool(drv <- dbDriver("PostgreSQL"),   dbname = database_params["dbname"],  host = database_params["host"],  port = database_params["port"],  user = database_params["user"],  password = database_params["password"])
onStop(function() {
  poolClose(pool)
})

reload_required = function(filename){
  
  dir.create(CACHE_DIR, showWarnings = FALSE)

  if (!file.exists(filename)){ return(TRUE) }
  else{
    latest_update = dbGetQuery(pool, read_file("sql_queries/latest_update_query.sql"))[1, 1]
    cached = file.mtime(filename)
    if(is.na(latest_update) | is.na(cached)){ return(FALSE) }
    return(cached < latest_update)
  }
}

if(reload_required(DATA_FILE) | !exists(DATA_FILE)){
  weeks = dbGetQuery(pool, read_file("sql_queries/max_week_query.sql"))
  max_week = weeks[1, 1]
  character_names = dbGetQuery(pool, read_file("sql_queries/character_names_query.sql"))
  player_names = dbGetQuery(pool, read_file("sql_queries/player_names_query.sql"))
  save(... = max_week, character_names, player_names, file=DATA_FILE)
}else{
  load(DATA_FILE)
}

cols = brewer.pal(11, "Spectral")

cols1 <- readhex(file = textConnection(paste(cols, collapse = "\n")),
                 class = "RGB")
#transform to hue/lightness/saturation colorspace
cols1 <- as(cols1, "HLS")
#additive decrease of lightness
cols1@coords[, "L"] <- pmax(0, 0.25)#cols1@coords[, "L"] - 0.1)
cols1 <- as(cols1, "RGB")
cols1 <- hex(cols1)
spectrum = cols1

shinyServer(function(input, output, session) {
  output$player_experience <- renderDataTable({
    if(reload_required(EXP_TABLE) | !(file.exists(EXP_TABLE))){
      player_experience_rs <- dbGetQuery(pool, glue(read_file("sql_queries/player_experience_table_query.sql")))
      save(... = player_experience_rs, file = EXP_TABLE)
    }else{
      load(file=EXP_TABLE)
    }
    g_player_experience_rs <<- player_experience_rs
    player_experience_table = datatable(player_experience_rs, selection = 'single', options = list(dom = 't', pageLength = -1), rownames = FALSE) %>% formatStyle( 'Player Name', target = 'row', "-webkit-text-color" = "0px black", "-webkit-text-fill-color" = styleEqual(unique(player_names$player_name), values = spectrum))
    
    return(player_experience_table)
  })
  
  filtered_aventures = reactive({
    load(ADVEN_LST)
    table_of_adventures = adventure_results_rs
    sel <- input$player_experience_rows_selected
    if(length(sel)){
      table_of_adventures = table_of_adventures[grepl(g_player_experience_rs[[sel,2]], table_of_adventures$Adventurers), ]
    }else{
      return(table_of_adventures)
    }
  })
  
  output$list_of_adventures <- renderDataTable({
    if(reload_required(ADVEN_LST) | !file.exists(ADVEN_LST)){
      adventure_results_rs = dbGetQuery(pool, read_file("sql_queries/adventure_stories_query.sql"))
      save(... = adventure_results_rs, file = ADVEN_LST)
    }else{
      load(ADVEN_LST)
    }
    
    table_of_adventures = filtered_aventures()
    
    table_of_adventures$"Stories" = lapply(X = table_of_adventures$"Stories", FUN = function(url){ if(is.na(url)){ "" } else{ paste0("<a href=\"",url,"\">",tools::toTitleCase(sub(".*/","",gsub("-"," ",sub(".html","",url)))),"</a>") }})
    
    table_of_adventures$"Summary" = lapply(X = table_of_adventures$"Summary", FUN = function(x){if(is.na(x)){ "" } else{ markdown::markdownToHTML(text = x,fragment.only = TRUE) }})
    
    list_of_adventures = datatable(table_of_adventures, selection = 'none', escape = FALSE, options = list(columnDefs = list(list(visible=FALSE, targets=c(4,6))),dom = 'tp', pageLength = 5), rownames = FALSE) %>% formatStyle('Summary Author',target = 'row', color = styleEqual(levels = unique(player_names$player_name), values = spectrum))
    
    return(list_of_adventures)
  }, server = FALSE)
  
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    if (reload_required(NETWORK_D) | !file.exists(NETWORK_D)) {
      network_list = list()
      
      # no edges "zeroth" week - as empty ajacency
      week_0 = dbGetQuery(pool, read_file("sql_queries/graph_week_zero_query.sql"))
      
      empty_matrix = matrix(0, length(week_0$name), length(week_0$name))
      network_list[[1]] = as.network(x = empty_matrix, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
      set.vertex.attribute(x = network_list[[1]], attrname = "vertex.names",value = week_0$name)
      set.vertex.attribute(x = network_list[[1]], attrname = "vertex.level",value = 1)
      set.vertex.attribute(x = network_list[[1]], attrname = "vertex.player_name",value = player_names$player_name)
      set.vertex.attribute(x = network_list[[1]], attrname = "vertex.alpha",value = pmin(character_names$final_adventuring_week/1,1))
      
      xp_per_week = dbGetQuery(pool, read_file("sql_queries/graph_xp_per_week_query.sql"))
      for (w in 1:max_week) {
        # create network edgelist matrix for week w
        query = glue(read_file("sql_queries/graph_week_w_query.sql"))
        df <- dbGetQuery(pool, query)
        
        # convert to network - edgelist (simpler)
        network_matrix = as.matrix(df[,1:2])
        network_w = as.network(x = network_matrix, directed = FALSE, loops = FALSE, matrix.type = "edgelist")
        
        level_this_week = aggregate(x = xp_per_week[xp_per_week$adventure_week <= w,]$level, by=list(xp_per_week[xp_per_week$adventure_week <= w,]$character_id), max)
        Group.1 = seq(1,11)
        x = rep(1,11)
        fix_week_one = rbind(level_this_week,data.frame(Group.1,x))
        level_this_week_fixed = aggregate(fix_week_one$x,by=list(fix_week_one$Group.1), max)
        
        # set names and attributes
        set.vertex.attribute(x = network_w, attrname = "vertex.names",value = character_names$name)
        set.vertex.attribute(x = network_w, attrname = "vertex.player_name",value = player_names$player_name)
        set.vertex.attribute(x = network_w, attrname = "vertex.level",value = level_this_week_fixed$x)
        set.vertex.attribute(x = network_w, attrname = "vertex.alpha",value = pmin(character_names$final_adventuring_week/w,1))
        set.edge.attribute(x = network_w, attrname = "num_adventures", value = df$num_adv)
        set.edge.attribute(x = network_w, attrname = "weight", value = df$num_adv)
        set.edge.attribute(x = network_w, attrname = "percentage_adventures", value = df$num_adv/w)
        set.edge.attribute(x = network_w, attrname = "adventure_list", value = df$adv_list)
        
        network_list[[w + 1]] = network_w
      }
      
      # create dynamic network from list of networks
      net.dyn = networkDynamic(network.list = network_list, 
                               create.TEAs = TRUE, 
                               edge.TEA.names = c("num_adventures", "adventure_list"), 
                               vertex.pid = "vertex.names")
      
      
      compute.animation(net.dyn, animation.mode = "kamadakawai", slice.par = list(start = 0, end = max_week, interval = 1, aggregate.dur = 1, rule = "any"))
      network_diagram = render.d3movie(net.dyn,usearrows = F, 
                                       label = function(slice) { get.vertex.attribute(slice, "vertex.names") }, 
                                       displaylabels = T, bg = "#ffffff", vertex.border = "#333333",
                                       label.col = "black", 
                                       vertex.col = function(slice){
                                         spectrum_extended = spectrum[match(get.vertex.attribute(slice, "vertex.player_name"),player_names$player_name)]
                                         unname(mapply(spectrum_extended,FUN = adjustcolor,alpha.f = get.vertex.attribute(slice,"vertex.alpha")))
                                       }, 
                                       edge.lwd = function(slice){
                                         if(typeof(get.edge.attribute(slice, "num_adventures")) == "double"){
                                           get.edge.attribute(slice, "num_adventures")^(1/3)
                                         }else{
                                           1
                                         }
                                       },
                                       edge.curved=.4, 
                                       edge.col = function(slice) { v = (1 - get.edge.attribute(slice, "percentage_adventures")); rgb(v, v, v) }, 
                                       vertex.sides = function(slice){
                                         # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
                                         pmax(3,get.vertex.attribute(slice, "vertex.level"))
                                       },
                                       vertex.cex = function(slice) { get.vertex.attribute(slice, "vertex.level")^(2/3)},
                                       vertex.tooltip = function(slice) { paste("<b>", get.vertex.attribute(slice, "vertex.names"), get.vertex.attribute(slice, "vertex.level"),"</b>")}, 
                                       edge.tooltip = function(slice) { paste("<b>",get.edge.attribute(slice, "num_adventures")," Adventures:</b>", get.edge.attribute(slice, "adventure_list")) },
                                       render.par = list(tween.frames = 10, show.time = T), 
                                       plot.par = list(mar = c(0, 0, 0, 0)), 
                                       output.mode = 'htmlWidget')
      
      save(... = network_diagram, file = NETWORK_D)
    }
    else{ load(file = NETWORK_D) }
    return(network_diagram)
  })
  
  output$plotlyBarPlot <- renderPlotly({
    if (reload_required(ADVEN_PLT) | !file.exists(ADVEN_PLT)) {
      
      # definitely a better way to do this but it works.
      query = read_file("sql_queries/bar_plot_week_zero_query.sql")
      
      for (w in 1:max_week) {
        query = paste(query, glue(read_file("sql_queries/bar_plot_week_w_query.sql")))
      }
      
      melted <- melt(data.frame(dbGetQuery(pool, query)), id.vars = c("player_name", "week"))
      colnames(melted) <- c("Name", "Week", "DM", "Sessions")
      
      plot_with_melted_tween = ggplot(data = melted, aes(x = Name, y = Sessions, fill = DM, frame = Week)) + 
        geom_bar(stat = "identity", position = "dodge", width = 0.9) + 
        scale_fill_manual(values = c("Ming" = spectrum[3], "Ben" = spectrum[9], "George" = spectrum[4], "Tyson" = spectrum[8])) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) + 
        labs(fill = "DM", x = "",y = "Number of Adventures",frame = "Week") +
        guides(guide_legend())
      
      
      adventure_plot = ggplotly(plot_with_melted_tween) %>% config(displayModeBar = F) %>% layout(xaxis = list(fixedrange = TRUE)) %>% layout(yaxis = list(fixedrange = TRUE))  %>% layout(showlegend = FALSE)
      
      save(... = adventure_plot, file = ADVEN_PLT)
    }
    else{ load(file = ADVEN_PLT) }
    return(adventure_plot)
  })
  
  reactive_list_of_magic_items = reactive({
    load(MAGIC_LST)
    list_of_magic_items = magic_items_rs
    sel <- input$player_experience_rows_selected
    if(length(sel)){
      return(list_of_magic_items[grepl(g_player_experience_rs[[sel,2]], list_of_magic_items$Character),])
    }else{
      return(list_of_magic_items)
    }
  })
  
  output$list_of_magic_items = renderDataTable({
    if(reload_required(MAGIC_LST) | !file.exists(MAGIC_LST)){
      magic_items_rs = dbGetQuery(pool, glue(read_file("sql_queries/magic_item_query.sql")))
      save(... = magic_items_rs, file = MAGIC_LST)
    }
    else{ load(MAGIC_LST)}
    
    list_of_magic_items = datatable( reactive_list_of_magic_items(), selection = 'single', escape = FALSE, options = list(dom = 'tp', pageLength = 10, columnDefs = list(list(visible=FALSE, targets=c(2,3,4,6)))), rownames = FALSE) %>% formatStyle('player_name', target = 'row', color = styleEqual(levels = unique(player_names$player_name), values = spectrum))
    return(list_of_magic_items)
  }, server = FALSE)
  
  filteredTable_selected <- reactive({
    sel <- input$list_of_magic_items_rows_selected
    return(reactive_list_of_magic_items()[sel,])
  })
  
  output$magic_item_frame <- renderText({
    load(MAGIC_LST)
    magic_item_sel <- input$list_of_magic_items_rows_selected
    
    if(length(magic_item_sel)){
      item = filteredTable_selected()
      if(item$requires_attunement){ req = "_Requires Attunement_" }else{ req = "" }
      if(!is.na(item$description)){ desc = item$description }else{ desc = "" }
      if(!is.na(item$rules)){ rules = item$rules } else{ rules = "" }
      
      markdown = glue(paste("# {item$Item} #",
                            "#### {req} ####",
                            "{desc}",
                            "",
                            "---",
                            "{rules}",sep="\n"))
      
      item_html = markdown::markdownToHTML(text=markdown,fragment.only = TRUE)
    }else{ item_html = "" }
    return(item_html)
  })
  
})
