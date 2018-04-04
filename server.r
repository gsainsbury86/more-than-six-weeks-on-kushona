#database_params <- c(dbname = "********",
#                             host = "********",
#                             port = 0,
#                             user = "********",
#                             password = "********")
#save(... = database_params,file = DB_PARAMS)

#library(rsconnect)
#deployApp("/Users/a1132077/Google Drive/D&D/Kushona/six-weeks-on-kushona")


#killDbConnections <- function () {
#  all_cons <- dbListConnections(drv = dbDriver("PostgreSQL"))
#  print(all_cons)
#  for (con in all_cons)
#    +  dbDisconnect(con)
#  print(paste(length(all_cons), " connections killed."))
#}
# kill all connections on startup
#killDbConnections()


#filenames
DB_PARAMS = "database_params"
DATA_FILE = "data_file"
UPDATE_LG = "update_log"
ADVEN_LST = "adventure_list"
MAGIC_LST = "magic_item_list"
NETWORK_D = "network_diagram"
EXP_TABLE = "experience_table"
ADVEN_PLT = "adventure_plot"

#Establish connection to PoststgreSQL using RPostgreSQL
load(file = DB_PARAMS)
pool = dbPool(drv <- dbDriver("PostgreSQL"),   dbname = database_params["dbname"],  host = database_params["host"],  port = database_params["port"],  user = database_params["user"],  password = database_params["password"])
onStop(function() {
  poolClose(pool)
})

new_record = dbGetQuery(pool, "select max(updated) from update_log")[1, 1]

if(file.exists(UPDATE_LG)){
  load(file = UPDATE_LG)
  if (new_record > newest_record | !exists("newest_record")) {
    newest_record = new_record
    save(... = newest_record,file = UPDATE_LG)
    reload_required = TRUE
  }else{
    reload_required = FALSE
  }
}else{
  newest_record = new_record
  save(... = newest_record,file = UPDATE_LG)
  reload_required = TRUE
}

if(reload_required){
  weeks = dbGetQuery(pool, "select max(week) from adventure")
  max_week = weeks[1, 1]
  character_names = dbGetQuery(pool, "select name from character order by id;")
  player_names = dbGetQuery(pool, "select player_name from character order by id;")
  save(... = max_week, character_names, player_names, file=DATA_FILE)
}else{
  load(DATA_FILE)
}

colours = brewer.pal(length(character_names$name), "Spectral")

shinyServer(function(input, output, session) {
  output$player_experience <- renderDataTable({
    if(reload_required){
      player_experience_rs <- dbGetQuery(pool, 'select name as "Name", total_xp as "XP", level as "Level", race as "Race", class as "Class" from character_xp order by name')
      player_experience_table = datatable(player_experience_rs, selection = 'single', options = list(dom = 't', pageLength = -1), rownames = FALSE) %>% formatStyle( 'Name', target = 'row', backgroundColor = styleEqual(player_experience_rs$"Name", values = colours))
      save(... = player_experience_table, file = EXP_TABLE)
    }else{
      load(file=EXP_TABLE)
    }
    return(player_experience_table)
  })
  
  output$list_of_adventures <- renderDataTable({
    if(reload_required | !file.exists(ADVEN_LST)){
      adventure_results_rs = dbGetQuery(pool, 'SELECT week as "Week", name as "Adventure", dm as "DM", xp as "XP", bonus_xp as "Bonus XP", url as "Recounts", player_name as "Author", min(string_agg) as "Adventurers" from adventure_recounts as sub group by player_name, url ,week, name, xp, bonus_xp, dm order by week desc, player_name;')
    save(... = adventure_results_rs, file = ADVEN_LST)
    }else{
      load(ADVEN_LST)
    }
    
    table_of_adventures = adventure_results_rs
    
    table_of_adventures$Recounts = lapply(X = table_of_adventures$Recounts, FUN = function(url){ if(is.na(url)){ "" } else{ paste0("<a href=\"",url,"\">",tools::toTitleCase(sub(".*/","",gsub("-"," ",sub(".html","",url)))),"</a>") }})
    
    sel <- input$player_experience_rows_selected
    if (length(sel)) {
      table_of_adventures = table_of_adventures[grepl(character_names[sel, ], table_of_adventures$Adventurers), ]
    }
    list_of_adventures = datatable(table_of_adventures, selection = 'single', escape = FALSE, options = list(dom = 'tp', pageLength = 5), rownames = FALSE) %>% formatStyle('Author',target = 'row', backgroundColor = styleEqual(levels = player_names[[1]], values = colours))
    
    return(list_of_adventures)
  }, server = FALSE)
  
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    if (reload_required | !file.exists(NETWORK_D)) {
      network_list = list()
      
      # no edges "zeroth" week - as empty ajacency
      network_list[[1]] = as.network(x = matrix(0, 11, 11), directed = FALSE, loops = FALSE, matrix.type = "adjacency")
      
      network.vertex.names(x = network_list[[1]]) = character_names$name
      
      for (w in 1:max_week) {
        # create network edgelist matrix for week w
        query = paste0("select C1.id as c1id, C2.id as c2id, C1.name, C2.name, string_agg(adventure.name, ', ' ORDER by week) as adv_list, count(adventure.name) as num_adv from xp_record T1 join xp_record T2 on not T1.id = T2.id join adventure on T1.adventure_id = adventure.id join character C1 on T1.character_id = C1.id join character C2 on T2.character_id = C2.id where T1.bonus = False and T2.bonus = False and T1.adventure_id = T2.adventure_id and C1.id < C2.id and week <= ", w, "group by C1.id, C2.id order by C1.id, C2.id")
        df <- dbGetQuery(pool, query)
        
        # convert to network - edgelist (simpler)
        network_w <- as.network(x = df, directed = FALSE, loops = FALSE, matrix.type = "edgelist")
        
        # set names and attributes
        network.vertex.names(x = network_w) <- character_names$name
        set.edge.attribute(x = network_w, attrname = "num_adventures", value = df$num_adv)
        set.edge.attribute(x = network_w, attrname = "percentage_adventures", value = df$num_adv/w)
        set.edge.attribute(x = network_w, attrname = "adventure_list", value = df$adv_list)
        
        network_list[[w + 1]] <- network_w
      }
      
      # create dynamic network from list of networks
      net.dyn = networkDynamic( base.net = network_list[[1]], network.list = network_list, create.TEAs = TRUE, edge.TEA.names = c("num_adventures", "adventure_list"))
      
      compute.animation(net.dyn, animation.mode = "kamadakawai", slice.par = list(start = 0, end = max_week, interval = 1, aggregate.dur = 1, rule = 'any'))
      network_diagram = render.d3movie(net.dyn,usearrows = F, label = function(slice) { get.vertex.attribute(slice, "vertex.names") }, displaylabels = T, bg = "#ffffff", vertex.border = "#333333", vertex.cex = 3, label.col = "black", vertex.col = colours, edge.lwd = 5, edge.col = function(slice) { v = (1 - get.edge.value(slice, "percentage_adventures")); rgb(v, v, v) }, vertex.tooltip = function(slice) { paste("<b>", get.vertex.attribute(slice, "vertex.names"),"</b>")}, edge.tooltip = function(slice) { paste("<b>",get.edge.value(slice, "num_adventures")," Adventures:</b>",get.edge.value(slice, "adventure_list")) },render.par = list(tween.frames = 10, show.time = T), plot.par = list(mar = c(0, 0, 0, 0)), output.mode = 'htmlWidget')
      
      save(... = network_diagram, file = NETWORK_D)
    }
    else{ load(file = NETWORK_D) }
    return(network_diagram)
  })
  
  output$plotlyBarPlot <- renderPlotly({
    if (reload_required | !file.exists(ADVEN_PLT)) {
      n_frames <- (max_week + 1) * 1
      
      # definitely a better way to do this but it works.
      query_week_zero = "SELECT character.NAME, 0 AS \"Ming\",0 AS \"George\", 0 AS \"Tyson\", 0 AS \"Ben\", 0 AS week FROM character"
      
      query = query_week_zero
      
      for (i in 1:max_week) {
        query = paste(query, "UNION SELECT character.NAME, COALESCE(Sum(\"Ming\"), 0) AS \"Ming\", COALESCE(Sum(\"George\"), 0) AS \"George\", COALESCE(Sum(\"Tyson\"), 0) AS \"Tyson\", COALESCE(Sum(\"Ben\"), 0) AS \"Ben\", ",i, " AS week FROM (SELECT * FROM adventures_by_player_and_dm) AS T RIGHT JOIN character ON T.NAME = character.NAME WHERE T.week <= ", i,"GROUP BY character.NAME")
      }
      query = paste(query, "ORDER BY NAME, week")
      
      melted <- melt(data.frame(dbGetQuery(pool, query)), id.vars = c("name", "week"))
      colnames(melted) <- c("Name", "Week", "DM", "Sessions")
      
      plot_with_melted_tween = ggplot(data = melted, mapping = aes(x = Name, y = Sessions, fill = DM, frame = Week, cumulative = TRUE)) + geom_bar(stat = "identity", position = "dodge", width = 0.9) + scale_fill_manual(values = c("Ming" = colours[3], "Ben" = colours[9], "George" = colours[4], "Tyson" = colours[8])) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) + labs(fill = "DM", x = "",y = "Number of Adventures",frame = "Week")
      
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
      return(list_of_magic_items[grepl(character_names[sel, ], list_of_magic_items$Character),])
    }else{
      return(list_of_magic_items)
    }
  })
  
  output$list_of_magic_items = renderDataTable({
    if(reload_required | !file.exists(MAGIC_LST)){
      magic_items_rs = dbGetQuery(pool, 'select magic_item.name as "Item", item_type as "Type", requires_attunement, description, rules, adventure.name as "Found", character.name as "Character" FROM magic_item join adventure on adventure.id = adventure_id join character on character.id = character_id;')
      save(... = magic_items_rs, file = MAGIC_LST)
    }
    else{ load(MAGIC_LST)}

    list_of_magic_items = datatable( reactive_list_of_magic_items()[,!(names(magic_items_rs) %in% c('description', 'rules', 'requires+attunment'))], selection = 'single', escape = FALSE, options = list(dom = 'tp', pageLength = 5)
                                     , rownames = FALSE) %>% formatStyle('Character', target = 'row', backgroundColor = styleEqual(levels = character_names[[1]], values = colours))
    
    return(list_of_magic_items)
  }, server = FALSE)
  
  filteredTable_selected <- reactive({
    sel <- input$list_of_magic_items_rows_selected
    return(reactive_list_of_magic_items()[sel,])
  })
  
  #output$magic_item_frame <- renderText({
  observeEvent(input$list_of_magic_items_rows_selected,{
    load(MAGIC_LST)
    magic_item_sel <- input$list_of_magic_items_rows_selected
    player_sel <- input$player_experience_rows_selected
    #if(player_sel){
      #print(magic_items_rs[grepl(character_names[player_sel, ], magic_items_rs$Character), ][magic_item_sel])
    #}
    if(length(magic_item_sel)){
      #print(filteredTable_selected())
      
      #print(filteredTable_selected()$rules)
      
      item = gsub("'","&#39;",gsub("\\n","\\\\n",paste(filteredTable_selected()$description, "\\n", filteredTable_selected()$rules)))
      
      code = paste0("var converter = new showdown.Converter(); html = converter.makeHtml('",item,"'); document.getElementById('magic_item_container').innerHTML = html;")
      
      print(code)
      
      session$sendCustomMessage(type = 'jsCode', list(value = code))
    }
    ""
  })
  
  #session$onFlushed(function() {
    #session$sendCustomMessage(type = 'jsCode', list(value = "setTimeout(function() {set_recount_height();}, 1000);"))
  #})
  
})
