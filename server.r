#database_params <- c(dbname = "********",
#                             host = "********",
#                             port = 0,
#                             user = "********",
#                             password = "********")
#save(... = database_params,file = DB_PARAMS)

#filenames
DB_PARAMS = "database_params"
DATA_FILE = "data_file.cache"
ADVEN_LST = "adventure_list.cache"
MAGIC_LST = "magic_item_list.cache"
NETWORK_D = "network_diagram.cache"
EXP_TABLE = "experience_table.cache"
ADVEN_PLT = "adventure_plot.cache"
cache_files = c(DATA_FILE,ADVEN_LST,MAGIC_LST,NETWORK_D,EXP_TABLE,ADVEN_PLT)


#library(rsconnect)
#base_dir = "/Users/a1132077/development/personal/six-weeks-on-kushona/"
#base_dir = "C:\\Users\\George\\Google Drive\\D&D\\Kushona\\six-weeks-on-kushona"
#file.remove(paste0(base_dir,cache_files))
#deployApp(base_dir)


#list.of.packages <- c("markdown", "XML", "DT", "pool", "plotly","ggplot2","reshape2","tweenr","plyr","rvest","ndtv","RColorBrewer","network","RPostgreSQL","shiny")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)


#killDbConnections <- function () {
#  all_cons <- dbListConnections(drv = dbDriver("PostgreSQL"))
#  print(all_cons)
#  for (con in all_cons)
#    +  dbDisconnect(con)
#  print(paste(length(all_cons), " connections killed."))
#}

#killDbConnections()




#Establish connection to PoststgreSQL using RPostgreSQL
load(file = DB_PARAMS)
pool = dbPool(drv <- dbDriver("PostgreSQL"),   dbname = database_params["dbname"],  host = database_params["host"],  port = database_params["port"],  user = database_params["user"],  password = database_params["password"])
onStop(function() {
  poolClose(pool)
})


reload_required = function(filename){
  
  if (!file.exists(filename)){ return(TRUE) }
  else{
    latest_update = dbGetQuery(pool, "select max(updated) from update_log")[1, 1]
    cached = file.mtime(filename)
    if(is.na(latest_update) | is.na(cached)){ return(FALSE) }
    return(cached < latest_update)
  }
  }

if(reload_required(DATA_FILE) | !exists(DATA_FILE)){
  weeks = dbGetQuery(pool, "select max(week) from adventure")
  max_week = weeks[1, 1]
  character_names = dbGetQuery(pool, "select name from character order by id;")
  player_names = dbGetQuery(pool, "select player_name from character order by id;")
  save(... = max_week, character_names, player_names, file=DATA_FILE)
}else{
  load(DATA_FILE)
}

getPalette = colorRampPalette(brewer.pal(11, "Spectral"))
spectrum = getPalette(length(character_names$name))
#print(spectrum)

shinyServer(function(input, output, session) {
  output$player_experience <- renderDataTable({
    if(reload_required(EXP_TABLE) | !(file.exists(EXP_TABLE))){
      player_experience_rs <- dbGetQuery(pool, 'select name as "Name", xp as "XP", level as "Level", race as "Race", class as "Class" from character_xp order by id;')
      player_experience_table = datatable(player_experience_rs, selection = 'single', options = list(dom = 't', pageLength = -1), rownames = FALSE) %>% formatStyle( 'Name', target = 'row', backgroundColor = styleEqual(player_experience_rs$"Name", values = spectrum))
      save(... = player_experience_table, file = EXP_TABLE)
    }else{
      load(file=EXP_TABLE)
    }
    return(player_experience_table)
  })
  
  filtered_aventures = reactive({
    load(ADVEN_LST)
    table_of_adventures = adventure_results_rs
    sel <- input$player_experience_rows_selected
    if(length(sel)){
      table_of_adventures = table_of_adventures[grepl(character_names[sel, ], table_of_adventures$Adventurers), ]
    }else{
      return(table_of_adventures)
    }
  })
  
  output$list_of_adventures <- renderDataTable({
    if(reload_required(ADVEN_LST) | !file.exists(ADVEN_LST)){
      adventure_results_rs = dbGetQuery(pool, 'SELECT * from adventure_writeups;')
    save(... = adventure_results_rs, file = ADVEN_LST)
    }else{
      load(ADVEN_LST)
    }
    
    table_of_adventures = filtered_aventures()
    
    table_of_adventures$"Write Ups" = lapply(X = table_of_adventures$"Write Ups", FUN = function(url){ if(is.na(url)){ "" } else{ paste0("<a href=\"",url,"\">",tools::toTitleCase(sub(".*/","",gsub("-"," ",sub(".html","",url)))),"</a>") }})
    
    list_of_adventures = datatable(table_of_adventures[, !(names(table_of_adventures) %in% c("Recount","Recount Author"))], selection = 'single', escape = FALSE, options = list(dom = 'tp', pageLength = 5), rownames = FALSE) %>% formatStyle('Author',target = 'row', backgroundColor = styleEqual(levels = player_names[[1]], values = spectrum))
    
    return(list_of_adventures)
  }, server = FALSE)
  
  #our modal dialog box
  adventure_recount_popup <- function(failed=FALSE){
    sel = input$list_of_adventures_rows_selected
    table_of_adventures = filtered_aventures()
    #print(table_of_adventures[sel,]$"Recount")
    recount_text = table_of_adventures[sel,]$"Recount"
    if(is.na(recount_text)){ recount_text = ""}
    modalDialog(HTML(markdown::markdownToHTML(text = recount_text,fragment.only = TRUE)), title = table_of_adventures[sel,]$"Recount Author", easyClose = TRUE)
  }
  
  #event to trigger the modal box to appear
  observeEvent(input$list_of_adventures_rows_selected,{
    showModal(adventure_recount_popup())
  })

  
  output$netPlot <- ndtv:::renderNdtvAnimationWidget({
    if (reload_required(NETWORK_D) | !file.exists(NETWORK_D)) {
      network_list = list()
      
      # no edges "zeroth" week - as empty ajacency
      week_0 = dbGetQuery(pool, "select character.name from character where starting_level = 1;")
      
      empty_matrix = matrix(0, length(week_0$name), length(week_0$name))
      network_list[[1]] = as.network(x = empty_matrix, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
      set.vertex.attribute(x = network_list[[1]], attrname = "vertex.names",value = week_0$name)
      
      #empty_matrix = matrix(0, length(character_names$name), length(character_names$name))
      #network_list[[1]] = as.network(x = empty_matrix, directed = FALSE, loops = FALSE, matrix.type = "adjacency")
      #set.vertex.attribute(x = network_list[[1]], attrname = "vertex.names",value = character_names$name)
      
      for (w in 1:max_week) {
        # create network edgelist matrix for week w
        query = paste0("select C1.id as c1id, C2.id as c2id, C1.name, C2.name, string_agg(adventure.name, ', ' ORDER by week) as adv_list, count(adventure.name) as num_adv 
                       from record T1 join record T2 on not T1.id = T2.id 
                       join adventure on T1.adventure_id = adventure.id 
                       join character C1 on T1.character_id = C1.id 
                       join character C2 on T2.character_id = C2.id 
                       where T1.adventure_id = T2.adventure_id and C1.id < C2.id and week <= ", w, "
                       and (C1.final_week >= ",w," or C1.final_week is NULL) 
                       and (C2.final_week >= ",w," or C2.final_week is NULL)
                       group by C1.id, C2.id order by C1.id, C2.id")
        df <- dbGetQuery(pool, query)
        
        # convert to network - edgelist (simpler)
        network_matrix = as.matrix(df[,1:2])
        #attr(x = network_matrix, which = 'n') = length(character_names$name)
        network_w = as.network(x = network_matrix, directed = FALSE, loops = FALSE, matrix.type = "edgelist")
        
        xp_per_week = dbGetQuery(pool, paste0("WITH xp_table AS (
          SELECT character_1.id AS character_id,
          sum(adv_level.xp_per_session) AS xp
          FROM character character_1
          JOIN record ON character_1.id = record.character_id
          JOIN adventure ON record.adventure_id = adventure.id
          JOIN level adv_level ON adv_level.level = adventure.level
          WHERE adventure.week < ",w,"
          GROUP BY character_1.id
        )
        SELECT character.id,
        character.name,
        start_level.start_xp + COALESCE(xp_table.xp, 0::bigint) AS xp,
        l.level,
        character.race,
        character.class,
        character.class_option
        FROM xp_table
        RIGHT JOIN character ON xp_table.character_id = character.id
        JOIN level start_level ON start_level.level = character.starting_level
        LEFT JOIN LATERAL ( SELECT level.level
                            FROM level
                            WHERE (level.start_xp::numeric - (level.xp_per_session / 2)::numeric) < (start_level.start_xp + COALESCE(xp_table.xp, 0::bigint))::numeric
                            ORDER BY level.level DESC
                            LIMIT 1) l ON true
        ORDER BY xp_table.character_id;"))
        
        # set names and attributes
        set.vertex.attribute(x = network_w, attrname = "vertex.names",value = character_names$name)
        set.vertex.attribute(x = network_w, attrname = "vertex.xp",value = xp_per_week$xp)
        set.edge.attribute(x = network_w, attrname = "num_adventures", value = df$num_adv)
        set.edge.attribute(x = network_w, attrname = "percentage_adventures", value = df$num_adv/w)
        set.edge.attribute(x = network_w, attrname = "adventure_list", value = df$adv_list)
        
        network_list[[w + 1]] = network_w
      }
      
      vertex_spells = dbGetQuery(pool, "select min(week), max(week), character.id from adventure
                                 join record on adventure_id = adventure.id
                                 join character on character_id = character.id
                                 group by character.id
                                 order by character.id")
      

      # create dynamic network from list of networks
      #net.dyn = networkDynamic( base.net = network_list[[1]], network.list = network_list, create.TEAs = TRUE, edge.TEA.names = c("num_adventures", "adventure_list"), vertex.pid = "vertex.names")
      
      
      net.dyn = networkDynamic(network.list = network_list, 
                               create.TEAs = TRUE, 
                               edge.TEA.names = c("num_adventures", "adventure_list"), 
                               vertex.pid = "vertex.names")
      
      #vid = get.vertex.id(net.dyn,"Bryn")
      #deactivate.vertices(x=net.dyn,v=vid,onset=1,length=10)
      
      
      compute.animation(net.dyn, animation.mode = "kamadakawai", slice.par = list(start = 0, end = max_week, interval = 1, aggregate.dur = 1, rule = "any"))
      network_diagram = render.d3movie(net.dyn,usearrows = F, 
                                       label = function(slice) { get.vertex.attribute(slice, "vertex.names") }, 
                                       displaylabels = T, bg = "#ffffff", vertex.border = "#333333",
                                       label.col = "black", vertex.col = spectrum, edge.lwd = 5, edge.curved=0.4, 
                                       edge.col = function(slice) { v = (1 - get.edge.value(slice, "percentage_adventures")); rgb(v, v, v) }, 
                                       vertex.cex = function(slice) { 5*get.vertex.attribute(slice, "vertex.xp")/20000 }, #TODO: Max for the week
                                       vertex.tooltip = function(slice) { paste("<b>", get.vertex.attribute(slice, "vertex.names"),"</b>")}, 
                                       edge.tooltip = function(slice) { paste("<b>",get.edge.value(slice, "num_adventures")," Adventures:</b>", get.edge.value(slice, "adventure_list")) },
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
      query_week_zero = "SELECT character.NAME, 0 AS \"Ming\",0 AS \"George\", 0 AS \"Tyson\", 0 AS \"Ben\", 0 AS week FROM character"
      
      query = query_week_zero
      
      for (i in 1:max_week) {
        query = paste(query, "UNION SELECT character.NAME, COALESCE(Sum(\"Ming\"), 0) AS \"Ming\",",
                      " COALESCE(Sum(\"George\"), 0) AS \"George\", COALESCE(Sum(\"Tyson\"), 0) AS \"Tyson\",",
                      " COALESCE(Sum(\"Ben\"), 0) AS \"Ben\", ",i, " AS week FROM",
                      " (SELECT * FROM adventures_by_player_and_dm) AS T RIGHT JOIN character ON T.NAME = character.NAME WHERE T.week <= ", i,"GROUP BY character.NAME")
      }
      query = paste(query, "ORDER BY NAME, week")
      
      melted <- melt(data.frame(dbGetQuery(pool, query)), id.vars = c("name", "week"))
      colnames(melted) <- c("Name", "Week", "DM", "Sessions")
      
      plot_with_melted_tween = ggplot(data = melted, mapping = aes(x = Name, y = Sessions, fill = DM, frame = Week, cumulative = TRUE)) + 
        geom_bar(stat = "identity", position = "dodge", width = 0.9) + scale_fill_manual(values = c("Ming" = spectrum[3], "Ben" = spectrum[9], "George" = spectrum[4], "Tyson" = spectrum[8])) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) + 
        labs(fill = "DM", x = "",y = "Number of Adventures",frame = "Week")
      
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
    if(reload_required(MAGIC_LST) | !file.exists(MAGIC_LST)){
      magic_items_rs = dbGetQuery(pool, 'select magic_item.name as "Item", item_type as "Type", requires_attunement, description, rules, adventure.name as "Found", character.name as "Character" FROM magic_item join adventure on adventure.id = adventure_id join character on character.id = character_id ORDER BY adventure.id DESC;')
      save(... = magic_items_rs, file = MAGIC_LST)
    }
    else{ load(MAGIC_LST)}

    list_of_magic_items = datatable( reactive_list_of_magic_items()[,!(names(magic_items_rs) %in% c('description', 'rules', 'requires_attunement'))], selection = 'single', escape = FALSE, options = list(dom = 'tp', pageLength = 5), rownames = FALSE) %>% formatStyle('Character', target = 'row', backgroundColor = styleEqual(levels = character_names[[1]], values = spectrum))
    return(list_of_magic_items)
  }, server = FALSE)
  
  filteredTable_selected <- reactive({
    sel <- input$list_of_magic_items_rows_selected
    return(reactive_list_of_magic_items()[sel,])
  })
  
  output$magic_item_frame <- renderText({
  #observeEvent(input$list_of_magic_items_rows_selected,{
    load(MAGIC_LST)
    magic_item_sel <- input$list_of_magic_items_rows_selected
    
    if(length(magic_item_sel)){
        item = filteredTable_selected()
        if(item$requires_attunement){
          req = "_Requires Attunement_"
        }else{ 
          req = ""
        }
        if(!is.na(item$description)){
          desc = item$description
        }else{ 
          desc = ""
        }
        if(!is.na(item$rules)){
          rules = item$rules
        }else{ 
          rules = ""
        }
        
        markdown = glue(paste("# {item$Item} #",
                          "#### {req} ####",
                          "{desc}",
                          "",
                          "---",
                          "{rules}",sep="\n"))
        
        item_html = markdown::markdownToHTML(text=markdown,fragment.only = TRUE)
    }else{
      item_html = ""
    }
    return(item_html)
  })
  
})
