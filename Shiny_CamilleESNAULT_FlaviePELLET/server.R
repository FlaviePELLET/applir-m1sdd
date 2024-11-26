
# Définir la logique du serveur
server <- function(input, output, session) {
  
  # Calcul du résumé statistique
  output$str_stats <- renderPrint({
    str_stats <- str(df)
    str_stats
  })
  
  # Fonction pour mettre à jour les choix disponibles dans les filtres de département et de commune
  update_filters <- function(region) {
    departments <- unique(df[df$Region == region, "Departement"])
    updateSelectInput(session, "selected_department", choices = departments, selected = departments[1])
    
    communes <- unique(df[df$Region == region, "Nom"])
    js$initAutocomplete(communes)
  }
  
  # Observer les changements dans la sélection de la région
  observeEvent(input$selected_region, {
    region <- input$selected_region
    update_filters(region)
  })
  
  filtered_data <- reactive({
    req(input$selected_region)
    filtered_df <- df[df$Region == input$selected_region, ]
    if (!is.null(input$selected_department) && length(input$selected_department) > 0) {
      filtered_df <- filtered_df[filtered_df$Departement %in% input$selected_department, ]
    }
    if (!is.null(input$selected_commune) && input$selected_commune != "") {
      filtered_df <- filtered_df[grep(input$selected_commune, filtered_df$Nom, ignore.case = TRUE), ]
    }
    return(filtered_df)
  })
  
  output$filtered_data <- renderTable({
    data_decimal <- filtered_data()
    data_decimal$NbMedecins <- format(data_decimal$NbMedecins, big.mark = "", decimal.mark = " ")
    data_decimal$Population <- format(data_decimal$Population, big.mark = "", decimal.mark = " ")
    data_decimal
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("filtered_data", ".xlsx", sep = "") },
    content = function(file) {
      write_xlsx(filtered_data(), path = file)
    }
  )
  
  # Observer les changements dans la sélection de la région pour les graphiques
  observe({
    region <- input$selected_region_graph
    departments <- unique(df[df$Region == region, "Departement"])
    updateSelectInput(session, "selected_department_graph", choices = departments)
  })
  
  
  output$bar_plot <- renderPlotly({
    # Sélectionner la variable à analyser en fonction de l'entrée de l'utilisateur
    variable_graph <- input$selected_variable_graph
    
    # Filtrer les données en fonction de la région sélectionnée et de la population maximale si applicable
    data_graph <- df %>%
      filter(Region == input$selected_region_graph, Population <= input$pop_analyse) %>%
      group_by(Departement) %>%
      summarize(TotalValue = sum(.data[[variable_graph]], na.rm = TRUE))
    
    # Créer le graphique en fonction de la variable sélectionnée
    plot_ly(data_graph, x = ~Departement, y = ~TotalValue, type = 'bar') %>%
      layout(title = paste("Distribution de", ifelse(variable_graph == "NbMedecins", "médecins", "la population"),
                           "par département dans la région de", input$selected_region_graph),
             xaxis = list(title = "Département"),
             yaxis = list(title = ifelse(variable_graph == "NbMedecins", "Nombre de médecins", "Population")))
  })
  ########################################################################      
  # Créer le nuage de points
  output$scatter_plot <- renderPlotly({
    data_scatter <- df %>%
      filter(Region == input$selected_region_graph, Population <= input$pop_analyse)
    
    plot_ly(data_scatter, x = ~Population, y = ~NbMedecins, type = 'scatter', mode = 'markers') %>%
      layout(title = paste("Relation entre la population et le nombre de médecins dans la région de", input$selected_region_graph),
             xaxis = list(title = "Population"),
             yaxis = list(title = "Nombre de médecins"))
  })
  
  
  output$box_plot <- renderPlotly({
    data_graph <- df %>%
      filter(Region == input$selected_region_graph, Population <= input$pop_analyse) %>%
      group_by(Departement)
    
    # Sélectionner la variable pour l'axe Y
    y_var <- input$selected_variable_graph
    
    plot_ly(data_graph, x = ~Departement, y = data_graph[[y_var]], type = 'box') %>%
      layout(title = paste("Distribution de", ifelse(y_var == "NbMedecins", "nombre de médecins", "la population"),
                           "par département dans la région de", input$selected_region_graph),
             xaxis = list(title = "Département"),
             yaxis = list(title = ifelse(y_var == "NbMedecins", "Nombre de médecins", "Population")))
  })
  
  
  
  # Calculer les KPI basés sur la région sélectionnée dans le filtre
  output$total_medecins <- renderValueBox({
    total_medecins <- sum(df[df$Region == input$selected_region_graph, "NbMedecins"], na.rm = TRUE)
    valueBox(
      format(total_medecins, big.mark = ","),
      subtitle = "Nombre total de médecins",
      icon = icon("user-md"),
      color = "blue"
    )
  })
  
  output$total_population <- renderValueBox({
    total_population <- sum(df[df$Region == input$selected_region_graph, "Population"], na.rm = TRUE)
    valueBox(
      format(total_population, big.mark = ","),
      subtitle = "Population totale",
      icon = icon("users"),
      color = "green"
    )
  })
  
  
  output$map_s <- renderLeaflet({
    leaflet(data[data$Nom %in% communes_sans_medecins, ]) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude, popup = ~Nom)
  })
  
  
  output$table_communes_sans_medecin <- renderTable({
    data %>%
      filter(Nom %in% communes_sans_medecins) %>%
      select(Nom, Population) %>% 
      mutate(
        Population = round(Population, 0) 
      )
  })
  
  
  #############################
  
  # Render map
  map <- eventReactive(input$update,{
    
    url <- paste0("https://geo.api.gouv.fr/departements?nom=",URLencode(input$dept))
    response <- GET(url)
    content <- content(response, "parsed")
    
    df_cartes <- df_cartes[which(substr(df_cartes$Code,1,2)==content[[1]]$code),]
    shapes <- getCommuneShapes(content[[1]]$code)
    
    # Créer les polygones
    liste_polygones <- list()
    valeurs <- c()
    
    for(i in 1:length(shapes$features)){
      
      if(length(shapes$features[[i]]$geometry$coordinates[[1]])>4){
        x <- sapply(shapes$features[[i]]$geometry$coordinates[[1]], function(x) x[[1]])
        y <- sapply(shapes$features[[i]]$geometry$coordinates[[1]], function(x) x[[2]])
        xym <- cbind(x, y)
        p <- Polygon(xym)
        liste_polygones <- append(liste_polygones, list(Polygons(list(p), ID = shapes$features[[i]]$properties$code)))
      }
      else{
        x <- sapply(shapes$features[[i]]$geometry$coordinates[[1]][[1]], function(x) x[[1]])
        y <- sapply(shapes$features[[i]]$geometry$coordinates[[1]][[1]], function(x) x[[2]])
        xym <- cbind(x, y)
        p <- Polygon(xym)
        liste_polygones <- append(liste_polygones, list(Polygons(list(p), ID = shapes$features[[i]]$properties$code)))
      }
      
    }
    
    sps <- SpatialPolygons(liste_polygones)
    
    valeurs <- df_cartes[[input$colorBy]]
    # Créer une data.frame avec des noms de lignes correspondant aux IDs des polygones
    data <- data.frame(
      Value = valeurs,
      row.names = sapply(slot(sps, "polygons"), function(x) slot(x, "ID"))
    )
    spDf <- SpatialPolygonsDataFrame(sps, data = data)        
    # Créer les étiquettes
    labelpoly <- sprintf(
      "<center><strong>%s</strong><br>%s</center>",
      df_cartes$Nom, prettyNum(valeurs, scientific = FALSE, digits = 16, big.mark = " ")
    ) %>% lapply(htmltools::HTML)
    
    
    # Créer la palette de couleurs
    #pal <- colorNumeric(palette = "YlOrRd", domain = valeurs, na.color = "#D3D3D3")
    
    
    # Condition pour choisir la palette et la légende
    if (input$colorBy == "Population") {
      # Créer la palette de couleurs pour la population
      pal <- colorNumeric(palette = "YlOrRd", domain = valeurs, na.color = "#D3D3D3")
    } else if (input$colorBy == "NbMedecins") {
      # Créer la palette de couleurs pour les médecins
      pal <- colorNumeric(palette = "Greens", domain = valeurs, na.color = "#D3D3D3")
      
    }
    
    
    
    
    
    # Créer la carte Leaflet
    map <- leaflet() %>% 
      addTiles() %>%
      addPolygons(data = spDf, weight = 1, fillOpacity = 1, color = "black", opacity = 1, fillColor = ~pal(Value),
                  highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1, weight = 5, sendToBack = FALSE, color = "white"), label = labelpoly) %>%
      addLegend(pal = pal, values = valeurs, opacity = 1, title = input$colorBy,
                position = "bottomright", labFormat = labelFormat(digits = 0, big.mark = " "))
    map
    
  })
  output$map <- renderLeaflet({
    map()
  })
  
  output$download_html <- downloadHandler(
    filename = function() {
      "documentation.html"
    },
    content = function(file) {
      writeLines(documentation_partie_html, file)
    }
  )
  
  
}

# Lancer l'application Shiny
shinyApp(ui = ui, server)



