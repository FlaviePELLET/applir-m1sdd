library(shiny)
library(shinydashboard)
library(shinyjs)
library(writexl)
library(plotly)
library(dplyr)
library(httr)
library(leaflet)
library(jsonlite)
library(sp)
library(rsconnect)
library(readxl)
library(shinyWidgets)

documentation_partie_html <- 

       ' <!DOCTYPE html>
        <html>
        <body>
            <div class="box">
            <h1 class="section-title">Cette application a été développée par Camille ESNAULT et Flavie PELLET, étudiantes à l\'IUT de Vannes dans le cadre d\'une SAE.</h1>
                <h2 class="section-title">Présentation des données</h2>
                <p>Cette application étudie la pénurie des médecins dans les différentes communes en 2021.</p>
                <p>Les données proviennent de l\'INSEE et elles sont accessibles via le lien suivant: 
                    <a href="https://statistiques-locales.insee.fr" target="_blank">https://statistiques-locales.insee.fr</a>.
                </p>
                <p>Elles répertorient le nombre de médecins, la population par département, région, commune et code INSEE.</p>
            </div>
            <div class="box">
                <h2 class="section-title">Tableaux</h2>
                <p>Il est possible de sélectionner une région, un ou plusieurs départements, rechercher une commune pour afficher les données filtrées.</p>
                <p>Vous pouvez télécharger les données filtrées en format Excel en cliquant sur le bouton "Télécharger les données en Excel".</p>
            </div>
            <div class="box">
                <h2 class="section-title">Graphiques</h2>
                <p> Cette application offre différentes représentations graphiques : un nuage de points, une diagramme en barres et des boxplots.</p>
                <p>Le nuage de point affiche la relation entre la population et le nombre de médecins dans une région sélectionnée.</p>
                <p>Les filtres permettent d\'avoir des statistiques par régions selon deux variables : la population et le nombre de médecins. Aussi, il est possible d\'ajuster la population étudiée.</p>
                <p>Le diagramme en barres montre la distribution des médecins ou de la population par département dans une région sélectionnée.</p>
                <p>Les boxplots affichent la distribution du nombre de médecins ou de la population par département.</p>
            </div>
            <div class="box">
                <h2 class="section-title">Cartes</h2>
                <p>Cette partie est divisée en 2 cartes.</p>
                <p> Une carte affiche les communes de plus de 5 000 habitants qui n\'ont pas de médecin.</p>
                 <p>Une carte intéractive qui affiche les communes d\'un département sélectionné permettant d\'identifier les disparités sur le territoire selon la population ou le nombre de médecins.</p>
            </div>
        </body>
        </html>
    '

# Chargement des données

df <- read.csv2("data_medecins.csv")
data_Lat_Long <- read.csv2("communes_LatLong.csv")
names(df) <- c("Region", "Departement", "Code", "Nom", "Population", "NbMedecins", "Ratio")
df_cartes <- read.csv2("data_cartes.csv", skip = 2)
df_cartes$Code <- ifelse(nchar(df_cartes$Code) == 4, sprintf("%05d", as.numeric(df_cartes$Code)), df_cartes$Code)
colnames(df_cartes)[colnames(df_cartes) == "Médecin.généraliste..en.nombre..2021"] <- "NbMedecins"
colnames(df_cartes)[colnames(df_cartes) == "Population.municipale.2021"] <- "Population"

# Transformer les variables en numérique
df_cartes$Population <- as.numeric(df_cartes$Population)
df_cartes$NbMedecins <- as.numeric(df_cartes$NbMedecins)

# data_communes <- read.csv2("data_communes.csv", skip = 2) #
# data_communes$Code <- ifelse(nchar(data_communes$Code) == 4, sprintf("%05d", as.numeric(data_communes$Code)), data_communes$Code)

# Extraire les trois premiers caractères de la colonne 'Region'
df$Code_dep <- substr(df$Departement, 1, 2)
df$Region <- substring(df$Region, 6)
df$Departement <- substring(df$Departement, 6)

# Transformer les variables en numérique
df$Population <- as.numeric(df$Population)
df$NbMedecins <- as.numeric(df$NbMedecins)
df <- subset(df, select = -Code_dep)


data_Lat_Long$latitude <- as.numeric(data_Lat_Long$latitude)
data_Lat_Long$longitude <- as.numeric(data_Lat_Long$longitude)

data = merge(na.omit(df), na.omit(data_Lat_Long), by.x = "Code", by.y = "code_INSEE", all.x = TRUE)
data = unique(data)

data$Code <- ifelse(nchar(data$Code) == 4, sprintf("%05d", as.numeric(data$Code)), data$Code)


df <- subset(df, select = -Ratio)
df <- na.omit(df)
# Fusionner les données en supprimant les lignes avec des NA
#data <- merge(df, na.omit(data_Lat_Long), by.x = "Code", by.y = "code_INSEE", all.x = TRUE)

communes_sans_medecins <- unique(subset(data, NbMedecins == 0 & Population > 4000)$Nom)

get_commune_contours <- function(code) {
  url <- paste0("https://geo.api.gouv.fr/communes/", code, "?fields=nom,contour")
  response <- GET(url)
  content <- content(response, as = "parsed")
  coords_df <- data.frame(
    Longitude = sapply(content$contour$coordinates[[1]], function(x) x[[1]]),
    Latitude = sapply(content$contour$coordinates[[1]], function(x) x[[2]])
  )
  return(coords_df)
}

getCommuneShapes <- function(dept) {
  
  url <- paste0("https://geo.api.gouv.fr/departements/", dept, "/communes?geometry=contour&format=geojson&fields=nom,code")
  response <- GET(url)
  content <- content(response, "parsed")
  return(content)
}

url_dpt <- "https://geo.api.gouv.fr/departements"
response_dpt <- GET(url_dpt)
content_dpt <- content(response_dpt, "parsed")
dpts_df <- data.frame(
  Nom = sapply(content_dpt, function(x) x$nom),
  Code = sapply(content_dpt, function(x) x$code)
)


# Définir l'UI pour l'application
ui <- fluidPage(includeCSS("www/styles.css"),
  dashboardPage(
  dashboardHeader(title = "Les pénuries de médecins généralistes en 2021", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Données", tabName = "donnees", icon = icon("folder-open")),
      menuItem("Tableaux", tabName = "tab", icon = icon("th")),
      menuItem("Graphiques", tabName = "graph", icon = icon("chart-line")),
      menuItem("Carte", tabName = "cartes", icon = icon("map-location-dot"),
               menuSubItem("Pénuries des médecins", tabName = "carte_med"),
               menuSubItem("Département", tabName = "carte_dept")),
      menuItem("Documentation", tabName = "doc", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = "
      shinyjs.initAutocomplete = function(communes) {
        $('#selected_commune').autocomplete({
          source: communes
        });
      }
    ", functions = c("initAutocomplete")),
    
    tabItems(
      tabItem(tabName = "donnees",
              tags$div(
                style = "position: relative;",
                img(src = "logo_IUT.png", height = "55px", width = "70px",
                    style = "position: absolute; top: -5px; right: 0px;"),
                img(src = "logo_doctolib.svg", height = "65px", width = "70px",
                    style = "position: absolute; top: -10px; right: 70px;")),
              h2("Présentation des données"),
              box(title = "Nom du fichier : data_medecins",
                  p("Cette application étudie la pénurie des médecins dans les différentes communes en 2021."),
                  p("Les données proviennent de l'INSEE, elles sont accessibles via le lien suivant:
                  https://statistiques-locales.insee.fr"),
                  p("Elles répertorient le nombre de médécins, la population par département, région et commune et code INSEE."),
                  width = "100%",
                  solidHeader = TRUE,
                  collapsible = TRUE),
              box(title = "Résumé Statistique",
                  verbatimTextOutput("str_stats"),
                  width = "100%",
                  solidHeader = TRUE,
                  collapsible = TRUE),
      ),
      
      
      tabItem(tabName = "tab",
              tags$div(
                style = "position: relative;",
                img(src = "logo_IUT.png", height = "55px", width = "70px",
                    style = "position: absolute; top: -5px; right: 0px;"),
                img(src = "logo_doctolib.svg", height = "65px", width = "70px",
                    style = "position: absolute; top: -10px; right: 70px;")),
              h2("Tableaux"),
              fluidRow(
                box(
                  title = "Filtres",
                  width = 12, solidHeader = TRUE, collapsible = TRUE,
                  fluidRow(
                    column(4,
                           selectInput(inputId = "selected_region",
                                       label = "Sélectionnez une région",
                                       choices = unique(df$Region),
                                       selected = unique(df$Region)[1])
                    ),
                    column(4,
                           selectInput(inputId = "selected_department",
                                       label = "Sélectionnez un département",
                                       choices = NULL,
                                       multiple = TRUE)
                    ),
                    column(4,
                           textInput(inputId = "selected_commune",
                                     label = "Recherche par commune",
                                     placeholder = "Entrez une commune")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Tableau des données filtrées",
                  width = 12, solidHeader = TRUE, collapsible = TRUE,
                  div(style = "position: relative;",
                      div(style = "position: absolute; top: 1px; right: 1px;",
                          downloadButton("download_data", "Télécharger les données en Excel")
                      ),
                      div(style = "padding-top: 5px;",
                          tableOutput("filtered_data")
                      )
                  )
                )
              )
      ),
      
      tabItem(tabName = "graph",
              tags$div(
                style = "position: relative;",
                img(src = "logo_IUT.png", height = "55px", width = "70px",
                    style = "position: absolute; top: -5px; right: 0px;"),
                img(src = "logo_doctolib.svg", height = "65px", width = "70px",
                    style = "position: absolute; top: -10px; right: 70px;")),
              h2("Graphiques"),
              fluidRow(
                valueBoxOutput("total_medecins", width = 6),
                valueBoxOutput("total_population", width = 6)
              ),
              fluidRow(
                box(
                  title = "Nuage de points",
                  width = 12, solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("scatter_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Filtres",
                  width = 12, solidHeader = TRUE, collapsible = TRUE,
                  fluidRow(
                    column(4,
                           box(
                             title = "Région",
                             width = NULL, solidHeader = TRUE,
                             selectInput(inputId = "selected_region_graph",
                                         label = "Sélectionnez une région",
                                         choices = unique(df$Region),
                                         selected = unique(df$Region)[1])
                           )
                    ),
                    column(4,
                           box(
                             title = "Variable",
                             width = NULL, solidHeader = TRUE,
                             selectInput(inputId = "selected_variable_graph",
                                         label = "Sélectionnez une variable d'analyse",
                                         choices = c("Nombre de médecins" = "NbMedecins", "Population"),
                                         selected = "NbMedecins")
                           )
                    ),
                    column(4,
                           box(
                             title = "Population",
                             width = NULL, solidHeader = TRUE,
                             sliderInput("pop_analyse",
                                         "Effectif de la population :",
                                         min = 0,
                                         max = max(df$Population),
                                         value = max(df$Population))
                           )
                    )
                  )
                )
              )
              ,
              
              fluidRow(
                box(
                  title = "Diagramme en barres",
                  width = 12, solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("bar_plot")
                )
              ),
              
              fluidRow(
                box(
                  title = "Distribution",
                  width = 12, solidHeader = TRUE, collapsible = TRUE,
                  plotlyOutput("box_plot")
                )
              )
      ),
      
      
      tabItem(tabName = "carte_med",
              tags$div(
                style = "position: relative;",
                img(src = "logo_IUT.png", height = "55px", width = "70px",
                    style = "position: absolute; top: 10px; right: 15px;"),
                img(src = "logo_doctolib.svg", height = "65px", width = "70px",
                    style = "position: absolute; top: 5px; right: 80px;")),
              fluidPage(
                titlePanel("Communes de plus de 5 000 habitants sans médecin"),
                leafletOutput("map_s"),
                tableOutput("table_communes_sans_medecin")
              )),
      
      
      
      tabItem(tabName = "carte_dept",
              tags$div(
                style = "position: relative;",
                img(src = "logo_IUT.png", height = "55px", width = "70px",
                    style = "position: absolute; top: 10px; right: 15px;"),
                img(src = "logo_doctolib.svg", height = "65px", width = "70px",
                    style = "position: absolute; top: 5px; right: 80px;")),
              fluidPage(
                titlePanel("Carte des Communes par Département"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("dept", "Choisissez un département :", choices = dpts_df$Nom),
                    selectInput("colorBy", "Colorer par :", choices = c("Population" = "Population", "Nombre de médécins" = "NbMedecins")),
                    actionButton("update", "Valider")
                  ),
                  mainPanel(
                    leafletOutput("map")
                  )
                )
                
              )),
      
      
      
      tabItem(tabName = "doc",
              box(
                titlePanel("Documentation de l'application"),
                width = 30, solidHeader = TRUE, collapsible = TRUE,
                HTML(documentation_partie_html),
                div(style = "position: relative;",
                    div(style = "position: absolute; top: 1px; right: 1px;",
                        downloadButton("download_html", "Télécharger la documentation en HTML")
                    ))
              ))
      
      
      
      
      
      
      
    )
  ))
  )


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



