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
# Lancer l'application Shiny
shinyApp(ui, server = server)
