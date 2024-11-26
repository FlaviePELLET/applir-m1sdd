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

df <- read.csv2("data/data_medecins.csv")
data_Lat_Long <- read.csv2("data/communes_LatLong.csv")
names(df) <- c("Region", "Departement", "Code", "Nom", "Population", "NbMedecins", "Ratio")
df_cartes <- read.csv2("data/data_cartes.csv", skip = 2)
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
