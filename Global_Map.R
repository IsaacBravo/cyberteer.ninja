setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

Packages <- c("leaflet", "readxl", "sp",
              "networkD3")

lapply(Packages, library, character.only = TRUE)

worldcities <- read_excel("worldcities.xlsx")
View(worldcities)

worldcities$lng <- as.numeric(worldcities$lng)
worldcities$lat <- as.numeric(worldcities$lat)

awesome <- makeAwesomeIcon(
  icon='glyphicon-screenshot', 
  library='glyphicon', 
  markerColor = 'darkred', 
  iconColor = 'white'
)

map<- leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 2,
                                       dragging = TRUE)) %>%
  addTiles() %>%
  addProviderTiles(providers$Stamen.Toner)%>%
  #addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  #addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>% 
  addAwesomeMarkers(data = worldcities, lng = ~lng, 
                    lat = ~lat, 
                    popup = ~paste("<h3>Cybersecurity Capacity</h3>",
                                   "<b>Cyber Troops Status: </b> ",status,"<br>",
                                   "<b>Cyber Troops Capacity*: </b> ",capacity,"<br>",
                                   "<b>Rank Global Cybersecurity 2018: </b> ",rank2018, "<br>", 
                                   "<b>Rank Global Cybersecurity 2017: </b> ",rank2017,"<br>",
                                   "<b></b> ","<br>",
                                   "<b>Note(*): </b> ",definition,"<br>",
                                    sep = " "),
                    icon = awesome,
                    clusterOptions = markerClusterOptions()) %>% 
  addMiniMap(toggleDisplay = TRUE,
             tiles = providers$Stamen.TonerLite,
             aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = FALSE),
             shadowRectOptions = list(color = "#000000", weight = 1, clickable = FALSE, opacity =
                                        0, fillOpacity = 0),
             strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
             mapOptions = list())

saveNetwork(map,"C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/TUMProject/Map/Global_Map.html", selfcontained = TRUE)
