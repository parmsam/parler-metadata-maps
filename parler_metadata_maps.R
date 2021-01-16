library(tidyverse)
library(lubridate)
library(leaflet)
library(leafgl)
library(htmltools)
library(htmlwidgets)
library(sf)
library(geojsonio)
library(leaftime)

#parler public video metadata
parler_all <- read_csv("https://raw.githubusercontent.com/zumbov2/parler-video-metadata/main/parler_vids_geolocation_time.csv")

dt2 <- parler_all %>% 
  mutate(CreateDate = ymd_hms(CreateDate)) %>%
  mutate(start = as_date(CreateDate), end = as_date(CreateDate)+1) %>%
  filter(start > as_date("2020-11-01"))

dt2_geo <- geojsonio::geojson_json(dt2 )

# 2021-01-06 in DC
dt2_jan6 <- dt2 %>% 
  filter(date(CreateDate) == "2021-01-06") %>% 
  filter(lat > 38.6 & lat < 39.2)
tag.map.title_white  <- tags$style(HTML("
  .leaflet-control.map-title-white { 
    position: sticky !important;
    left: 25%;
    right: 50%;
    text-align: center;
    font-weight: bold;
    font-size: 18px;
    color: WhiteSmoke;
  }
"))
title <- tags$div(
  tag.map.title_white, HTML("Parler Metadata on Jan 6, 2020")
)  
tag.map.title_black  <- tags$style(HTML("
  .leaflet-control.map-title-black { 
    position: sticky !important;
    left: 25%;
    right: 50%;
    text-align: center;
    font-weight: bold;
    font-size: 18px;
    color: black;
  }
"))
title2 <- tags$div(
  tag.map.title_black, HTML("Parler Metadata over Time")
)  

# Leaflet
leaflet(dt2_jan6) %>% 
  addControl(title, position = "topright", className="map-title-white") %>%
  setView(lng = -77.025, lat = 38.892, zoom = 12) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(
    color = "red", radius=7,
    stroke = FALSE, fillOpacity = 0.7, 
    popup = ~paste(as.character(CreateDate),"at (", as.character(lat), as.character(lon),")"),
    labelOptions = labelOptions(style = list("font-size" = "8px")), 
    clusterOptions = markerClusterOptions(), clusterId = "quakesCluster"
  ) %>% 
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE) %>%
  addEasyButton(easyButton(
    states = list(
      easyButtonState(
        stateName="unfrozen-markers",
        icon="ion-toggle",
        title="Freeze Clusters",
        onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
      ),
      easyButtonState(
        stateName="frozen-markers",
        icon="ion-toggle-filled",
        title="UnFreeze Clusters",
        onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")))))

leaflet(dt2_geo, options = leafletOptions(zoomControl = TRUE, 
                                          dragging = TRUE, 
                                          preferCanvas = FALSE)) %>%
  addTiles() %>%
  addControl(title2, position = "topright", className="map-title-black") %>%
  setView(lng = -77.025, lat = 38.892, zoom = 6) %>%
  addTimeline(
    width = "40%",
    sliderOpts = sliderOptions(
      formatOutput = htmlwidgets::JS("function(StartDate) {return new Date(StartDate).toDateString()}"),
      steps=length(unique(dt2$start)),
      position = "bottomright",
      duration = 150000,
      showTicks = TRUE,
      enablePlayback = TRUE,
      enableKeyboardControls = TRUE, 
      waitToUpdateMap = FALSE),
    timelineOpts = timelineOptions(
      styleOptions = styleOptions(
        color = "red",
        fillOpacity = 0.5,
        radius = 2))
  ) %>%
  addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
                           onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  addEasyButton(easyButton(icon="fa-globe", title="Zoom to Level 1",
                           onClick=JS("function(btn, map){ map.setZoom(1); }")))