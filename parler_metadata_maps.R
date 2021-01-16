library(tidyverse)
library(lubridate)
library(leaflet)
library(leaftime)
library(htmltools)
library(htmlwidgets)
library(geojsonio)
# parler_dc <- read_csv("https://raw.githubusercontent.com/TheBlindEye/ParlerMetadata/main/Parler.01.06.21.csv")
parler_all <- read_csv("https://raw.githubusercontent.com/zumbov2/parler-video-metadata/main/parler_vids_geolocation_time.csv")

dt2 <- parler_all %>% 
  mutate(CreateDate = ymd_hms(CreateDate)) %>%
  mutate(start = as_date(CreateDate), end = as_date(CreateDate)) %>%
  filter(start > as_date("2020-12-01"))

#dt2 parler video data time lapse
dt2_geo <- geojsonio::geojson_json(dt2 )

# 2021-6-1 in DC
dt2_jan6 <- dt2 %>% 
  filter(date(CreateDate) == "2021-01-06") %>% 
  filter(lat > 38.6 & lat < 39.2)

# Capitol 2021-6-1 ----------------------------------------------------------------

# 2021-6-1 in DC
dt2_jan6 <- dt2 %>% 
  filter(date(CreateDate) == "2021-01-06") %>% 
  filter(lat > 38.6 & lat < 39.2)

# Leaflet
leaflet(dt2_jan6) %>% 
  setView(lng = -77.025, lat = 38.892, zoom = 12) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(
    color = "red", radius=7,
    stroke = FALSE, fillOpacity = 0.7, 
    popup = ~paste("Parler video (public) recorded on", as.character(CreateDate)),
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

power <- data.frame(
  "Latitude" = c(
    33.515556, 38.060556, 47.903056, 49.71, 49.041667, 31.934167,
    54.140586, 54.140586, 48.494444, 48.494444
  ),
  "Longitude" = c(
    129.837222, -77.789444, 7.563056, 8.415278, 9.175, -82.343889,
    13.664422, 13.664422, 17.681944, 17.681944
  ),
  "start" = seq.Date(as.Date("2015-01-01"), by = "day", length.out = 10),
  "end" = seq.Date(as.Date("2015-01-01"), by = "day", length.out = 10) + 1
)
power_geo <- geojsonio::geojson_json(power,lat="Latitude",lon="Longitude")

leaflet(power_geo) %>%
  addTiles() %>%
  setView(lng = -77.025, lat = 38.892, zoom = 5) %>%
  addTimeline(
    sliderOpts = sliderOptions(steps=10,enablePlayback = TRUE,enableKeyboardControls = FALSE,showTicks = FALSE,waitToUpdateMap = TRUE)
  )

leaflet(dt2_geo, options = leafletOptions(zoomControl = FALSE, dragging = FALSE, minZoom = 5, maxZoom = 5)) %>%
  addTiles() %>%
  setView(lng = -77.025, lat = 38.892, zoom = 5) %>%
  addTimeline(
    sliderOpts = sliderOptions(formatOutput = htmlwidgets::JS("function(CreateDate) {
                                                                        return new Date(CreateDate).toDateString()
                                                                        }"),
                               steps=10, enablePlayback = FALSE,
                               enableKeyboardControls = TRUE, 
                               showTicks = FALSE,
                               waitToUpdateMap = FALSE),
    timelineOpts = timelineOptions(
      styleOptions = styleOptions(
        color = "red",
        fillOpacity = 1,
        radius = 7))
    )



leaflet() %>%
  setView(lng = -77.025, lat = 38.892, zoom = 5) %>%
  addTiles() %>%
  addTimeline(data= dt2_geo, 
              sliderOpts = sliderOptions(formatOutput = htmlwidgets::JS("function(CreateDate) {
                                                                        return new Date(CreateDate).toDateString()
                                                                        }"),
                                         position = "bottomright",
                                         # duration = 120000,
                                         steps=50,
                                         enablePlayback = TRUE,
                                         enableKeyboardControls = TRUE,
                                         showTicks = FALSE,
                                         waitToUpdateMap = TRUE
                                         ),
              timelineOpts = timelineOptions(
                styleOptions = NULL,
                pointToLayer = htmlwidgets::JS("function(data, latlng) {
                                                        return L.circleMarker(latlng, {
                                                        radius: +data.properties.radius,
                                                        color: data.properties.color,
                                                        fillColor: data.properties.color,
                                                        fillOpacity: 1,
                                                        radius: 10});}")
              # styleOptions = styleOptions(
              #   color = "black",
              #   fillColor = "red",
              #   fillOpacity = 1,
              #   radius = 10))
              )) %>%
  addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
               onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  addEasyButton(easyButton(icon="fa-globe", title="Zoom to Level 1",
               onClick=JS("function(btn, map){ map.setZoom(1); }")))

