install.packages("flexdashboard")
install.packages("leaflet")
install.packages("leafem")
install.packages("tidyver")

library(stringi)
library(dplyr) # transformación de datos
library(sf) # manejo de datos vectoriales
library(terra) # manejo de datos raster
library(DT) # tablas interactivas
library(leaflet) # mapas interactivos
library(leaflet.extras) # funciones adicionales de leaflet
library(leafem)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(flexdashboard)



install.packages("spData")
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")


delitos_cr <- read_xls(path = "C:/Users/Dennis/Desktop/Rtudio/estadisticaspoliciales2021.xls")

delitos_cr %>%
  select(Delito, 
         Fecha, 
         Victima,
         Edad,
         Genero,
         Provincia,
         Canton,
         Hora) %>%
  
  datatable(
    colnames = c(
      "Delito", 
      "Fecha", 
      "Víctima",
      "Edad",
      "Género",
      "Provincia",
      "Cantón",
      "Hora"),
    options = list(
      pageLength = 10,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
    )) 

cantones <-
  st_read(dsn = "cantones_simplificados.geojson", quiet = TRUE) %>%
  st_transform(4326)

cantones <-
  cantones %>%
  st_transform(5367) %>%
  st_simplify(dTolerance = 100) %>% # simplificación de geometrías
  st_transform(4326)

# Lectura


cantones <-
  cantones %>%
  mutate(canton_normalizado = tolower(stri_trans_general(canton, id = "Latin-ASCII")))

# En el data frame de delitos
delitos_cr <-
  delitos_cr %>%
  mutate(canton_normalizado = tolower(stri_trans_general(Canton, id = "Latin-ASCII")))

# Unión del código de canton a delitos
delitos_cr <-
  delitos_cr %>%
  left_join(
    dplyr::select(
      st_drop_geometry(cantones),
      cod_canton,
      canton_normalizado
    ),
    by = "canton_normalizado",
    copy = FALSE,
    keep = FALSE
  )

# Conteo de registros por código de provincia
delitos_x_canton <-
  delitos_cr %>%
  count(cod_canton, name = "delitos")

# Unión de cantidad de delitos por provincia a provincias
cantones_delitos <-
  cantones %>%
  left_join(
    delitos_x_canton,
    by = "cod_canton",
    copy = FALSE,
    keep = FALSE
  )

colores_cantones_delitos <-
  colorNumeric(palette = "Reds",
               domain = cantones_delitos$delitos,
               na.color = "transparent")


# Mapa leaflet de delitos en provincias
leaflet() %>%
  setView(# centro y nivel inicial de acercamiento
    lng = -84.19452,
    lat = 9.572735,
    zoom = 7) %>%
  addTiles(group = "OpenStreetMap") %>% # capa base
  addPolygons(
    # capa de polígonos
    data = cantones_delitos,
    fillColor = ~ colores_cantones_delitos(cantones_delitos$delitos),
    fillOpacity = 0.8,
    color = "black",
    stroke = TRUE,
    weight = 1.0,
    popup = paste(
      # ventana emergente
      paste(
        "<strong>Canton:</strong>",
        cantones_delitos$canton
      ),
      paste(
        "<strong>Delitos:</strong>",
        cantones_delitos$delitos
      ),
      sep = '<br/>'
    ),
    group = "Delitos en cantones"
  ) %>%
  addLayersControl(
    # control de capas
    baseGroups = c("OpenStreetMap"),
    overlayGroups = c("Delitos en cantones")
  ) %>%
  addLegend(
    # leyenda
    position = "bottomleft",
    pal = colores_cantones_delitos,
    values = cantones_delitos$delitos,
    group = "Delitos",
    title = "Cantidad de delitos"
  )


ggplot2_delitos_CR <- 
  delitos_cr %>%
  count(Delito) %>%
  ggplot(aes(x = reorder(Delito, n ), y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Cantidad de delitos por delito cometidos en CR") +
  xlab("Delitos cometidos") +
  ylab("Cantidad de delitos cometidos") +
  coord_flip() +
  theme_minimal()


ggplot2_victima_CR <- 
  delitos_cr %>%
  count(Victima) %>%
  ggplot(aes(x = reorder(Victima, n ), y = n)) +
  geom_bar(stat = "identity") +
  ggtitle("Cantidad de delitos por delito cometidos en CR") +
  xlab("Delitos cometidos") +
  ylab("Cantidad de delitos cometidos") +
  coord_flip() +
  theme_minimal()

ggplotly(ggplot2_victima_CR) %>% config(locale = 'es')