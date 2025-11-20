# Benodigde packages
library(sf)
library(httr)
library(tidyverse)

# functies ----------------------------------------------------------------

# PDOK kan tot 1000 features per request ophalen. De oplossing is om paginering te gebruiken (startIndex + count)
# functie om aantal postcode paginas op te halen:
get_postcode_page <- function(startIndex = 0, count = 1000, bbox = NULL) {
  base_url <- "https://service.pdok.nl/cbs/postcode6/2023/wfs/v1_0"
  url <- httr::parse_url(base_url)
  url$query <- list(
    service = "WFS",
    version = "2.0.0",
    request = "GetFeature",
    typeNames = "postcode6:postcode6",
    outputFormat = "application/json",
    count = count,
    startIndex = startIndex
  )
  if (!is.null(bbox)) {
    url$query$bbox <- paste0(
      bbox["xmin"], ",", bbox["ymin"], ",",
      bbox["xmax"], ",", bbox["ymax"], ",EPSG:28992"
    )
  }
  request_url <- httr::build_url(url)
  st_read(request_url, quiet = TRUE)
}

# Functie om alle postcodes binnen een bounding box te krijgen door ophalen per pagina en die samen te voegen.
get_all_postcodes <- function(bbox = NULL, batch_size = 1000) {
  all_data <- list()
  start <- 0
  repeat {
    page <- get_postcode_page(startIndex = start, count = batch_size, bbox = bbox)
    if (nrow(page) == 0) break
    all_data[[length(all_data) + 1]] <- page
    cat("Opgehaald:", nrow(page), "features vanaf index", start, "\n")
    start <- start + batch_size
  }
  do.call(rbind, all_data)
}

# Functie om postcodes op te halen binnen shapefile met behulp van bovenstaande 2 functies
# functie maakt gebruikt van Postcode6 informatie van PDOK
# https://www.nationaalgeoregister.nl/geonetwork/srv/dut/catalog.search#/metadata/e9841ae6-8f1d-460f-9506-a6b5ed2dc15f/formatters/xsl-view?root=div&view=advanced

get_postcodes_via_wfs <- function(shapefile) {
  # transformeer je shapefile
  jouw_gebied <- st_transform(shapefile, crs = 28992)

  # Basis WFS URL met werkende parameters
  base_url <- "https://service.pdok.nl/cbs/postcode6/2023/wfs/v1_0"

  # Krijg bounding box voor filter
  bbox <- st_bbox(jouw_gebied)
  print("bbox is:")
  print(bbox)

  # Bouw correcte WFS request URL
  url <- parse_url(base_url)
  url$query <- list(
    request = "GetFeature",
    service = "WFS",
    version = "1.1.0",
    outputFormat = "application/json; subtype=geojson",
    typeName = "postcode6:postcode6",
    features = 50, # Zeer hoog getal om limiet te omzeilen
    # Voeg bbox filter toe in correcte format
    bbox = paste0(
      bbox["xmin"], ",", bbox["ymin"], ",",
      bbox["xmax"], ",", bbox["ymax"], ",EPSG:28992"
    )
  )

  request_url <- build_url(url)

  cat("Postcodedata ophalen...\n")
  cat("URL:", request_url, "\n\n")

  # Lees postcodedata
  tryCatch(
    {
      postcode_data <- get_all_postcodes(bbox = bbox, batch_size = 1000)
      cat("Postcodedata succesvol opgehaald met bbox:", nrow(postcode_data), "postcodes\n")
    },
    error = function(e) {
      cat("Fout bij ophalen met bbox, proberen zonder bbox filter...\n")

      # Probeer zonder bbox filter
      url$query$bbox <- NULL
      request_url_simple <- build_url(url)
      postcode_data <<- st_read(request_url_simple)
      cat("Alle postcodedata opgehaald:", nrow(postcode_data), "postcodes\n")
    }
  )

  # Transformeer naar juiste CRS voor de zekerheid
  postcode_data <- st_transform(postcode_data, crs = 28992)

  # Vind kruisingen met jouw gebied
  cat("OVerlappende postcodes zoeken met shapefile...\n")
  overlappende_postcodes <- st_intersection(postcode_data, jouw_gebied)

  # Haal postcodes op
  postcodes <- unique(overlappende_postcodes$postcode6)

  cat("Aantal gevonden postcodes binnen gebied:", length(postcodes), "\n")

  return(list(
    postcodes = postcodes,
    data = overlappende_postcodes
  ))
}


# inladen RWZI zuiveringskring shapefile ----------------------------------

# Indienje een shapefile nodig hebt van een zuiveringskring van een RWZI in Nederland
# kan beginnen met de zuiveringseenheden van Waterschappen Waterketen GWSW (https://api.pdok.nl/rioned/waterschappen-waterketen-gwsw/ogc/v1/collections/waterschap_zuiveringseenheid)
# indien je de methodiek voor iets anders wilt gebruiken dan moet je eigen shapefile hier gebruiken.

# laad zuiveringskring geojson van url PDOK
rwzi_zuiveringskring <- st_read("https://api.pdok.nl/rioned/waterschappen-waterketen-gwsw/ogc/v1/collections/waterschap_zuiveringseenheid/items?crs=http%3A%2F%2Fwww.opengis.net%2Fdef%2Fcrs%2FOGC%2F1.3%2FCRS84&f=json&limit=1000")
rwzi_zuiveringskring <- st_cast(st_zm(rwzi_zuiveringskring), "MULTIPOLYGON") # 2d kaart maken.
rwzi_zuiveringskring <- st_transform(rwzi_zuiveringskring, crs = 28992) # omzetten naar RD coordinaten
# mapview::mapview(rwzi_zuiveringskring) # voorbeeld van RWZI's waaruit je kunt kiezen.

# filter voor RWZI('s) naar keuze in Nederland, zie kaart hierboven voor de namen
RWZI_naam <- c("Dinther", "Bath", "Heugem") # mag ook 1 of meerdere zijn
# filteren
for (rwzis in RWZI_naam) {
  rwzi_shp <- rwzi_zuiveringskring |>
    mutate(rwzi_cap = toupper(naam)) |>
    filter(str_detect(rwzi_cap, toupper(rwzis)))
  # mapview::mapview(rwzi_shp) # check of selectie is wat je wilt
  
  # opslaan shapefile naar folder data/Emissieregistratie
  output_folder <- "data/02_modified/rwzi_shapes"
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  # zorg voor geldig bestandsnaam
  shapefile_naam <- paste0(output_folder, "/",rwzis, "_RWZI.shp")
                           
  # verwijder eerst oude shapefile-bestanden (alle extensies)
  base <- tools::file_path_sans_ext(shapefile_naam)
  extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg")
  to_delete <- paste0(base, extensions)
  file.remove(to_delete[file.exists(to_delete)])
  
  # schrijf shapefile
  st_write(rwzi_shp, paste0(shapefile_naam), delete = T, append = F)
}


# Haal vervolgens postcodes op die binnen shapefile (zuiveringskring) vallen ---------------------
postcodes_voor_ER_filter <- list()  # initialiseren

for (rwzis in RWZI_naam) {
  print(glue::glue("----------{rwzis}---------------"))
  shapefile_naam <- paste0(output_folder, "/", rwzis, "_RWZI.shp")
  RWZI_zuiveringskring <- st_read(shapefile_naam, quiet = F) # shapefile inladen
  
  # ophalen postcodes m.b.v. CBS Postcode6 API
  postcodes_cbs <- get_postcodes_via_wfs(RWZI_zuiveringskring)

  # sla zowel postcodes als data op per RWZI
  postcodes_voor_ER_filter[[rwzis]] <- list(
    postcodes = postcodes_cbs$postcodes,
    data      = postcodes_cbs$data
  )
}

# check of postcodes matchen met je shapefile
for (rwzis in RWZI_naam) {
  shapefile_naam <- paste0(output_folder, "/", rwzis, "_RWZI.shp")
  RWZI_zuiveringskring <- st_read(shapefile_naam, quiet = TRUE) # shapefile
  
  # postcodes uit je lijst
  postcode_check <- postcodes_voor_ER_filter[[rwzis]][["data"]] |> 
    select(postcode6) |> 
    mutate(pc4 = substr(postcode6, 1, 4))   # eerste 4 cijfers
  postcode_totaal <- length(postcodes_voor_ER_filter[[rwzis]][["postcodes"]]) 
    
  # plot
  p <- ggplot() +
    geom_sf(data = postcode_check, aes(fill = as.numeric(pc4)), color = "lightgrey") +
    geom_sf(data = RWZI_zuiveringskring, fill = NA, color = "red", size = 1.2) +
    scale_fill_viridis_c(name = "Postcode (4 cijfers)", option = "plasma") +
    labs(
      title = paste("RWZI postcode check voor ophalen ER gegevens:", rwzis),
      subtitle = paste("Totaal aantal postcodes binnen zuiveringskring:", postcode_totaal)
    ) +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(hjust = 0, face = "bold")
    )
  print(glue::glue("Opslaan van kaart van zuiveringsgebied {rwzis} met postcodes waarmee bedrijven uit ER kunnen worden opgehaald.."))
  ggsave(glue::glue("data/02_modified/rwzi_shapes/{rwzis}_postcodesER.png"), p)
  print(p)
}

# Haal bedrijfsinformatie op van Emissieregistratiewebsite  ---------------

# meest recente jaar waar Emissieregistratie (ER) data van heeft.
emissiejaar_recent <- year(Sys.time()) - 2  # emissieregistratie loopt 2 jaar achter op huidige jaar.

# je kan de volgende stappen zelf uit voeren door naar ER export site te gaan
# https://data.emissieregistratie.nl/export 
# dan de volgende parameters te selecteren
# - compartiment = Emissie op riool
# - stof = Alle stoffen
# - gebiedsindeling = Nederland
# - bronniveau = Bedrijf
# - jaren = emissiejaar_recent
# --> druk vervolgens op Exporteer (ordening = Onder elkaar)

# sla het exportbestand op in subfolder data/Emissieregistratie
# huidige script werkt met ER-data uit 2023.

# vind meest recente ER export file in folder Emissieregistratie
ER_files <- list.files("data/01_raw/Emissieregistratie", pattern = "^ER_DataExport.*\\.xlsx$", full.names = TRUE)
ER_dates <- as.POSIXct(str_extract(basename(ER_files), "\\d{4}-\\d{2}-\\d{2}-\\d{6}"), format = "%Y-%m-%d-%H%M%S")

# bestand met de meest recente export datum selecteren
meest_recent <- ER_files[which.max(ER_dates)]
print(glue::glue("ophalen van ER data uit bestand {meest_recent}"))
ER_data_all <- readxl::read_xlsx(ER_files[which.max(ER_dates)],
                             sheet = "Emissies")
# filter ER data op basis van postcodes

output_folder <- "data/02_modified/Emissieregistratie"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

for (rwzis in RWZI_naam) {
  postcodes_rwzi <- postcodes_voor_ER_filter[[rwzis]][["postcodes"]]
  ER_data_rwzi <- ER_data_all |> 
  filter(Postcode %in% postcodes_rwzi) |> 
  mutate(RWZI_NAAM = rwzis)
  print(glue::glue("ER-Emissies van {nrow(ER_data_rwzi |> select(Nic, Bedrijf) |> distinct())} bedrijven gevonden voor op basis van {length(postcodes_voor_ER_filter[[rwzis]][['postcodes']])} postcodes binnen zuiveringskring van {rwzis}"))
  print(glue::glue('Wegschrijven van gefilterde ER bedrijfsdata voor RWZI {rwzis} naar {output_folder}/ER_data_{rwzis}.xlsx'))
  writexl::write_xlsx(ER_data_rwzi, glue::glue("{output_folder}/ER_data_{rwzis}.xlsx"))
}







