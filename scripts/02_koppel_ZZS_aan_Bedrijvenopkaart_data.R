# Titel:   Bedrijvenopdekaart data van een RWZI koppelen aan ZZS stoffen via SBI_ZZS_koppeltabel en plot data
# Datum:   2024-06-26

# Doel:    
# Script om de opgevraagde bedrijvenlijst van een bepaald gebied via Bedrijvenopdekaart.nl te koppelen aan
# aan ZZS stoffen die mogelijk worden gebruikt door deze bedrijven op basis van hun bedrijfsactivteit (Standaardbedrijfsindeling -SBI)

# Benodigde invoergegevens voor dit script:
#  - exceloutput van BedrijvenopKaart met daarin de bedrijfsinformatie (Naam, SBI en adresgegevens) - zie data/01_raw/Bedrijvenopkaart/BOK_dummy.xlsx
#  - SBI_ZZS koppeltabel die wordt gegenereerd in het script "01_maak_SBI_ZZS_koppeltabel". (data/02_modified/ZZS_navigator/ZZS_SBI_totaal.rds)
#  - selectie SBI lijst: ijst met SBI codes waarvan emissies mogelijk worden verwacht op basis van expert judgement. Zie data/01_raw/selectie_SBI_v2.xlsx

# Eindproduct
# Zie output/bedrijvenopkaart: 
# - bevat excels per RWZI met daarin verwachte en mogelijke verwachte stoffen voor de bedrijven geselecteerd via Bedrijvenopkaart.nl
# - bevat figuren met daarin het aantal bedrijven  per top 20 hoofdindelingen van SBI's binnen het geselecteerde gebied en de geselecteerde SBIs.
# - bevat shapefiles met 10, 25, 50, 75 &  90% contouren van de hittekaart

# Inladen van benodigde pakketten
library(tidyverse)
source("scripts/helper_functions/distinct_palette.R")  # functie overgenomen van MicroViz package(Barnett et al.,2021) om onderscheidende kleuren te creeeren voor de figuren 
library(tidygeocoder)  # voor geocoding
library(sf)  # voor shapefiles
library(mapview)  # voor visualisatie hittemap
library(eks)  # voor KDE berekening
library(maptiles)
library(sf)
library(ggspatial)
library(viridis)

# Gebruikersinvoer ----
locations <- tibble(
  rwzi_name = c("DummyRWZI1",  # naam van RWZI
                "DummyRWZI2"), # naam van tweede RWZI  (kan ook worden verwijderd als er maar 1 bestand is.)
  bok_file = c("dummy_bedrijvenopdekaart_locatie1.xlsx",  # naam van bedrijven op kaart bestand van eerste RWZI
               "dummy_bedrijvenopdekaart_locatie2.xlsx")  # naam van bedrijven op kaart bestand van tweede RWZI, (kan ook worden verwijderd als er maar 1 bestand is.)
)  # er kunnen ook nog meer RWZIs worden toegevoegd door de zowel de RWZI naam als de bestandsnaam toe te voegen aan de tabel.

SBI_LIST_FILE <- "selectie_SBI_v2.xlsx"  # lijst met SBI codes waarvan emissies mogelijk worden verwacht. Zie 


# rwzi_name <- "DummyRWZI1"
# bok_file <- "dummy_bedrijvenopdekaart_locatie1.xlsx"
# bok_location = "data/Bedrijvenopkaart"

# Verwerkingsfunctie ----
process_location <- function(rwzi_name, bok_file, bok_location = "data/01_raw/Bedrijvenopkaart") {
  bok_data <- readxl::read_xlsx(file.path(bok_location, bok_file)) %>%
    mutate(
      Sector = str_replace_all(Sector, " \\| ", "@"),
      Sbicodes = str_replace_all(Sbicodes, ", ", "@")
    ) %>%
    separate_longer_delim(c(Sbicodes, Sector), delim = "@") %>%
    count(Sbicodes, name = "n")
  
  join_df <- bok_data %>%
    left_join(sbi_lijst, by = join_by(Sbicodes == sbi_bok)) %>%
    filter(!is.na(PROCES_OMSCHRIJVING))
  
  SBI_niet_mee <- bok_data %>%
    anti_join(sbi_lijst, by = join_by(Sbicodes == sbi_bok))
  
  cat(glue::glue(
    "\nRWZI: {rwzi_name}\n",
    "Unieke SBI codes binnen selectie: {length(unique(join_df$Sbicodes))}\n",
    "Niet gekoppelde SBI codes wegens selctie: {length(unique(SBI_niet_mee$Sbicodes))} ({sum(SBI_niet_mee$n)} bedrijven)\n"
  ))
  print(unique(SBI_niet_mee$Sbicodes))
  
  eind_product <- join_df %>%
    select(Sbicodes, n_bedrijven = n) %>%
    left_join(ZZS_SBI_totaal, by = join_by(Sbicodes == SBI_bedrijvenopkaart)) %>%
    filter(!is.na(SBI)) %>%
    ungroup() %>%
    select(Sbicodes, n_bedrijven, Stofnaam:`T-score Explanation`) %>%
    distinct() %>%
    group_by(
      Stofnaam, `CAS-nummer`, `NL naam`, `UK naam`, `EC nr`, `smiles code`,
      Emissiestatus, Compartiment, PMT_name, `PMT-score`, `PMT-score Classification`, `PMT-score Explanation`,
      `P-score`, `P-score Classification`, `P-score Explanation`,
      `M-score`, `M-score Classification`, `M-score Explanation`,
      `T-score`, `T-score Classification`, `T-score Explanation`
    ) %>%
    summarize(
      totaal_bedrijven = sum(n_bedrijven),
      unique_sbicodes = n_distinct(Sbicodes),
      .groups = "drop"
    )
  
  eind_lijst <- eind_product %>%
    filter(Emissiestatus %in% c("Emissie verwacht", "Emissie mogelijk")) %>%
    select(Stofnaam:`T-score Explanation`, totaal_bedrijven, unique_sbicodes) %>%
    distinct() %>%
    arrange(desc(totaal_bedrijven), desc(unique_sbicodes), Stofnaam) |> 
    pivot_wider(
      names_from = c(Emissiestatus),
      values_from = c(totaal_bedrijven, unique_sbicodes)
    ) |> 
    mutate(Emissie_verwacht = if_else(is.na(`totaal_bedrijven_Emissie verwacht`), NA, 1),
           Emissie_mogelijk = if_else(is.na(`totaal_bedrijven_Emissie mogelijk`), NA, 1))
  
  
  # Zorg dat de outputmap bestaat
  output_folder <- "data/03_output/Bedrijvenopkaart"
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  print(glue::glue("Het bestand {rwzi_name}_ZZS.xls wordt opgeslagen in de folder output/bedrijvenopkaart."))
  writexl::write_xlsx(eind_lijst, glue::glue("{output_folder}/{rwzi_name}_ZZS.xlsx"))
  print(glue::glue("Done
                   -------"))
  return(eind_lijst)
}

process_location_for_plot <- function(rwzi_name, bok_file, bok_location = "data/01_raw/Bedrijvenopkaart") {
  bok_data <- readxl::read_xlsx(file.path(bok_location, bok_file)) %>% 
    mutate(
      Sector = str_replace_all(Sector, " \\| ", "@"),
      Sbicodes = str_replace_all(Sbicodes, ", ", "@")
    ) %>% 
    separate_longer_delim(c(Sbicodes, Sector), delim = "@") %>% 
    count(Sbicodes, name = "n")
  
  join_df <- bok_data %>% 
    left_join(sbi_lijst, by = join_by(Sbicodes == sbi_bok)) %>% 
    filter(!is.na(PROCES_OMSCHRIJVING))
  
  join_df %>% 
    group_by(`naam hoodindeling`) %>% 
    summarize(total = sum(n), .groups = "drop") %>% 
    mutate(location = rwzi_name)
}

process_location_for_heatmap <- function(rwzi_name, bok_file, bok_location = "data/01_raw/Bedrijvenopkaart") {
  bok_data <- readxl::read_xlsx(file.path(bok_location, bok_file)) %>% 
    mutate(
      rwzi = rwzi_name,
      Sector = str_replace_all(Sector, " \\| ", "@"),
      Sbicodes = str_replace_all(Sbicodes, ", ", "@")
    ) %>% 
    separate_longer_delim(c(Sbicodes, Sector), delim = "@")
  
  # prepareer adresgegevens voor geocoding
  join_df <- bok_data %>% 
    left_join(sbi_lijst, by = join_by(Sbicodes == sbi_bok)) %>% 
    filter(!is.na(PROCES_OMSCHRIJVING)) |> 
    mutate(address = paste0(Adres, ", ", Postcode, ", ", Plaatsnaam, ", Netherlands"))
}

# plot functie voor top 20 SBI binnen bedrijvenopkaart op basis van SBI selectie
create_plot <- function(location_name) {
  plot_data <- all_data %>% 
    filter(location == location_name) %>% 
    slice_max(total, n = 20) %>% 
    mutate(`naam hoodindeling wrapped` = str_replace(`naam hoodindeling`, "(.{30}\\S*)\\s", "\\1\n"))
  
  ggplot(plot_data, aes(x = `naam hoodindeling`, y = total, fill = `naam hoodindeling wrapped`)) +
    geom_col() +
    geom_text(
      aes(label = total), 
      vjust = 1.1, 
      color = "white", 
      fontface = "bold",
      size = 2.9
    ) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    scale_fill_manual(
      "sbi", 
      values = color_palette,
      breaks = all_categories_wrapped  # zorgt ervoor dat bij meerdere plots alle categorieen dezelfde kleuren hebben
    ) +
    guides(fill = guide_legend(title = "Hoofdindeling SBI's", ncol = 1)) +
    labs(
      x = "", 
      y = "Totaal aantal bedrijven",
      title = paste("Hoofdindeling SBI in", location_name, "via bedrijvenopkaart")
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 15),
      legend.key.size = unit(0.75, 'lines'),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 17)
    )
}

# hittemap plot:



# Functie om een kaart te maken per rwzi


make_map <- function(contour, rwzi_name) {
  # Zorg dat contour een sf-object is met CRS
  contour <- st_transform(contour, 3857)  # Web Mercator voor tiles
  
  # Haal OpenStreetMap tiles op
  basemap <- get_tiles(contour, provider = "OpenStreetMap", crop = TRUE, zoom = 12)
  
  # Bepaal de bounding box van de basemap
  bbox <- st_bbox(basemap)
  
  # Plot
  ggplot() +
    layer_spatial(basemap) +
    geom_sf(data = contour, aes(fill = factor(contperc)), alpha = 0.6, color = "black") +
    scale_fill_viridis_d(name = str_wrap("Relevante bedrijven (% van totaal)", 20)) +
    labs(title = paste("Hittekaart voor", rwzi_name)) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "right"
    ) +
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"]),
      expand = FALSE
    ) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tl", which_north = "true")
}




# Data verwerking ---------------------------------------------------------

# Data import ----
sbi_lijst <- readxl::read_xlsx(file.path("data/01_raw/koppeltabellen/", SBI_LIST_FILE)) %>%
  mutate(sbi_bok = str_replace_all(`SBI decimaal`, "\\.", ""))

ZZS_SBI_totaal <- readRDS("data/02_modified/ZZS_navigator/ZZS_SBI_totaal.rds")

# Verwerk alle locaties en sla deze op als xlsx in"output/bedrijvenopkaart" ----
resultaten <- map2(locations$rwzi_name, locations$bok_file, process_location)

# Toon aantal stoffen in eindlijst
names(resultaten) <- locations$rwzi_name  # Namen toewijzen aan resultatenlijst

# Print aantal rijen per RWZI
walk2(resultaten, names(resultaten), ~{
  print(glue::glue("RWZI: {.y} bevat {.x |> nrow()} stoffen in de eindlijst.\n"))
})


# Figuren sbi codes bedrijven op kaart ---------------------------------------

all_data <- map2_dfr(locations$rwzi_name, locations$bok_file, process_location_for_plot)
all_categories <- unique(all_data$`naam hoodindeling`)
# splits namen van SBI codes (>30 characters) op via een enter
all_categories_wrapped <- str_replace(all_categories, "(.{30}\\S*)\\s", "\\1\n")

color_palette <- setNames(
  distinct_palette(n = length(all_categories), add = NA),
  all_categories_wrapped
)

# Set thema voor figuren ----
#theme_set(theme_bw())
theme_update(
  panel.grid.major = element_line(color = "lightgrey", linewidth = 0.5),
  panel.grid.minor = element_blank(),
  panel.spacing = unit(0.3, "line"),
  legend.title = element_text(size = 15),
  legend.text = element_text(size = 14),
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 17),
  strip.background = element_blank(),
  strip.text = element_text(size = 12),
  legend.key = element_blank()
)

# maak de figuren met de create_plot functie
plots <- map(locations$rwzi_name, create_plot)
names(plots) <- locations$rwzi_name

# Laat figuren zien in R
walk2(plots, names(plots), ~ {
  cat("\n=== Plot for", .y, "===\n")
  print(.x)
})

# Sla de figuren op in de folder output/bedrijvenopkaart
walk2(plots, names(plots), ~ {
  ggsave(filename = paste0("plots/bedrijvenopkaart/", .y, "_sbi_plot.png"),
         width = 8.3, height = 5.8, dpi = 300,
         plot = .x)
})

# Hittekaart van relevante bedrijven binnen zuiveringskring ---------------

# Process all locations ----
all_data_heatmap <- map2_dfr(locations$rwzi_name, locations$bok_file, process_location_for_heatmap)

# bedrijven met meerdere sbi zitten er dubbel in. voor heatmap niet interessant
geocode_data <- all_data_heatmap[!duplicated(all_data_heatmap$address), ]

# de volgende stap haalt de x en y coordinaten van de adressen op via tidygeocoder package (kan erg lang duren)
# voor 1000 adressen duurt het ongeveer 5 a 10 minuten...
geocode_data <- geocode(geocode_data, address = address, method = "arcgis", lat = latitude, long = longitude, return_input = FALSE)
# dit kan sneller in batch mode maar dan is er wel een API key van bijv. google maps nodig: zie https://developers.google.com/maps/documentation/geocoding/overview
# api_key <- Sys.getenv("GOOGLEGEOCODE_API_KEY")  #  <- plaats key in je .Renviron map onder de naam GOOGLEGEOCODE_API_KEY, bijv GOOGLEGEOCODE_API_KEY = "jouw api key" 
#  geocode_data <- geocode(geocode_data, address = address, method = "google", lat = latitude, long = longitude, return_input = FALSE)

# check locaties die niet gelukt zijn 
failed_locations <- geocode_data |> 
  filter(is.na(longitude))
print(glue::glue("Voor {nrow(failed_locations)} van de {nrow(geocode_data)} adressen ({round(nrow(failed_locations)/nrow(geocode_data)*100,1)}%) is het geocoderen niet gelukt."))

if(nrow(failed_locations) != 0){
  print(failed_locations$address)
  # als adressen niet zijn gelukt kan je deze adress handmatig aanpassen naar een correct adres of het laten zoals het is (dan worden ze niet meegenomen in hittekaart)
  # bijv 2 adressen zijn niet gelukt, plaats hieronder dan de correcte adressen.
  adjusted_locations <- c("Stadhuisplein 1, 5461KN, Veghel, Netherlands",  # Gemeentehuis Veghel
                          "Markt 1, 5461JJ, Veghel, Netherlands")  # VVV veghel

  failed_locations$address <- adjusted_locations
  failed_locations <- failed_locations |> select(-latitude, -longitude)
  failed_locations <- failed_locations[!duplicated(failed_locations$address), ] # remove duplicates if they are present
  check_locs <- failed_locations %>% 
    tidygeocoder::geocode(address = address, method = 'arcgis', lat = latitude , long = longitude) |> 
    filter(!is.na(longitude))  # verwijder adressen die ook na aanpassing niet meer lukken
} else{
  check_locs <- tribble(~address, ~latitude, ~longitude)
}
# combineer aangepaste adreslijst met overige adressen
address_geodata <- geocode_data |> 
  filter(!is.na(longitude)) |> 
  bind_rows(check_locs)

# check data
mapview::mapview(address_geodata |> 
                   st_as_sf(coords = c("longitude", "latitude"),
                            remove = FALSE,
                            crs = 4326)  |> 
                   st_transform(crs = 28992)) # check bedrijf puntdata; geen punten in Frankrijk?


# creeer hittemap voor alle locaties
# kernel density estimator voor elke RWZI (contoours at 10%, 25%, 50% 75% an 90% of datapoints)

all_data_heatmap <- all_data_heatmap |> 
  left_join(address_geodata, by = join_by("address" == "address")) |> 
  filter(!is.na(longitude))

all_data_heatmap <- all_data_heatmap |> 
  st_as_sf(coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326) |> 
  st_transform(crs = 28992)
            
            
kde_analysis <- locations |>
  mutate(
    # Filter data for each location
    filtered_data = map(rwzi_name, ~{
      all_data_heatmap |> filter(rwzi == .x)
    }),
    # Calculate KDE for each location
    kde = map(filtered_data, st_kde),
    # Get contours for each KDE
    contours = map(kde, ~st_get_contour(.x, cont = c(10, 25, 50, 75, 90))),
    map_plot = map2(contours, rwzi_name, make_map)
  )
  
# check plots
print(kde_analysis$map_plot)

# export contours as sf to folder output/bedrijvenopkaart/contours
output_folder_contours <- "data/02_modified/bedrijvenopkaart/contours"
output_folder_plots <- "plots/bedrijvenopkaart"

if (!dir.exists(output_folder_contours)) {
  dir.create(output_folder_contours, recursive = TRUE)
}

# opslaan van plot en de shapefiles van de contour voor het geval je deze wilt bewerken met bijv. QGIS of ArcGIS
kde_analysis |>
  select(rwzi_name, contours) |>
  pwalk(~ {
    clean_name <- str_replace_all(.x, "[^A-Za-z0-9]", "_")
    filename <- paste0(output_folder_contours,"/", "contours_", clean_name, ".shp")
    # wegschrijven als shapefile
    st_write(.y, filename, delete = TRUE, append = FALSE, quiet = TRUE)
    cat("Exported:", filename, "\n")
  })

kde_analysis |>
  select(rwzi_name, map_plot) |> 
  pwalk(~ {
    clean_name <- str_replace_all(.x, "[^A-Za-z0-9]", "_")
    filename <- paste0(output_folder_plots,"/", "hittemap_", clean_name, ".png")
    # wegschrijven als plotje
    ggsave(filename = filename,
           width = 8.3, height = 5.8, dpi = 300,
           plot = .y)
    cat("Exported:", filename, "\n")
  })
