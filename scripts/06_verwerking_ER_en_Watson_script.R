# ==============================================================================
# Script: Samenvoegen ER en Watson lijsten
# Datum:  2024-06-28
# ==============================================================================
#
# DOEL:
# Samenvoegen van de verschillende lijsten (ER en Watson) om straks te kunnen
# combineren met ZZS lijst
#
# BENODIGDE INVOERGEGEVENS:
# - Excel output van Watson meetresultaten
#   (data/Emissieregistratie/Watson_download/Watson_Meetresultaten-*.xlsx)
# - Excel output van emissieregistratie (ER_data_*.xlsx)
# - Koppeltabel Aquocodes met ER stofcodes
#   (data/koppeltabellen/ER_aquo_koppeltabel_2024.xlsx)
#
# EINDPRODUCT:
# Excel bestand in output/Emissieregistratie_Watson met per RWZI:
# - Opgegeven vrachten per bedrijf binnen zuiveringskring via ER
# - Driejarig-gemiddelde influent concentraties van RWZI via Watson
#
# ==============================================================================

# Packages laden ---------------------------------------------------------------
library(tidyverse)
source("scripts/helper_functions/dom_package_andere_locale/dom_package_aangepast.R")


# Watson data inladen ----------------------------------------------------------

# Vind meest recente Watson bestand
watson_files <- list.files(
  "data/02_modified/Emissieregistratie/Watson_download",
  pattern = "^Watson_Meetresultaten.*\\.xlsx$",
  full.names = TRUE
)

watson_dates <- as.POSIXct(
  str_extract(basename(watson_files), "\\d{4}-\\d{2}-\\d{2}-\\d{6}"),
  format = "%Y-%m-%d-%H%M%S"
)

# Bestand met meest recente export datum selecteren
watson_meest_recent <- watson_files[which.max(watson_dates)]
print(glue::glue("Ophalen van Watson data uit bestand {watson_meest_recent}"))

# Inlezen Watson data
watson_influent <- readxl::read_xlsx(
  watson_meest_recent,
  sheet = "Meetresultaten"
)

# Filteren en berekenen vracht
watson_influent_filter <- watson_influent |>
  filter(JAAR == "Alle" & MED > 0 & RWZICODE != "Alle") |>
  mutate(
    # Inschatting vracht (kg) op basis van mediaan
    vracht_kg_jaar = round(MED * 365 / 1000000, 2)
  )


# ER bedrijfsdata inladen ------------------------------------------------------

# Vind ER bedrijvendata in folder (eindproduct van script 04)
ER_files <- list.files(
  "data/02_modified/Emissieregistratie",
  pattern = "^ER_data_.*\\.xlsx$",
  full.names = TRUE
)

ER_data <- tibble(filename = ER_files) |>
  mutate(contents = map(filename, ~readxl::read_excel(.x))) |>
  unnest(contents)


# Koppeltabellen inladen -------------------------------------------------------

# ER koppeltabel met aquocodes (voor ophalen CAS-nummers)
er_koppel <- readxl::read_xlsx(
  "data/01_raw/koppeltabellen/ER_aquo_koppeltabel_2024.xlsx"
)

# Aquo parameters via DOM package API (altijd up-to-date)
aquo_parameters <- dom("parameter")


# ER data verwerken ------------------------------------------------------------

# Koppelen met ER-Aquo koppeltabel
ER_data2 <- ER_data |>
  mutate(Stofcode = as.integer(Stofcode)) |>
  left_join(
    er_koppel,
    by = join_by(
      Stofcode == ER_Code,
      Stof == gerepresenteerde_stofnaam_ER
    )
  )

# Overbodige kolommen verwijderen
ER_data2 <- ER_data2 |>
  select(
    RWZI_NAAM, Bedrijf, Sbi_code, Sbi_naam,
    ER_Stofcode = Stofcode,
    ER_Stof = Stof,
    Emissie, Eenheid, Jaar, Herkomst, Dataset,
    everything(),
    -c(
      filename, Compartimentcode, Compartiment, Gebiedsindeling,
      Code_gebied, Gebied, Brontype, Sector, Subsector,
      Emissieoorzaak, Emissieoorzaak_code, Nic, Straat,
      Huisnummer, Toevoeging, Postcode, Woonplaats,
      Xcoord, Ycoord, Herkomst_code
    )
  )

# Check stoffen zonder aquocode
cat("\nStoffen zonder aquocode:\n")
ER_data2 |>
  filter(is.na(AQUO_code)) |>
  distinct(ER_Stofcode, ER_Stof) |>
  print()

# Koppelen met Aquo parameters
ER_data2 <- ER_data2 |>
  left_join(aquo_parameters, by = join_by(AQUO_code == codes))

# Check stoffen zonder aquo omschrijving
cat("\nStoffen zonder Aquo omschrijving:\n")
ER_data2 |>
  filter(is.na(omschrijving)) |>
  distinct(ER_Stof) |>
  print()

# ER data naar juiste format
ER_eind <- ER_data2 |>
  select(
    RWZI_NAAM,
    AQUO_code,
    AQUO_id = id,
    casnummer,
    omschrijving,
    ER_code = ER_Stofcode,
    `Gepresenteerde Stofnaam ER` = ER_Stof,
    Bedrijf,
    Sbi_naam,
    Emissie,
    Eenheid,
    Jaar,
    Herkomst,
    Dataset
  ) |>
  arrange(RWZI_NAAM, desc(Emissie))


# Watson data verwerken --------------------------------------------------------

# Koppelen met Aquo parameters
watson_influent_filter <- watson_influent_filter |>
  left_join(aquo_parameters, by = join_by(PARAMETERCODE == codes))

# Check Watson stoffen zonder aquo omschrijving
cat("\nWatson stoffen zonder Aquo omschrijving:\n")
watson_influent_filter |>
  filter(is.na(omschrijving)) |>
  distinct(STOFNAAM) |>
  print()

# Watson data naar juiste format
watson_voor_ER <- watson_influent_filter |>
  mutate(
    RWZI_naam = glue::glue("RWZI {str_to_title(LOCATIE)}"),
    Casus_naam = glue::glue("{str_to_title(LOCATIE)}"),
    ER_code = as.numeric(ERCODE),
    Jaar = as.numeric(2023),
    Eenheid = "kg",
    Sbi_naam = "Afvalwaterinzameling en -behandeling",
    Herkomst = "Inschatting o.b.v. Watson 3 jarig gem. influent concentratie data",
    Dataset = glue::glue("Watson database ER - 2023")
  ) |>
  select(
    RWZI_NAAM = Casus_naam,
    AQUO_code = PARAMETERCODE,
    AQUO_id = id,
    casnummer = CAS_NUMMER,
    omschrijving,
    ER_code,
    `Gepresenteerde Stofnaam ER` = STOFNAAM,
    Bedrijf = RWZI_naam,
    Sbi_naam,
    Emissie = vracht_kg_jaar,
    Eenheid,
    Jaar,
    Herkomst,
    Dataset
  )


# ER en Watson combineren ------------------------------------------------------

ER_watson_df <- bind_rows(ER_eind, watson_voor_ER) |>
  arrange(RWZI_NAAM, desc(Emissie))


# Output wegschrijven ----------------------------------------------------------

# Output folder aanmaken indien niet bestaat
output_folder <- "data/03_output/Emissieregistratie_Watson"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
  cat("\nSubfolder 'Emissieregistratie_Watson' aangemaakt in map 'data/03_output'\n")
}


# Excel bestand wegschrijven
writexl::write_xlsx(
  ER_watson_df,
  glue::glue("{output_folder}/ER_watson_data.xlsx")
)

cat(glue::glue("\nScript voltooid! Output opgeslagen in {output_folder}/\n"))
