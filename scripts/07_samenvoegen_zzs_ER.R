# ==============================================================================
# Script: Samenvoegen ZZS en ER lijsten tot suspectlist
# Datum:  2024-06-26
# ==============================================================================
#
# DOEL:
# Samenvoegen van verschillende lijsten (ER en ZZS) om één suspectlist te maken
#
# BENODIGDE INVOERGEGEVENS:
# - Excel output van BedrijvenopKaart met bedrijfsinformatie
#   (data/Bedrijvenopkaart/BOK_dummy.xlsx)
# - SBI_ZZS koppeltabel gegenereerd in script "01_maak_SBI_ZZS_koppeltabel"
#   (output/ZZS_navigator/ZZS_SBI_totaal.rds)
# - Selectie SBI lijst met SBI codes met verwachte emissies o.b.v. expert judgement
#   (data/selectie_SBI_v2.xlsx)
#
# EINDPRODUCT:
# Output/bedrijvenopkaart bevat:
# - Excels per RWZI met verwachte en mogelijk verwachte stoffen voor bedrijven
#   geselecteerd via Bedrijvenopkaart.nl
# - Figuren met aantal bedrijven per top 20 SBI hoofdindelingen binnen
#   geselecteerd gebied en geselecteerde SBIs
#
# ==============================================================================

# Packages laden ---------------------------------------------------------------
library(tidyverse)


# ZZS data per RWZI inladen ----------------------------------------------------

# Specificeer ZZS bestanden
# NB: Vul hier de namen in van excels gegenereerd in script 02
#     Moeten in folder output/bedrijvenopkaart staan
ZZS_files <- c(
  "DummyRWZI1_ZZS.xlsx",
  "DummyRWZI2_ZZS.xlsx"
)

input_folder <- "data/03_output/bedrijvenopkaart"

# Importeer en combineer alle ZZS bedrijvenopkaart info
RWZI_ZZS <- ZZS_files |>
  (\(x) file.path(input_folder, x))() |>
  lapply(function(f) {
    readxl::read_excel(f) |>
      mutate(
        RWZI = tools::file_path_sans_ext(basename(f)) |>
          str_remove("_ZZS")
      )
  }) |>
  bind_rows()

cat(glue::glue("ZZS data ingeladen: {nrow(RWZI_ZZS)} rijen voor {n_distinct(RWZI_ZZS$RWZI)} RWZI's\n"))


# Stoffenlijst ZZS inladen -----------------------------------------------------

# Importeer stoffenlijst ZZS met aquocodes
stoffenlijst_ZZS <- arrow::read_parquet(
  "data/02_modified/stoffenlijsten/ZZS_stoffenlijst_all.parquet"
)

cat(glue::glue("Stoffenlijst ZZS ingeladen: {nrow(stoffenlijst_ZZS)} stoffen\n"))


# ZZS lijst koppelen met stoffenlijst ------------------------------------------

ZZS_lijst <- RWZI_ZZS |>
  left_join(
    stoffenlijst_ZZS |> select(Stofnaam, aquo_id = id, aquo_code = codes, aquo_omschrijving = omschrijving) |> distinct(),
    by = join_by(Stofnaam == Stofnaam), na_matches = "never"
  )

# RWZI namen aanpassen (alleen voor voorbeeld!)
# NB: Dit is alleen voor het voorbeeld. Hoeft niet bij echte casus omdat
#     dan de namen gelijk moeten zijn
ZZS_lijst <- ZZS_lijst |>
  mutate(
    RWZI = case_when(
      RWZI == "DummyRWZI1" ~ "Bath",
      RWZI == "DummyRWZI2" ~ "Dinther",
      .default = RWZI
    )
  )

cat(glue::glue("ZZS lijst verwerkt: {nrow(ZZS_lijst)} rijen\n"))


# ER en Watson data inladen ----------------------------------------------------

ER_watson_data <- readxl::read_xlsx(
  "data/03_output/Emissieregistratie_Watson/ER_watson_data.xlsx"
)

cat(glue::glue("ER/Watson data ingeladen: {nrow(ER_watson_data)} rijen\n"))


# Suspectlist maken ------------------------------------------------------------

# Combineer ZZS en ER/Watson data op basis van CAS-nummer en RWZI naam
suspectlist <- ZZS_lijst |>
  full_join(
    ER_watson_data,
    by = join_by(`CAS-nummer` == casnummer, RWZI == RWZI_NAAM),
    na_matches = "never") |> 
  mutate(Stofnaam = if_else(is.na(Stofnaam), `Gepresenteerde Stofnaam ER`, Stofnaam),
         AQUO_id = if_else(is.na(aquo_id), AQUO_id, aquo_id),
         AQUO_code = if_else(is.na(aquo_code), AQUO_code, aquo_code),
         AQUO_omschrijving = if_else(is.na(aquo_omschrijving), omschrijving, aquo_omschrijving),
         ) |> 
  select(Stofnaam, `CAS-nummer`, AQUO_id, AQUO_code, AQUO_omschrijving, everything(), -c(aquo_omschrijving, aquo_id, omschrijving, aquo_code))

cat(glue::glue("\nSuspectlist aangemaakt: {nrow(suspectlist)} rijen\n"))
cat(glue::glue("  - Aantal unieke stoffen: {n_distinct(suspectlist$`CAS-nummer`)}\n"))
cat(glue::glue("  - Aantal RWZI's: {n_distinct(suspectlist$RWZI)}\n"))


# Output wegschrijven ----------------------------------------------------------

# Output folder aanmaken indien niet bestaat

# Suspectlist opslaan als Parquet bestand
arrow::write_parquet(
  suspectlist,
  "data/02_modified/suspectlist_zonder_molecuulinfo.parquet"
)
writexl::write_xlsx(
  suspectlist,
  "data/02_modified/suspectlist_zonder_molecuulinfo.xlsx"
)

cat("\nScript voltooid!")
cat("\nOutput opgeslagen: data/02_modified/suspectlist_zonder_molecuulinfo\n")
