# ==============================================================================
# Script: PubChem Data Collection and Chemical Classification
# Auteur:  Kees Wesdorp (Deltares) & Peli Angouraki (KWR/VU)
# Datum:  13-02-2024
# ==============================================================================
#
# DOEL:
# 1. Verzamelen van moleculaire informatie uit PubChem (naam, SMILES, formule,
#    InChI, InChIKeys, monoisotopische massa, lading)
# 2. Classificeren van chemische structuren in categorieën:
#    - Inorganic
#    - Organic
#    - Organometallic
#    - Organic Ionic
#
# BENODIGDE INVOERGEGEVENS:
# - Suspectlist zonder molecuulinfo
#   (data/tussenproduct/suspectlist_zonder_molecuulinfo.parquet)
# - ZZS stoffenlijst (data/stoffenlijsten/stoffenlijst_all.parquet)
#
# EINDPRODUCT:
# - Excel bestand met complete compoundlist inclusief moleculaire informatie
#   en chemische classificatie (output/WP3_compoundlist_allcasestudies_YYYYMMDD.xlsx)
#
# ==============================================================================

# Packages laden ---------------------------------------------------------------
library(tidyverse)
library(webchem)
library(here)


# Suspectlist inladen ----------------------------------------------------------

WP3_compoundlist_allcasestudies <- arrow::read_parquet(
  "data/02_modified/suspectlist_zonder_molecuulinfo.parquet"
)

parents_list <- WP3_compoundlist_allcasestudies

cat(glue::glue("Suspectlist ingeladen: {nrow(parents_list)} rijen\n"))


# Duplicaten verwijderen op basis van CAS-nummer ------------------------------

parents_list_clean <- parents_list[
  !duplicated(parents_list$`CAS-nummer`) & !is.na(parents_list$`CAS-nummer`),
]
row.names(parents_list_clean) <- NULL

cat(glue::glue("Na verwijderen duplicaten: {nrow(parents_list_clean)} unieke CAS-nummers\n"))


# CID's ophalen uit PubChem ----------------------------------------------------

cat("\nCID's ophalen uit PubChem (dit kan even duren)...\n")

parentsCAStoCID <- get_cid(
  parents_list_clean$`CAS-nummer`,
  domain = "compound",
  match = "first",
  verbose = TRUE
)


# Controleren op missende CID's ------------------------------------------------

check_missing_cids <- function(matrix) {
  if (anyNA(matrix$cid)) {
    missing_cid_sum <- sum(is.na(matrix$cid))
    warning(paste(
      "Voor",
      missing_cid_sum,
      "CAS-nummers is geen CID gevonden"
    ))
    missing_cids <- matrix[is.na(matrix$cid), ]
    return(missing_cids)
  } else {
    cat("Alle CID's zijn gevonden\n")
    return(data.frame())
  }
}

missing_cid <- check_missing_cids(parentsCAStoCID)

# Namen van stoffen zonder CID ophalen
if (nrow(missing_cid) > 0) {
  missing_cid_data <- left_join(
    data.frame(cas_nm = missing_cid$query),
    data.frame(
      name = parents_list_clean$Stofnaam,
      cas_nm = parents_list_clean$`CAS-nummer`
    ),
    by = "cas_nm"
  )
  cat(glue::glue("Stoffen zonder CID: {nrow(missing_cid_data)}\n"))
}

# NA waarden verwijderen
parentsCAStoCID_clean <- parentsCAStoCID[!is.na(parentsCAStoCID$cid), ]
row.names(parentsCAStoCID_clean) <- NULL

cat(glue::glue("CID's beschikbaar: {nrow(parentsCAStoCID_clean)}\n"))


# Duplicaat CID's identificeren ------------------------------------------------

# Alle duplicaten (inclusief NA's)
parentsCAStoCID_duplicates <- parentsCAStoCID[
  duplicated(parentsCAStoCID$cid) |
    duplicated(parentsCAStoCID$cid, fromLast = TRUE),
]

# Duplicaten zonder NA's
parentsCAStoCID_duplicates_clean <- parentsCAStoCID_clean[
  duplicated(parentsCAStoCID_clean$cid) |
    duplicated(parentsCAStoCID_clean$cid, fromLast = TRUE),
]

if (nrow(parentsCAStoCID_duplicates_clean) > 0) {
  cat(glue::glue("Aantal duplicaat CID's: {nrow(parentsCAStoCID_duplicates_clean)}\n"))
}


# Moleculaire formule en SMILES ophalen ---------------------------------------

cat("\nMoleculaire formules en SMILES ophalen...\n")

parents_form_smiles <- pc_prop(
  parentsCAStoCID_clean$cid,
  c("MolecularFormula", "SMILES")
)

# CAS-nummers toevoegen
cas_nm <- parentsCAStoCID_clean$query
parents_form_smiles <- cbind(parents_form_smiles, cas_nm)
parents_form_smiles <- parents_form_smiles |>
  mutate(SMILES = str_trim(SMILES))

# Duplicaten verwijderen op basis van isomeric SMILES
parents_form_smiles_clean <- parents_form_smiles[
  !duplicated(parents_form_smiles$SMILES),
]
row.names(parents_form_smiles_clean) <- NULL

cat(glue::glue("Na verwijderen SMILES duplicaten: {nrow(parents_form_smiles_clean)} stoffen\n"))


# Volledige PubChem informatie ophalen -----------------------------------------

cat("\nVolledige PubChem informatie ophalen...\n")

parents_list_summary <- pc_prop(
  parents_form_smiles_clean$CID,
  properties = c(
    "CanonicalSMILES",
    "SMILES",
    "InChI",
    "InChIKey",
    "Title",
    "MonoisotopicMass",
    "MolecularFormula",
    "Charge"
  ),
  verbose = TRUE
)

# CAS-nummers toevoegen
CAS_nm <- parents_form_smiles_clean$cas_nm
parents_list_summary <- cbind(parents_list_summary, CAS_nm)

cat(glue::glue("PubChem data verzameld voor {nrow(parents_list_summary)} stoffen\n"))


# ==============================================================================
# CHEMISCHE CLASSIFICATIE
# ==============================================================================

cat("\n=== Chemische classificatie starten ===\n")

p_original <- parents_list_summary

# Duplicaten verwijderen op basis van CAS-nummer
p_cid <- p_original |>
  distinct(CAS_nm, .keep_all = TRUE)

# Duplicaten verwijderen op basis van SMILES
p_form_smiles_initial <- p_cid
p_cid <- p_cid[!duplicated(p_cid$SMILES), ]
row.names(p_cid) <- NULL

removed_smiles <- length(p_form_smiles_initial$CID) - length(p_cid$CID)
removed_pct <- (removed_smiles / length(p_form_smiles_initial$CID)) * 100
cat(glue::glue("{removed_smiles} ({round(removed_pct, 1)}%) SMILES verwijderd als duplicaten\n"))


# Splitsen in organisch en anorganisch -----------------------------------------

# Organisch: minstens 2 koolstofatomen
p_organic <- p_cid[str_detect(p_cid$MolecularFormula, regex("C[1-9]")), ]
row.names(p_organic) <- NULL

# Anorganisch: 1 of geen koolstofatoom
p_inorganic <- p_cid[!str_detect(p_cid$MolecularFormula, regex("C[1-9]")), ]
row.names(p_inorganic) <- NULL

cat(glue::glue("\nOrganische verbindingen: {nrow(p_organic)}\n"))
cat(glue::glue("Anorganische verbindingen: {nrow(p_inorganic)}\n"))


# Organische verbindingen verder classificeren ---------------------------------

# CONSPX: alleen C, O, N, S, P, halogenen (geen andere metalen)
p_CONSPX <- p_organic[
  !str_detect(p_organic$SMILES, regex("C[adenorus]")) &
    !str_detect(p_organic$SMILES, regex("S[bceni]")) &
    !str_detect(p_organic$SMILES, "Pb") &
    !str_detect(p_organic$SMILES, "Hg") &
    !str_detect(p_organic$SMILES, "[AKMYLG]") &
    !str_detect(p_organic$SMILES, "N[a-z]") &
    !str_detect(p_organic$SMILES, regex("B[^r]")),
]
row.names(p_CONSPX) <- NULL

# Niet-CONSPX: bevat andere elementen (organometallisch)
p_notCONSPX <- p_organic[
  str_detect(p_organic$SMILES, regex("C[adenorus]")) |
    str_detect(p_organic$SMILES, regex("S[bceni]")) |
    str_detect(p_organic$SMILES, "Pb") |
    str_detect(p_organic$SMILES, "Hg") |
    str_detect(p_organic$SMILES, "[AKMYLG]") |
    str_detect(p_organic$SMILES, "N[a-z]") |
    str_detect(p_organic$SMILES, regex("B[^r]")),
]
row.names(p_notCONSPX) <- NULL

cat(glue::glue("Organisch (CONSPX): {nrow(p_CONSPX)}\n"))
cat(glue::glue("Organometallisch: {nrow(p_notCONSPX)}\n"))


# Ionische structuren extraheren (zouten) --------------------------------------

# Zouten uit CONSPX
p_CONSPX_salts <- p_CONSPX[str_detect(p_CONSPX$SMILES, fixed(".")), ]
row.names(p_CONSPX_salts) <- NULL

p_CONSPX <- p_CONSPX[!str_detect(p_CONSPX$SMILES, fixed(".")), ]
row.names(p_CONSPX) <- NULL

# Zouten uit niet-CONSPX
p_notCONSPX_salts <- p_notCONSPX[str_detect(p_notCONSPX$SMILES, fixed(".")), ]
row.names(p_notCONSPX_salts) <- NULL

p_notCONSPX <- p_notCONSPX[!str_detect(p_notCONSPX$SMILES, fixed(".")), ]
row.names(p_notCONSPX) <- NULL

# Combineren van alle zouten
p_salts <- rbind(p_CONSPX_salts, p_notCONSPX_salts)

cat(glue::glue("Organisch ionisch (zouten): {nrow(p_salts)}\n"))


# Classificatie labels toevoegen -----------------------------------------------

p_inorganic <- p_inorganic |>
  mutate(class = "Inorganic") |>
  filter(!is.na(CID))

p_CONSPX <- p_CONSPX |>
  mutate(class = "Organic") |>
  filter(!is.na(CID))

p_notCONSPX <- p_notCONSPX |>
  mutate(class = "Organometallic") |>
  filter(!is.na(CID))

p_salts <- p_salts |>
  mutate(class = "Organic Ionic") |>
  filter(!is.na(CID))


# Alle classificaties combineren -----------------------------------------------

combi_df <- rbind(p_inorganic, p_CONSPX, p_notCONSPX, p_salts)

cat(glue::glue("\nTotaal geclassificeerde verbindingen: {nrow(combi_df)}\n"))
cat("  - Inorganic:", sum(combi_df$class == "Inorganic"), "\n")
cat("  - Organic:", sum(combi_df$class == "Organic"), "\n")
cat("  - Organometallic:", sum(combi_df$class == "Organometallic"), "\n")
cat("  - Organic Ionic:", sum(combi_df$class == "Organic Ionic"), "\n")

# Tussenproduct opslaan voor metabolieten analyse van Peli opslaan
write.csv(
  combi_df,
  here("data", "03_output", "stoffenlijst_voor_metabolieten_analyse.csv"),
  row.names = FALSE
)


# ==============================================================================
# FINALE DATASET SAMENSTELLEN
# ==============================================================================

cat("\n=== Finale dataset samenstellen ===\n")

# ZZS stofgroepen inladen
stoffenlijst_ZZS <- arrow::read_parquet(
  "data/02_modified/stoffenlijsten/ZZS_stoffenlijst_all.parquet"
) |>
  select(ZZS_Stofgroepen, Stofnaam, `CAS-nummer`) |>
  distinct()

# Alle data combineren
final_list <- WP3_compoundlist_allcasestudies |>
  left_join(combi_df, by = join_by(`CAS-nummer` == CAS_nm))


# Dataset formatteren ----------------------------------------------------------

final_list_WP3 <- final_list |>
  mutate(Stofnaam = if_else(is.na(Stofnaam), `Gepresenteerde Stofnaam ER`, Stofnaam),
         ) |>
  left_join(stoffenlijst_ZZS, by = join_by(Stofnaam, `CAS-nummer`)) |>
  select(
    # Case study informatie
    case_study = RWZI,
    
    # Stof informatie
    Stofgroep = class,
    ZZS_Stofgroepen,
    Stofnaam,
    `CAS-nummer`,
    AQUO_id,
    AQUO_code,
    AQUO_omschrijving,
    CID,
    Molecuulformule = MolecularFormula,
    SMILES,
    InChIKey,
    MonoisotopicMass,
    `Lading_molecuul` = Charge,
    naam_pubchem = Title,
    RIVM_naam_nl = `NL naam`,
    EC_nr = `EC nr`,
    
    # ZZS navigator informatie
    Emissie_verwacht,
    Emissie_verwacht_totaal_bedrijven = `totaal_bedrijven_Emissie verwacht`,
    Emissie_verwacht_unieke_SBIcodes = `unique_sbicodes_Emissie verwacht`,
    Emissie_mogelijk,
    Emissie_mogelijk_totaal_bedrijven = `totaal_bedrijven_Emissie mogelijk`,
    Emissie_mogelijk_unieke_SBIcodes = `unique_sbicodes_Emissie mogelijk`,
    
    # ER data
    ER_stofcode = ER_code,
    `Gepresenteerde Stofnaam ER`,
    Bedrijf,
    Sbi = Sbi_naam,
    Emissie,
    Eenheid,
    Emissiejaar = Jaar,
    ER_Herkomst = Herkomst,
    ER_Dataset = Dataset,
    
    # PMT informatie
    PMT_name:`T-score Explanation`
  ) |>
  arrange(
    case_study,
    desc(Emissie),
    Stofgroep,
    desc(Emissie_verwacht),
    desc(Emissie_verwacht_totaal_bedrijven),
    desc(Emissie_verwacht_unieke_SBIcodes),
    desc(Emissie_mogelijk),
    desc(Emissie_mogelijk_totaal_bedrijven),
    desc(Emissie_mogelijk_unieke_SBIcodes)
  )

final_list_organic <- final_list_WP3 |> filter(Stofgroep %in% c("Organic", "Organic Ionic ", "Organometallic"))

# Output wegschrijven ----------------------------------------------------------

current_date <- today() |> format("%Y%m%d")

output_file <- here(
  "data",
  "03_output",
  glue::glue("WP3_compoundlist_allcasestudies_{current_date}.xlsx")
)
output_organic <- here(
  "data",
  "03_output",
  glue::glue("WP3_compoundlist_allcasestudies_organic_{current_date}.xlsx")
)

writexl::write_xlsx(final_list_WP3, output_file)
writexl::write_xlsx(final_list_organic, output_organic)
cat(glue::glue("\nScript voltooid!\n"))
cat(glue::glue("Finale dataset: {nrow(final_list_WP3)} rijen\n"))
cat(glue::glue("Output opgeslagen: {output_file}\n"))
