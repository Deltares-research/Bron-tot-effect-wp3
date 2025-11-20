# Script om ZZS Navigator-lijst met stoffen te koppelen aan SBI-codes volgens Bijlage 3
# van achtergrondinformatie ZZS Navigator: Allocatie industriecodes aan Bal-activiteiten
# (https://rvs.rivm.nl/documenten/achtergrondinformatie-zzs-navigator)

# Datum:   2024-06-26

# Benodigde invoergegevens voor dit script: 
# - alle ZZS tekstbestanden van elke BAL activiteit (zie data\01_raw\ZZS_navigator\download_20230817)
# - koppeltabel van Bal-activiteiten naar SBI-codes die CBS en KvK hanteren (data\01_raw\ZZS_navigator\ZZS_smiles_20230718.xlsx)
# - ZZS-componentenlijst van RIVM (opgevraagd op 2023-07-18 voor het koppelen van SMILE (data\01_raw\ZZS_navigator\SBI_naar_Bal_ZZS.xlsx)
# - PMT tool scores van RIVM PMT tool (opgevraagd op 2023-08-21) - https://rvszoeksysteem.rivm.nl/ScreeningTool  zie data\01_raw\ZZS_navigator\PMT_tool
# Eindproduct van script:
# - koppeltabel tussen alle ZZS stoffen van de ZZS-navigator en hun verwachte gebruik bij bedrijfsactiviteiten (o.b.v. SBI-codes)
# - Doel van tabel is om bij vervolgstappen de bedrijven opgevraagd vanuit hun KvK registratie te kunnen koppelen aan mogelijke gebruik ZZS stoffen via de SBI

# Bibliotheken en functies ----------------------------------------------------

library("tidyverse")

# Functie om aantal tekens vóór een punt te tellen in een tekst
# Als er geen punt voorkomt, wordt 0 geretourneerd
count_characters_before_dot <- function(text) {
  pattern <- "^(.*?)\\."
  match <- regexpr(pattern, text)
  
  if (match != -1) {
    count <- attr(match, "match.length") - 1
    return(count)
  } else {
    return(0)
  }
}

# Functie om SBI-format te transformeren
transform_sbi_bal <- function(df, sbi_col) {
  # Nog uitbreiden naar andere inputmogelijkheden
  measures <- as_tibble(df[[sbi_col]])
  colnames(measures) <- "SBI"
  
  measures <- measures %>%
    mutate(
      SBI_decimal = str_replace_all(SBI, pattern = ",", replacement = "."),
      getallen = str_count(SBI_decimal, '\\d'),
      chr_before_dot = sapply(SBI_decimal, count_characters_before_dot),
      SBI_check = case_when(
        getallen == 1 ~ paste0(0, SBI_decimal),
        chr_before_dot == 1 ~ paste0(0, SBI_decimal),
        TRUE ~ SBI_decimal
      ),
      SBI_decimaal = ifelse(
        getallen - chr_before_dot == 3,
        gsub('^(.*)(.{1})$', '\\1.\\2', SBI_check),
        SBI_check
      ),
      SBI_bedrijvenopkaart = str_replace_all(SBI_decimaal, "\\.", ""),
      SBI_ER = str_pad(SBI_bedrijvenopkaart, width = 5, pad = "0", side = "right")
    ) %>%
    select(-SBI_decimal, -getallen, -chr_before_dot, -SBI_check)
  
  df <- cbind(df[, !(names(df) %in% sbi_col)], measures)
  return(df)
}

# ZZS Navigator-gegevens importeren en opschonen --------------------------

# Importeer componenten uit ZZS Navigator-lijst
ZZS_navi <- list.files("data/01_raw/ZZS_navigator/download_20230817", full.names = TRUE, pattern = '*.txt') %>%
  map_df(~ vroom::vroom(., id = 'file')) %>%
  filter(!is.na(Stofnaam))

# Controleer of er stoffen zijn die wel in een afdelingslijst staan, maar niet in een branche
ZZS_zonder_branche <- ZZS_navi %>%
  filter(is.na(Branche)) %>%
  select(Afdeling, Stofgroep, Stofnaam, `CAS-nummer`) %>%
  distinct()

ZZS_met_branche <- ZZS_navi %>%
  filter(!is.na(Branche)) %>%
  select(Afdeling, Stofgroep, Stofnaam, `CAS-nummer`) %>%
  distinct()

verschil <- setdiff(ZZS_zonder_branche, ZZS_met_branche)
verschil # Alleen formaldehyde staat bij 3.6 Agrarische sector, maar is nergens terug te vinden in een van de branches van die afdeling

brancheloze_stoffen <- ZZS_navi %>%
  filter(
    Afdeling %in% verschil$Afdeling &
      Stofgroep %in% verschil$Stofgroep &
      Stofnaam %in% verschil$Stofnaam &
      `CAS-nummer` %in% verschil$`CAS-nummer`
  )

ZZS_totaal <- rbind(
  ZZS_navi %>% filter(!is.na(Branche)),
  brancheloze_stoffen
) %>%
  select(-file) %>%
  distinct() # Verwijder dubbele stoffen en voeg brancheloze stoffen toe

# Controle van totalen
check1 <- ZZS_totaal %>%
  select(Stofnaam, `CAS-nummer`) %>%
  distinct() # In totaal 2025 unieke stoffen met 1910 unieke CAS-nummers

# Voeg InChI codes toe aan componenten met ZZS-componentenlijst van RIVM (2023-07-18)
ZZS_smiles <- readxl::read_xlsx("data/01_raw/ZZS_navigator/ZZS_smiles_20230718.xlsx") # 2110 stoffen

# Controleer of alle stoffen uit ZZS_totaal in deze lijst staan
setdiff(check1$`CAS-nummer`, ZZS_smiles$`Cas nr`) # 3 CAS-nummers ontbreken
setdiff(check1$Stofnaam, ZZS_smiles$`NL naam`) # 16 stofnamen ontbreken

# Voeg PMT-scores toe op basis van CAS-nummer via screening tool
PMT_cas <- list.files("data/01_raw/ZZS_navigator/PMT_tool", full.names = TRUE, pattern = '*.csv') %>%
  map_df(~ vroom::vroom(., id = 'file')) %>%
  rename(PMT_name = "Name", CAS_RN = "CAS  RN") # 245 stoffen hebben een PMT-score

ZZS_smiles_PMT <- ZZS_smiles %>%
  left_join(PMT_cas, by = join_by(`Cas nr` == CAS_RN)) %>%
  select(-file)

# Combineer Smiles + PMT met ZZS_totaal lijst
ZZS_totaal <- ZZS_totaal %>%
  left_join(
    ZZS_smiles_PMT,
    by = join_by(Stofnaam == `NL naam`, `CAS-nummer` == `Cas nr`),
    keep = TRUE
  ) %>%
  select(
    Afdeling:Compartiment,
    Stofgroep,
    Stofnaam,
    `CAS-nummer`,
    `NL naam`:`smiles code`,
    Emissiestatus,
    PMT_name:`T-score Explanation`,
    -`Cas nr`
  )

# BAL-activiteiten volgens bijlage --------------------------------------------------------

# Importeer data van BAL-lijst naar SBI-lijst
bal_data <- readxl::read_xlsx("data/01_raw/ZZS_navigator/SBI_naar_Bal_ZZS.xlsx", skip = 2)
bal_data <- transform_sbi_bal(bal_data, "SBI_bijlage")

# Koppel SBI-code aan BAL-activiteiten -----------------------------------

ZZS_SBI_totaal <- ZZS_totaal %>%
  left_join(
    bal_data,
    by = join_by(
      Afdeling == Afdeling,
      Branche == Branche,
      Subbranche == Subbranche
    ),
    relationship = "many-to-many"
  ) %>%
  rename(
    Bal_Afdeling = Afdeling,
    Bal_Branche = Branche,
    Bal_Subbranche = Subbranche
  ) %>%
  select(
    SBI_bedrijvenopkaart,
    SBI_ER,
    SBI,
    SBI_decimaal,
    SBI_naam,
    bal_nr,
    Bal_Afdeling,
    Bal_Branche,
    Bal_Subbranche,
    Stofgroep:Emissiestatus,
    everything()
  ) %>%
  arrange(SBI_ER) %>%
  filter(!is.na(SBI_bedrijvenopkaart))

# stof heeft alleen een Alleen NL of UK naam als CAS-nummer aanwezig is

# ZZS alleen aan SBI
ZZS_SBI_breed <- ZZS_SBI_totaal %>%
  mutate(waarde = 1) %>%
  select(SBI_decimaal, waarde, Stofgroep:`T-score Explanation`) %>%
  distinct() %>%
  pivot_wider(names_from = SBI_decimaal, values_from = waarde)

# Verwijder lege rijen
ZZS_SBI_breed <- ZZS_SBI_breed |>
  filter(!if_all(23:ncol(ZZS_SBI_breed), is.na))

# Opslaan van dataset ------------------------------------------------------------

# Folder + subfolder naam (indien nog niet bestaand)
main_folder <- "data/02_modified"
sub_folder <- file.path(main_folder, "ZZS_navigator")

# Controleer of folder bestaat
if (!dir.exists(main_folder)) {
  dir.create(main_folder)
  cat("Folder '02_modified' aangemaakt.\n")
} else {
  cat("Folder '02_modified' bestaat al.\n")
}

# Controleer of subfolder bestaat
if (!dir.exists(sub_folder)) {
  dir.create(sub_folder)
  cat("Subfolder 'ZZS_navigator' aangemaakt in '02_modified'.\n")
} else {
  cat("Subfolder 'ZZS_navigator' bestaat al.\n")
}

# Opslaan als CSV-bestand
write_excel_csv2(ZZS_SBI_breed, glue::glue("{sub_folder}/ZZS_SBI_breed.csv"))

# sla totale lijst op als csv file & R file
saveRDS(ZZS_SBI_totaal, glue::glue("{sub_folder}/ZZS_SBI_totaal.rds"))
