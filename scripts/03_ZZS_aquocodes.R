# koppel RIVM ZZS stoffen aan aquo codes via CAS-nummer & Aquo domein tabellen m.b.v. aquodom package

# DOEL: stoffenlijst maken die in script 03C wordt gebruikt om verschillende tabellen aan elkaar te koppelen.


# library
library(tidyverse)
source("scripts/helper_functions/dom_package_andere_locale/dom_package_aangepast.R") # aquodom package (https://redtent.github.io/aquodom/) maar dan aangepast dat lokale tijd werkt

# inladen stof data ZZS navigator
ZZS_SBI_totaal <- readRDS("data/02_modified/ZZS_navigator/ZZS_SBI_totaal.rds")
stoffenlijst <- ZZS_SBI_totaal %>% select(Stofgroep:length(ZZS_SBI_totaal)) %>% distinct()

# inladen aquoparameters
aquo_parameters <- dom("parameter")

# samenvoegen stoffenlijst + aquocodes op basis van CAS-nummers
stoffenlijst_zzs_aquo <- stoffenlijst %>%
  left_join(aquo_parameters, by = join_by(`CAS-nummer` == casnummer))

gekoppeld <- nrow(stoffenlijst_zzs_aquo |> filter(!is.na(codes))) # aantal stoffen gekoppeld met aquocode op basis van CAS-nummer
print(glue::glue("Er zijn {gekoppeld} van de {nrow(stoffenlijst)} [{round(gekoppeld/nrow(stoffenlijst)*100, 0)}%] stoffen gekoppeld aan aquocodes o.b.v CAS-nummer via de domeintabellen van Aquo"))
print(glue::glue("De aquocodes worden verder niet gebruikt in de scripts maar zijn wel handig om er bij te hebben
                 voor eventuele koppeling met andere databases"))

stoffenlijst_totaal <- stoffenlijst_zzs_aquo %>%
  pivot_wider(names_from = Stofgroep, values_from = Stofgroep) %>%
  unite("stofgroep", starts_with("ZZS "), remove = T, sep = ", ") %>%
  mutate(
    ZZS_Stofgroepen = str_replace_all(stofgroep, "NA, ", ""),
    ZZS_Stofgroepen = str_replace_all(ZZS_Stofgroepen, "NA", ""),
    ZZS_Stofgroepen = if_else(
      is.na(`Individuele stoffen`),
      ZZS_Stofgroepen,
      paste(`Individuele stoffen`, ZZS_Stofgroepen, sep = ", ")
    ),
    ZZS_Stofgroepen = gsub(", $", "", ZZS_Stofgroepen)
  ) %>%
  select(
    ZZS_Stofgroepen,
    everything(),
    -c("stofgroep", "Individuele stoffen")
  ) %>%
  distinct()

dupes <- stoffenlijst_totaal %>%
  group_by(Stofnaam, `CAS-nummer`, `NL naam`, Emissiestatus) %>%
  summarize(n = n()) %>%
  filter(n > 1)

if (nrow(dupes) > 0) {
  print("Er zitten een aantal dubbele stoffen in de lijst:")
  # lets fix this
  a <- stoffenlijst_totaal %>%
    filter(`CAS-nummer` %in% dupes$`CAS-nummer`) %>%
    group_by(across(c(-ZZS_Stofgroepen))) %>%
    summarise(ZZS_Stofgroepen = first(ZZS_Stofgroepen)) %>%
    filter(`CAS-nummer` != "NVT")
  
  stoffenlijst_totaal <- stoffenlijst_totaal %>%
    filter(!`CAS-nummer` %in% dupes$`CAS-nummer`)
  stoffenlijst_totaal <- rbind(stoffenlijst_totaal, a) |> distinct()
  
  dupes <- stoffenlijst_totaal %>%
    group_by(Stofnaam, `CAS-nummer`, `NL naam`, Emissiestatus) %>%
    summarize(n = n()) %>%
    filter(n > 1)
}

# wegschrijven van totale stoffenlijst

output_folder <- "data/02_modified/stoffenlijsten"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

stoffenlijst_totaal |>
  writexl::write_xlsx(glue::glue("{output_folder}/ZZS_stoffenlijst_all.xlsx"))
stoffenlijst_totaal |>
  arrow::write_parquet(glue::glue("{output_folder}/ZZS_stoffenlijst_all.parquet"))


