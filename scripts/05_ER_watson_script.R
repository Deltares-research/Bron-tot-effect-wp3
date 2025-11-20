# Watson
# Ophalen van influentdata RWZI via Watson database van Emissieregistratie --------------------------

# Ga naar https://data.emissieregistratie.nl/watson
# - Selecteer alle stoffen (of stofgroep waarin je interesse hebt)
# - Vul RWZI in die overeenkomt met RWZI_naam
# - Bij rapportage:
#     - Selecteer meetresultaten per stof
#     - Selecteer influent (of effluent, afhankelijk van je vraag. Effluent heeft vaak meer metingen,
#       maar daar is de zuivering al overheen gegaan, dus de vrachten zijn niet representatief voor de lozingen)
#     - Selecteer vracht
#     - Selecteer vanaf 2020 tot nu (we gebruiken gemiddelde vracht vanaf die periode)
#     - Selecteer per stof per RWZI per rapportagejaar
# -> Klik op exporteer -> download
# Plaats het bestand in de folder data/Emissieregistratie/Watson_download

# Alternatief: gebruik onderstaand dynamisch webscraping script voor Firefox
# -------------------------------------------------------------------------
# !!!!!! Let op: je moet voor dit script wel Firefox browser geïnstalleerd hebben !!!!!!!!
# -------------------------------------------------------------------------
# Plaats het Watson downloadbestand in data/Emissieregistratie/Watson_download nadat je het script hebt uitgevoerd.

library(tidyverse)
library(RSelenium)  # Package voor automatisch browsen
rJavaEnv::use_java("21")  # nodig voor opzeten java env 

# Invoerparameters -------------------------------------------------------
# Selecteer de juiste RWZI-namen zoals ze in Watson staan waarvoor je influentdata wilt ophalen
RWZI_names = c("Bath", "Dinther", "Heugem")  # is niet hoofdletter gevoelig
begin_periode <- "2020-01-01"  # Beginperiode vanaf wanneer je data wilt ophalen
eind_periode <- "2025-01-01"  # Er zit tot 2023 in Watson, dus je middelt nu vanaf 2020-2023 (3-jarig gemiddelde)

# Functies ---------------------------------------------------------------

# Functie om te wachten tot een element geladen is (voor headless modus)
wait_for_element <- function(using, value, timeout = 10) {
  start_time <- Sys.time()
  while(difftime(Sys.time(), start_time, units = "secs") < timeout) {
    tryCatch({
      element <- remDr$findElement(using = using, value = value)
      return(element)
    }, error = function(e) {
      Sys.sleep(0.5)
    })
  }
  stop("Webelement niet gevonden binnen de tijdslimiet")
}

# Functie om groepen te selecteren met foutafhandeling en rapportage
select_groups <- function(group_list, selected_names = NULL) {
  # Indien geen specifieke selectie, gebruik alles
  if (is.null(selected_names)) {
    selected_names <- names(group_list)
  }
  results <- data.frame(
    name = character(),
    success = logical(),
    error_message = character(),
    stringsAsFactors = FALSE
  )
  for (group_name in selected_names) {
    if (group_name %in% names(group_list)) {
      xpath <- group_list[group_name]
      cat("Probeer te klikken:", group_name, "\n")
      result <- tryCatch({
        element <- remDr$findElement(using = 'xpath', value = xpath)
        element$clickElement()
        # Sys.sleep(0.2)  # Small delay
        results <- rbind(results, data.frame(
          name = group_name,
          success = TRUE,
          error_message = "",
          stringsAsFactors = FALSE
        ))
        cat("✓ Succesvol geklikt:", group_name, "\n")
        TRUE
      }, error = function(e) {
        error_msg <- e$message
        cat("✗ Mislukt te klikken:", group_name, "- Error:", error_msg, "\n")
        results <<- rbind(results, data.frame(
          name = group_name,
          success = FALSE,
          error_message = error_msg,
          stringsAsFactors = FALSE
        ))
        FALSE
      })
    } else {
      cat("⚠ Naam niet gevonden in lijst:", group_name, "\n")
      results <- rbind(results, data.frame(
        name = group_name,
        success = FALSE,
        error_message = glue::glue("Niet gevonden in {deparse(substitute(group_list))}"),
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results)
}

# Functie om startdatum te zetten via JavaScript
set_start_date_js <- function(date_string) {
  tryCatch({
    js_script <- paste0("document.getElementById('report-start').value = '", date_string, "';")
    remDr$executeScript(js_script)
    trigger_script <- "document.getElementById('report-start').dispatchEvent(new Event('change'));"
    remDr$executeScript(trigger_script)
    return(TRUE)
  }, error = function(e) {
    cat("✗ Startdatum instellen mislukt - Error:", e$message, "\n")
    return(FALSE)
  })
}

# Functie om einddatum te zetten via JavaScript
set_end_date_js <- function(date_string) {
  tryCatch({
    js_script <- paste0("document.getElementById('report-end').value = '", date_string, "';")
    remDr$executeScript(js_script)
    trigger_script <- "document.getElementById('report-end').dispatchEvent(new Event('change'));"
    remDr$executeScript(trigger_script)
    return(TRUE)
  }, error = function(e) {
    cat("✗ Einddatum instellen mislukt - Error:", e$message, "\n")
    return(FALSE)
  })
}

# # Functie om checkbox direct te selecteren via xpath
select_checkbox_direct_xpath <- function(title_text) {
  tryCatch({
    xpath <- paste0("//div[@class='item'][.//p[@class='name' and contains(@title, '", title_text, "')]]//div[@class='checkmarkContainer']")
    checkmark_container <- remDr$findElement(using = 'xpath', value = xpath)
    checkmark_container$highlightElement()
    checkmark_container$clickElement()
    cat("✓ Checkbox geselecteerd voor:", title_text, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗ Checkbox selecteren mislukt voor:", title_text, "- Error:", e$message, "\n")
    return(FALSE)
  })
}


# Uitvoeren van de code ---------------------------------------------------

# Headless modus (browser wordt niet zichtbaar gestart)
firefox_caps <- list(
  "moz:firefoxOptions" = list(
    args = list("--headless")
  )
)

# Start RSelenium met geckodriver
rD <- rsDriver(
  browser = "firefox",
  geckover = "latest",
  chromever = NULL,
  phantomver = NULL,
  port = 4567L
  #,extraCapabilities = firefox_caps
)
# Sla het browserobject op in remDr
remDr <- rD[["client"]]

# Navigeer naar Watson database website
remDr$navigate("https://data.emissieregistratie.nl/watson")
Sys.sleep(3)  # Wacht tot pagina geladen is

# Selecteer stofgroepen
stof_element <- tryCatch({
   wait_for_element(using = 'xpath', 
                    value = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[1]/div[1]")
   element <- remDr$findElement(using = 'xpath', 
                                value = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[1]/div[1]")  # stofkeuzemenu
 }, error = function(e) NULL)
stof_element$clickElement()

# Selecteer alle stoffen in de stofgroepenlijst

# keuze lijst van stofgroepen op de watson database (htmls referen naar xpath van de checkboxen van elke stofgroep)
stoffen_keuze_lijst <- c(
"Antivlooienmiddel" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[1]/div/div[1]/label/div[1]/div",  # Antivlooienmiddel
"Bestrijdingsmiddelen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[2]/div/div[1]/label/div[1]/div",  # Bestrijdingsmiddelen
"Biociden" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[3]/div/div[1]/label/div[1]/div",  # Biociden
"# E-PRTR" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[4]/div/div[1]/label/div[1]/div",  # E-PRTR
"Geur-, kleur- en smaakstoffen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[5]/div/div[1]/label/div[1]/div", # Geur-, kleur- en smaakstoffen
"Hormonen / medicijnen voor hormoonsysteem" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[6]/div/div[1]/label/div[1]/div",  # Hormonen / medicijnen voor hormoonsysteem
"Industriële stoffen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[7]/div/div[1]/label/div[1]/div",  # Industriële stoffen  (Selecteerd ook PFAS)
"KRW aandachtstoffen (watchlist)" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[8]/div/div[1]/label/div[1]/div",  # KRW aandachtstoffen (watchlist)
"KRW Prioritaire stoffen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[9]/div/div[1]/label/div[1]/div",  # KRW Prioritaire stoffen
"KRW specifiek verontreinigende stoffen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[10]/div/div[1]/label/div[1]/div",  # KRW specifiek verontreinigende stoffen
"Medicijnen dier" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[11]/div/div[1]/label/div[1]/div",  # Medicijnen dier
"# Medicijnen mens" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[12]/div/div[1]/label/div[1]/div",  # Medicijnen mens
"Metalen en elementen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[13]/div/div[1]/label/div[1]/div",  # Metalen en elementen
"Nutriënten" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[14]/div/div[1]/label/div[1]/div",  # Nutriënten
"PAK's" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[15]/div/div[1]/label/div[1]/div",  # PAK's
# "PFAS" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[16]/div/div[1]/label/div[1]/div",  # PFAS  (zit al in industrie en zet hem nu dus uit omdat hij al aangevnkt wordt bij industrie)
"Potentieel zeer zorgwekkende stoffen" ="/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[17]/div/div[1]/label/div[1]/div",  # Potentieel zeer zorgwekkende stoffen
"Verslavende middelen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[18]/div/div[1]/label/div[1]/div",  # Verslavende middelen
"Zeer zorgwekkende stoffen" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[19]/div/div[1]/label/div[1]/div",  # Zeer zorgwekkende stoffen
"# Overig" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[20]/div/div[1]/label/div[1]/div"
)

results_all <- select_groups(stoffen_keuze_lijst)

# Select specifieke stofgroep
# selected_stoffen <- c("Antivlooienmiddel", "PFAS", "Metalen en elementen")
# result_selected <- select_stoffen_groups(stoffen_keuze_lijst, selected_stoffen)

# Selecteer RWZI's --------------------------------------------------------
RWZI_element <- tryCatch({
  RWZI_xpath <- "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[1]/div[2]"
  wait_for_element(using = 'xpath', 
                   value = RWZI_xpath)  # RWZI keuze menu
  element <- remDr$findElement(using = 'xpath', 
                               value = RWZI_xpath) # RWZI keuze menu
}, error = function(e) NULL)
RWZI_element$clickElement()

# Deselecteer alle RWZI's die standaard aangevinkt zijn
rwzi_keuze_lijst <- c(
  "EEMS" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[1]/div/div[1]/label/div[1]",
  "MAAS" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[2]/div/div[1]/label/div[1]",
  "RIJN-NOORD" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[3]/div/div[1]/label/div[1]",
  "RIJN-OOST" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[4]/div/div[1]/label/div[1]",
  "RIJN-WEST" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[5]/div/div[1]/label/div[1]",
  "SCHELDE" = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[6]/div/div[1]/label/div[1]",
  "Niet INGEDEELD/N.N.B." = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/div[7]/div/div[1]/label/div[1]"
)
select_groups(rwzi_keuze_lijst)  # deselecteer alle RWZI's (download kan dit niet aan)

# Selecteer de RWZI namen die je bij input parameters hebt opgegeven
results <- data.frame(
  rwzi_name = character(),
  search_success = logical(),
  checkbox_success = logical(),
  stringsAsFactors = FALSE
)

for (rwzi_name in RWZI_names) {
  cat("\n--- Verwerken RWZI:", rwzi_name, "---\n")
  # Zoek RWZI
  search_success <- tryCatch({
    zoekbalk <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div/input")
    zoekbalk$highlightElement()
    zoekbalk$clearElement()
    zoekbalk$sendKeysToElement(list(rwzi_name, key = "enter"))
     TRUE
  }, error = function(e) {
    cat("✗ Zoeken mislukt voor:", rwzi_name, "- Error:", e$message, "\n")
    FALSE
  })
  # Selecteer checkbox als zoeken gelukt is
  checkbox_success <- FALSE
  if (search_success) {
    checkbox_success <- select_checkbox_direct_xpath(toupper(rwzi_name))
  }
  # Resultaten opslaan
  results <- rbind(results, data.frame(
    rwzi_name = rwzi_name,
    search_success = search_success,
    checkbox_success = checkbox_success,
    stringsAsFactors = FALSE
  ))
}

for (rwzi_name in RWZI_names) {
  print(rwzi_name)
}
# Controleer of alles gelukt is
all_successful <- all(results$search_success & results$checkbox_success)
cat("\nAlle RWZI-selecties succesvol:", all_successful, "\n")

# klik op Rapportage keuze menu
rapportage_element <- tryCatch({
  rapportage_xpath <- "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[1]/div[3]"
  wait_for_element(using = 'xpath', 
                   value = rapportage_xpath) 
  element <- remDr$findElement(using = 'xpath', 
                               value = rapportage_xpath) 
}, error = function(e) NULL)
rapportage_element$clickElement()

# Selecteer influent
influent_element <- tryCatch({
  value <- 'option-INFLUENT'
  wait_for_element(using = 'id', 
                   value = value)  
  element <- remDr$findElement(using = 'id', 
                               value = value) # selecteer influent
}, error = function(e) NULL)
influent_element$clickElement()

# Selecteer periode
set_start_date_js(begin_periode)
set_end_date_js(eind_periode)

# Klik op drop-down menu van rapporteren
rapporteren_element <- tryCatch({
  value <- '/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div[6]/div[2]/div/div/p'
  wait_for_element(using = 'xpath', 
                   value = value)  
  element <- remDr$findElement(using = 'xpath', 
                               value = value)  #
}, error = function(e) NULL)
rapporteren_element$clickElement()

# Selecteer per stof per RWZI per rapportagejaar
rapporteren_element_keuze <- tryCatch({
  value <- '/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div[6]/div[2]/div/div[2]/div/div[2]/div/div[4]'
  wait_for_element(using = 'xpath', 
                   value = value)  
  element <- remDr$findElement(using = 'xpath', 
                               value = value) 
}, error = function(e) NULL)
rapporteren_element_keuze$clickElement()  # per stof per RWZI per rapportagejaar

# Rapportage keuze menu
rapportage_element <- tryCatch({
  rapportage_xpath <- "/html/body/div[2]/div[2]/div[4]/div[3]/div/div[1]/div[3]"
  wait_for_element(using = 'xpath', 
                   value = rapportage_xpath) 
  element <- remDr$findElement(using = 'xpath', 
                               value = rapportage_xpath) 
}, error = function(e) NULL)
rapportage_element$clickElement()

# Klik op exporteren keuze menu
exporteer_element <- tryCatch({
  value <- '/html/body/div[2]/div[2]/div[4]/div[3]/div/div[1]/div[4]'  
  wait_for_element(using = 'xpath', 
                   value = value)  
  element <- remDr$findElement(using = 'xpath', 
                               value = value) # 
}, error = function(e) NULL)
exporteer_element$clickElement() #  klik op exporteer

# Klik op Download
download_element <- tryCatch({
  value <- '/html/body/div[2]/div[2]/div[4]/div[3]/div/div[2]/div[1]/a/div'  
  wait_for_element(using = 'xpath', 
                   value = value)  
  element <- remDr$findElement(using = 'xpath', 
                               value = value) # 
}, error = function(e) NULL)
download_element$clickElement() #  klik op download

# Wacht op download, code wacht 15 seconden om te kijken of bestand er al is.
Sys.sleep(15)
cat("Code wacht 15 seconden op downloaden van bestand voordat connectie wordt gesloten\nJe kunt het bestand vinden in je standaard downloadlocatie, vaak is dit 'Downloads'\n")
# Sluit Selenium netjes af na download
# P.S. het is mij nog niet gelukt automatisch het bestand in de folder Watson_download te downloaden.
remDr$close()
rD$server$stop()

output_folder <- "data/02_modified/Emissieregistratie/Watson_download"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

cat("Plaats het Watson-download Excelbestand in de folder 'data/02_modified/Emissieregistratie/Watson_download'\n voordat je verder gaat met script 06")

# einde --------------------------------------------------------------------
