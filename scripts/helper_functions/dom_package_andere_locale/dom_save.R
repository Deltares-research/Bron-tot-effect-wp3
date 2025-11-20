#' Opslaan domeintabel
#'
#' Met deze functie is het mogelijk om een domeintabel op te slaan als .xlsx-bestand of als .csv-bestand.
#'
#' @inheritParams dom
#'
#' @param bestandsnaam Naam van het bestand om op te slaan. Eventueel als volledig pad. Default is de datum
#' gevolgd door de naam van de domeintabel. De bestandsnaam mag zonder extensie worden opgegeven.
#' @param map Naam van de map. De map moet bestaan. Optioneel.
#' @param bestandstype "xlsx" of "csv"  Opslaan als xlsx of als csv-bestand. Default is xlsx.
#'
#' @return Slaat de domeintabel op de schijf. De domeintabel zelf wordt onzichtbaar geretourneerd.
#'
#' @details
#' Voor het opslaan van een bestand als .xlsx wordt [openxlsx::write.xlsx()] gebruikt. Voor het opslaan
#' als .csv wordt [readr::write_csv2()] gebruikt.
#'
#' Opgeslagen excelbestanden kunnen ingelezen worden met [openxlsx::read.xlsx()] of [readxl::read_excel()].
#' csv-bestanden kunnen worden ingelezen met [readr::read_csv2()]
#'
#' @family domeintabellen
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  dom_save("MonsterType")
#'  dom_save("MonsterType", bestandsnaam = "test.csv", map = "data", bestandstype = "csv")
#'
#' }
dom_save <- function(naam,
                     bestandsnaam = paste(Sys.Date(), naam),
                     map = NULL,
                     bestandstype = c("xlsx", "csv"),
                     peildatum = Sys.Date()) {
  
  bestandstype <- bestandstype[[1]]
  rlang::arg_match(bestandstype, c("xlsx", "csv"))
  
  dom_tabel <- dom(naam, peildatum)
  
  if (bestandstype == "xlsx") {
    bestandsnaam <- bestandsnaam %>% stringr::str_remove(".xlsx$") %>% stringr::str_c(".xlsx")
    if (!is.null(map)) bestandsnaam <- file.path(map, bestandsnaam)
    
    openxlsx::write.xlsx(dom_tabel, file = bestandsnaam)
  }
  
  if (bestandstype == "csv") {
    bestandsnaam <- bestandsnaam %>% stringr::str_remove(".csv$") %>% stringr::str_c(".csv")
    if (!is.null(map)) bestandsnaam <- file.path(map, bestandsnaam)
    
    readr::write_csv2(dom_tabel, path = bestandsnaam)
  }
  
  invisible(dom_tabel)
  
}

# dom_save("MonsterType", "DEV/test.xlsx", bestandstype = "xlsx")
# x <- dom_save("MonsterType", "DEV/test.xlsx")
# file.remove("DEV/test.xlsx")
#
# dom_save("MonsterType", "DEV/test.csv", "csv", "20000101")
# file.remove("DEV/test.csv")
# dom_save("MonsterType")
# file.remove(paste(Sys.Date(),"MonsterType.xlsx"))
# dom_save("MonsterType", bestandstype = "csv")
# file.remove(paste(Sys.Date(),"MonsterType.csv"))
# dom_save("MonsterType", map = "DEV")
# file.remove(file.path("DEV", paste(Sys.Date(),"MonsterType.xlsx")))
#
# naam <- "MonsterType"