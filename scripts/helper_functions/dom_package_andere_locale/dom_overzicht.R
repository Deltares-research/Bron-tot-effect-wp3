

dom_overzicht <- function(peildatum = Sys.Date()){
  
  dom_overzicht_m <- memoise::memoise(dom_overzicht_basis,
                                      cache = cachem::cache_disk(dir = tempdir()))
  
  overzicht <- suppressWarnings(dom_overzicht_m())
  
  if (!is.null(peildatum)) {
    if (class(peildatum) != "Date") {peildatum <- lubridate::as_date(peildatum)}
    overzicht <- overzicht %>% dplyr::filter(begin_geldigheid <= peildatum, eind_geldigheid >= peildatum)
  }
  return(overzicht)
}

dom_overzicht_basis <- function() {
  
  url <- "https://www.aquo.nl/index.php?title=Speciaal:Vragen&x=[[Elementtype::Domeintabel%20%7C%7C%20Domeintabeltechnisch%20%7C%7C%20Domeintabelverzamellijst]]%20/?Elementtype/?Voorkeurslabel/?Metadata/?Wijzigingsdatum/?Begin%20geldigheid/?Eind%20geldigheid/&format=csv&sep=;&offset=0&limit=500"
  
  req <- httr::GET(url)
  
  if (req$status_code != 200 || length(req$content) == 0) {
    message("Geen domeintabellen gevonden")
    return(NULL)
  }
  
  datum_formats <- c("dBY HMS", "dBY", "dBY")
  lokale <- "Dutch_Netherlands.1252"
  
  overzicht <- req %>%
    httr::content(as = "text") %>%
    readr::read_csv2(locale = readr::locale(decimal_mark = ",", grouping_mark = ".")) %>%
    dplyr::rename_with(.fn = ~"guid", .cols = dplyr::any_of(c("X1", "...1"))) %>% # ivm verschillen in readr versies
    dplyr::select(domeintabel = Voorkeurslabel,
                  domeintabelsoort = Elementtype,
                  wijzigingsdatum = Wijzigingsdatum,
                  begin_geldigheid = `Begin geldigheid`,
                  eind_geldigheid = `Eind geldigheid`,
                  kolommen = Metadata,
                  guid) %>% dplyr::mutate(dplyr::across(.cols = c(wijzigingsdatum, begin_geldigheid, eind_geldigheid),
                                .fns = ~lubridate::as_date(lubridate::parse_date_time(.x, orders = datum_formats, 
                                                                                      locale = lokale)))
    ) %>% dplyr::mutate(kolommen = stringr::str_split(kolommen, ","))
  
  return(overzicht)
}


