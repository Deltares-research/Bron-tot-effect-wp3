#' Opvragen domeintabel
#'
#' Deze functie haalt een domeintabel op van www.aquo.nl.
#'
#' @param naam Naam van een domeintabel - De namen zijn niet hoofdlettergevoelig. Zie
#'   `dom_overzicht()` voor geldige domeintabelnamen.
#' @param peildatum Date of een character die omgezet kan worden in een Date met
#'   `lubridate::as_date()`. De peildatum filtert de output om alleen geldige
#'   domeinwaarden op de peildatum weer te geven. Gebruik `NULL` om alle
#'   domeinwaarden ongeacht de geldigheid weer te geven.
#'
#' @section Caching: Deze functie maakt gebruik van caching voor het
#'   optimaliseren van snelheid en om de aquo-server niet onnodig te belasten.
#'   Hiervoor wordt de map `tempdir()` gebruikt als cache. Deze map wordt na
#'   elke R-sessie verwijderd.
#'
#' @return Een tibble met een met domeinwaarden. De kolommen zijn afhankelijk
#'   van de betreffende domeintabel.
#'
#' @family domeintabellen
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dom("MonsterType")
#' dom("MonsterType", peildatum = Sys.Date())
#' dom("MonsterType", peildatum = "2021-04-05")
#'
#' }
dom <- function(naam, peildatum = Sys.Date()) {
  
  if (length(naam) != 1) stop("`naam` dient een vector met lengte 1 te zijn")
  if (!is_domeintabel(naam)) stop(paste(naam, "is geen geldige domeintabelnaam"))
  
  naam <- dom_convert_case(naam)
  
  dom_m <- memoise::memoise(dom_basis, cache = cachem::cache_disk(dir = tempdir()))
  
  domeintabel <- suppressWarnings(dom_m(naam))
  
  if (!is.null(peildatum)) {
    if (!"begin_geldigheid" %in% names(domeintabel) | !"eind_geldigheid" %in% names(domeintabel)) {
      stop("Voor deze tabel is geen begin_geldigheid of eind_geldigheid beschikbaar")
    }
    if (class(peildatum) != "Date") {peildatum <- lubridate::as_date(peildatum)}
    domeintabel <- domeintabel %>% dplyr::filter(begin_geldigheid <= peildatum, eind_geldigheid >= peildatum)
    
  }
  
  return(domeintabel)
}

dom_basis <- function(naam){
  
  lokale <- "Dutch_Netherlands.1252"
  limit <- 500
  offset <- 0
  res <- tibble::tibble()
  while(TRUE){ # break out with break
    
    dom_deel_url <- create_dom_url(naam, limit, offset)
    
    deel_res <-
      dom_deel_url %>%
      httr::GET() %>%
      httr::content(as = "text") %>%
      readr::read_csv2(locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
    
    res <- dplyr::bind_rows(res, deel_res)
    
    if (nrow(deel_res) < limit) break()
    
    offset <- offset + limit
    cat(".")
  }
  cat("\n")
  
  datum_formats <- c("dBY HMS", "dBY", "dmY HMS", "Ymd HMS", "Ymd", "dmY")
  
  res <-
    res %>%
    dplyr::rename_with(.fn = ~"guid", .cols = dplyr::any_of(c("...1", "X1"))) %>%
    dplyr::rename_with(.fn = function(x) stringr::str_replace(x, pattern = " ", "_")) %>%
    dplyr::rename_with(.fn = stringr::str_to_lower) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::contains(c("geldigheid", "datum")),
                                .fns = ~lubridate::as_date(lubridate::parse_date_time(.x, orders = datum_formats,
                                                                                      locale = lokale)))
    ) %>%
    dplyr::relocate(dplyr::any_of(c("id", "codes", "cijfercode", "omschrijving", "begin_geldigheid", "eind_geldigheid")))
  
  return(res)
}