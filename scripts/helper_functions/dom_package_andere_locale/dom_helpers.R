#' Check domeintabelnamen
#'
#' Checkt of een namen geldige domeintabelnamen zijn.
#'
#' @param namen Character vector met namen van domeintabellen.
#'
#' @return Logical vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' is_domeintabel("MonsterType")
#' is_domeintabel("Domeintabel")
#'
#' }
is_domeintabel <- function(namen){
  overzicht <- dom_overzicht(peildatum = NULL)
  stringr::str_to_lower(namen) %in% stringr::str_to_lower(overzicht$domeintabel)
}

#' Guid van domeintabel
#'
#' Zoek de guid van domeintabellen op.
#'
#' @param namen Character vector met namen van domeintabellen.
#'
#' @return Vector met guid's
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dom_guid("MonsterType")
#'
#' }
dom_guid <- function(namen){
  
  namen <- dom_convert_case(namen)
  
  overzicht <- dom_overzicht(peildatum = NULL)
  tibble::tibble(namen = namen) %>%
    dplyr::left_join(overzicht, by = c("namen" = "domeintabel")) %>%
    dplyr::pull(guid) %>%
    unname()
}


#' Kolommen van een domeintabel
#'
#' Deze functie zoekt op welke kolommen een domeintabel heeft.
#'
#' @param naam Naam van een domeintabel
#'
#' @return Een vector met kolomnamen
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dom_kolommen("MonsterType")
#'
#' }
dom_kolommen <- function(naam){
  if (length(naam) != 1) stop("`naam` dient een vector met lengte 1 te zijn")
  
  if (!is_domeintabel(naam)) stop(paste(naam, "is geen geldige domeintabelnaam"))
  
  overzicht <- dom_overzicht(peildatum = NULL)
  
  naam <- dom_convert_case(naam)
  
  overzicht %>%
    dplyr::filter(domeintabel == naam) %>%
    dplyr::pull(kolommen) %>%
    .[[1]]
}

#' Maak een URL naar de csv domeintabel
#'
#' Deze functie genereert de juiste URL van een domeintabel voor de functie `dom()`
#'
#' @param naam Naam van de domeintabel
#' @param limit Aantal waarden per keer
#' @param offset Startpunt
#'
#' @return Een string met de URL
#'
#' @noRd
create_dom_url <- function(naam, limit = 500, offset = 0){
  
  kolomstring <- dom_kolommen(naam) %>%
    stringr::str_replace(" ", "+") %>%
    paste(collapse = "%0D%0A?")
  
  glue::glue(
    "https://www.aquo.nl/index.php?title=Speciaal:Vragen&q=+[[Breder::{dom_guid(naam)}]]",
    "%0D%0A&po=?{kolomstring}%0D%0A",
    "&p[format]=csv&p[sep]=;&p[limit]={limit}&p[offset]={offset}"
  )
}

#' Conversie van hoofdletters van domeintabel namen
#'
#' @param namen
#'
#' @return `namen` met de juiste hoofdletters.
#'
#' @noRd
#'
dom_convert_case <- function(namen) {
  
  opzoektabel <-
    dom_overzicht(peildatum = NULL) %>%
    dplyr::mutate(lower = stringr::str_to_lower(domeintabel)) %>%
    dplyr::select(lower, domeintabel) %>%
    tibble::deframe()
  
  unname(opzoektabel[as.character(stringr::str_to_lower(namen))])
}