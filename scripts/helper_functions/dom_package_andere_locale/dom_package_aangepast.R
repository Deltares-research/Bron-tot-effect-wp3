# laad alle andere files van dom package maar dan met kleine aanpassing dat 
# locale altijd dutch is onafhankelijk van je eigen computer instellingen

source("scripts/helper_functions/dom_package_andere_locale/dom.R")  # overwrite package zodat locale werkt
source("scripts/helper_functions/dom_package_andere_locale/dom_overzicht.R") # overwrite package functie zodat locale werkt
source("scripts/helper_functions/dom_package_andere_locale/dom_save.R")
source("scripts/helper_functions/dom_package_andere_locale/dom_helpers.R")
source("scripts/helper_functions/dom_package_andere_locale/zz_globals.R")