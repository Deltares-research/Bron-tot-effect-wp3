# SPOT_B2E - Suspectlist generatie

Dit project bevat R-scripts om op basis van ZZS- en Emissieregistratie-data een "suspectlist" te genereren. Samen vormen de scripts de Stoffen Prioritering en Opsporing Tool (SPOT). Volg de acht scripts in de `scripts/` map in de aangegeven volgorde om van ruwe data tot een samengestelde suspectlist te komen.
Meer informatie over de gebruikte methodiek is te vinden in het onderstaande rapport
https://publications.deltares.nl/11208834_003_0001.pdf 

**Belangrijk:** de beschrijving hieronder is in het Nederlands.

**Vereisten:**
- R (gebruik bij voorkeur RStudio voor gemakkelijke uitvoering)
- De R-packages die in de scripts worden geladen. Zie onderstaande sectie voor de exacte lijst.

**Projectstructuur (relevant):**
- `scripts/` : de acht hoofd-scripts (01–08) die de workflow uitvoeren
- `helper_functions/` : hulpfuncties en aangepaste packages die door de scripts gebruikt worden
- `data/01_raw/` : ruwe invoerbestanden (ZZS, Emissieregistratie, koppeltabellen, enz.)
- `data/02_modified/` : bewerkte bestanden en shapefiles (bv. `rwzi_shapes/`)
- `data/03_output/` : uiteindelijke outputbestanden, waaronder de suspectlist(en)

**Workflow (volgorde van scripts):**

- `01_maak_SBI_ZZS_koppeltabel.R` :
  - Maakt een koppeltabel tussen SBI-codes en ZZS-codes/klassen.

- `02_koppel_ZZS_aan_Bedrijvenopkaart_data.R` :
  - Koppelt ZZS-informatie aan de bedrijven/kaart-data zodat locaties gekoppeld zijn aan ZZS-codes.

- `03_ZZS_aquocodes.R` :
  - Haalt en verwerkt aquo-/aquacodes uit de ZZS-data (voor water-gerelateerde classificaties).

- `04_ER_filter_on_shapefile.R` :
  - Filtert Emissieregistratie (ER) records op basis van shapefiles (bv. RWZI-contouren) om relevante ER-records te selecteren.

- `05_ER_watson_script.R` :
  - Ophalen/verwerken van Watson/ER-influent data (webscraping of manuele download via Watson).

- `06_verwerking_ER_en_Watson_script.R` :
  - Verdere verwerking en integratie van ER- en Watson-data; zet data klaar voor samenvoeging.

- `07_samenvoegen_zzs_ER.R` :
  - Voegt de verwerkte ZZS-data en ER-data samen tot één dataset met relevante velden voor de suspectlist.

- `08_voeg_molecuulinfo_toe.R` :
  - Verrijkt de gecombineerde dataset met molecuulinformatie (PubChem) en maakt de uiteindelijke compoundlist/suspectlist.

**Benodigde R-packages (geïdentificeerd uit de scripts)**
- `tidyverse` (dplyr, tidyr, ggplot2, purrr, readr, stringr, etc.)
- `vroom`
- `readxl`
- `writexl`
- `tidygeocoder`
- `sf`
- `mapview`
- `eks`
- `maptiles`
- `ggspatial`
- `viridis`
- `arrow`
- `glue`
- `httr`
- `RSelenium`
- `rJavaEnv`
- `webchem`
- `here`

Opmerking: sommige functionaliteit wordt geleverd via lokale helper-scripts in `helper_functions/` (bv. een aangepaste `dom_package_aangepast.R` voor aquodom). Deze bestanden zijn onderdeel van de repo.

Installatie voorbeeld (CRAN):

```r
install.packages(c(
  "tidyverse", "vroom", "readxl", "writexl", "tidygeocoder",
  "sf", "mapview", "eks", "maptiles", "ggspatial", "viridis",
  "arrow", "glue", "httr", "RSelenium", "rJavaEnv", "webchem", "here"
))
```

Let op:
- `sf` en `mapview` vereisen systeemafhankelijke bibliotheken (GDAL, GEOS, PROJ). Op Windows kun je de binaire versies gebruiken of de instructies op https://r-spatial.github.io/sf/#installing volgen.
- `RSelenium` vereist een browserdriver (bijv. `geckodriver` voor Firefox) en een geschikte browser (Firefox/Chrome). Zie de `RSelenium`-documentatie voor setup.

**Hoe te gebruiken (kort voorbeeld):**

1. Open het project in RStudio of start een R-session in de projectroot.
2. Installeer de benodigde packages zoals hierboven.
3. Voer de scripts één voor één uit in dezelfde volgorde (01 → 08). In de terminal of PowerShell kun je ook `Rscript` gebruiken:

```powershell
Rscript scripts/01_maak_SBI_ZZS_koppeltabel.R
Rscript scripts/02_koppel_ZZS_aan_Bedrijvenopkaart_data.R
# ... enzovoort tot
Rscript scripts/08_voeg_molecuulinfo_toe.R
```

4. Of gebruik de wrapper `scripts/run_all.R` (beschrijving hieronder).

5. Controleer na afloop de `data/03_output/` en `data/02_modified/` mappen voor gegenereerde suspectlist(en) en tussenproducten.

**Automatisch uitvoeren (wrapper)**
Er is een wrapper toegevoegd die de 8 scripts sequentieel uitvoert en uitvoer/logs naar `logs/run_all.log` schrijft.

Voer de wrapper uit in PowerShell of een terminal:

```powershell
Rscript scripts/run_all.R
```

Of in R:

```r
source("scripts/run_all.R")
# of: system("Rscript scripts/run_all.R")
```

De wrapper stopt bij een fout en schrijft zowel standaardoutput als fouten naar het logbestand.

**Windows-specifieke installatie (kort)**
Als je op Windows werkt, zijn er een paar extra stappen die vaak nodig zijn om packages als `sf` en `RSelenium` goed te laten werken.

- RTools (build tools): Installeer RTools (voor R < 4.3) of zorg dat je recente R-versie en bijbehorende tools hebt. Download: https://cran.r-project.org/bin/windows/Rtools/
- `sf` en systeembibliotheken: veel Windows-gebruikers kunnen `sf` direct installeren met `install.packages("sf")`. Als er fouten optreden over GDAL/PROJ/GEOS, installeer de binaire versies of gebruik een package manager zoals `conda` of `choco` om GDAL/PROJ te installeren.

  Voorbeeld (PowerShell met Chocolatey):

  ```powershell
  choco install geospatial-gdal
  choco install gdal
  # daarna in R
  install.packages("sf")
  ```

- `RSelenium` en geckodriver:
  - Installeer een recente Firefox-browser.
  - Zet `geckodriver` in je PATH. Je kunt `geckodriver` downloaden van https://github.com/mozilla/geckodriver/releases en het exe-bestand in een folder plaatsen die in je `%PATH%` staat.
  - Als je Chocolatey gebruikt:

  ```powershell
  choco install geckodriver
  ```

  - Alternatief in R: gebruik `wdman` / `binman` (niet altijd stabiel op Windows). Zie `RSelenium` documentatie.

- Java voor `rJavaEnv`/`RSelenium`: installeer een JDK (11 of nieuwer) en stel `JAVA_HOME` in. Voor PowerShell:

  ```powershell
  # voorbeeld: als JDK in C:\Program Files\Java\jdk-11.0.XX staat
  $env:JAVA_HOME = 'C:\Program Files\Java\jdk-11.0.XX'
  [Environment]::SetEnvironmentVariable('JAVA_HOME', $env:JAVA_HOME, 'User')
  ```

**Wrapper: continue-on-error gebruik**
Je kunt de wrapper instructies geven om door te gaan bij fouten met een command-line vlag of env var:

PowerShell:

```powershell
# Met vlag
Rscript scripts/run_all.R --continue-on-error

# Of via environment variable
$env:CONTINUE_ON_ERROR = '1'
Rscript scripts/run_all.R
```

De wrapper logt fouten en geeft aan welke scripts gefaald hebben; aan het eind is er een korte samenvatting in `logs/run_all.log`.

**Tips en aantekeningen:**
- Lees de header/config-variabelen bovenaan elk script; daar staan vaak paden en filteropties die je kunt aanpassen.
- De scripts gebruiken code uit `helper_functions/` — wijzig of update die bestanden alleen wanneer je zeker weet wat je doet.
- Als je grote delen van de pipeline wilt herhalen of automatiseren, gebruik dan de wrapper of schrijf een extra scheduler/CI wrapper.

---
