#!/usr/bin/env Rscript
# Wrapper om alle 8 scripts sequentieel uit te voeren en te loggen

scripts <- c(
  "scripts/01_maak_SBI_ZZS_koppeltabel.R",
  "scripts/02_koppel_ZZS_aan_Bedrijvenopkaart_data.R",
  "scripts/03_ZZS_aquocodes.R",
  "scripts/04_ER_filter_on_shapefile.R",
  "scripts/05_ER_watson_script.R",
  "scripts/06_verwerking_ER_en_Watson_script.R",
  "scripts/07_samenvoegen_zzs_ER.R",
  "scripts/08_voeg_molecuulinfo_toe.R"
)

logdir <- "logs"
if (!dir.exists(logdir)) dir.create(logdir, recursive = TRUE)
logfile <- file.path(logdir, "run_all.log")

writeLines(paste0("Run started: ", Sys.time()), logfile)

# Controleer command-line args & env var voor continue-on-error
args <- commandArgs(trailingOnly = TRUE)
continue_on_error <- FALSE
if (any(grepl("--continue-on-error", args, ignore.case = TRUE))) continue_on_error <- TRUE
env_coe <- Sys.getenv("CONTINUE_ON_ERROR", unset = "")
if (env_coe %in% c("1", "TRUE", "true", "True")) continue_on_error <- TRUE

failures <- list()

for (s in scripts) {
  header <- paste0("\n\n==== Running: ", s, " (", Sys.time(), ") ====\n")
  writeLines(header, logfile, append = TRUE)
  if (!file.exists(s)) {
    msg <- paste0("Script niet gevonden: ", s, " - ")
    writeLines(paste0(msg, "\n"), logfile, append = TRUE)
    failures[[s]] <- msg
    if (!continue_on_error) stop(paste0(msg, "Pipeline stopt.")) else next
  }

  # Voer script uit en vang output/errors op
  out <- tryCatch({
    res <- system2("Rscript", args = s, stdout = TRUE, stderr = TRUE)
    status <- attr(res, "status")
    if (is.null(status)) status <- 0
    writeLines(res, logfile, append = TRUE)
    writeLines(paste0("Exit status: ", status), logfile, append = TRUE)
    if (status != 0) {
      msg <- paste0("Script retourneerde status ", status)
      writeLines(paste0("ERROR: ", msg), logfile, append = TRUE)
      stop(msg)
    }
    TRUE
  }, error = function(e) {
    writeLines(paste0("ERROR: ", e$message), logfile, append = TRUE)
    failures[[s]] <- e$message
    FALSE
  })

  if (!isTRUE(out)) {
    if (!continue_on_error) {
      writeLines(paste0("Stopping pipeline due to error at ", Sys.time()), logfile, append = TRUE)
      stop("Pipeline gestopt wegens fout. Zie logs/run_all.log voor details.")
    } else {
      writeLines(paste0("Continuing pipeline despite error (continue-on-error=TRUE)."), logfile, append = TRUE)
      next
    }
  }
}

# Samenvatting
writeLines(paste0("\nRun finished: ", Sys.time()), logfile, append = TRUE)
if (length(failures) > 0) {
  writeLines("\nSamenvatting van fouten:", logfile, append = TRUE)
  for (n in names(failures)) {
    writeLines(paste0(n, " : ", failures[[n]]), logfile, append = TRUE)
  }
  cat("Run voltooid met fouten. Zie ", logfile, " voor details.\n")
} else {
  cat("Run voltooid zonder fouten. Zie ", logfile, " voor details.\n")
}

