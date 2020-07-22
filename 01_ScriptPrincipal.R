# SETUP -------------------------------------------------------------------

## Préparation des packages utilisés ---------------------------------------

source("scripts/000_check_and_install.R")
check_and_install(pkgs = c("checkpoint", "renv"))

if (!dir.exists("renv")) {
    checkpoint::setSnapshot(snapshotDate = "2020-06-20")
    renv::init(bare = TRUE, restart = FALSE)
}

check_and_install(pkgs = c("checkpoint", "renv",  "visNetwork", "future", "here", "drake", "dplyr", "purrr", "vroom", "lubridate", "tibble", "readr", "plotly", "stringr", "tidyr", "forcats", "ggplot2", "paletteer", "rjson", "rmapshaper"))


## Chargement des scripts personnels ---------------------------------------

source("scripts/000_onde_utils.R")
source("scripts/001_prepare_data.R")

'%>%' <- dplyr::'%>%'

# Téléchargement des fichiers bruts de données -------------------------------
TelechargerOnde <- TRUE

if (TelechargerOnde) {
    DataDir = here::here("data/raw/exports")

    download_onde(years = c(2012:2019), dir = DataDir) 
    
    prepare_data(data_dir = DataDir,
                 campaign = "usuelle") %>% 
        readr::write_csv2(here::here("data/prepared/DonneesOnde.csv"))
}

# Chargement et visualisation du plan d'analyses -------------------------
source(here::here("02_PlanAnalyses.R"))
drake::vis_drake_graph(PlanAnalyses)

# Exécution du plan d'analyse --------------------------------------------------
drake::r_make()

renv::snapshot()
