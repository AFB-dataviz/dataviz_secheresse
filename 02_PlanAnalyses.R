# SETUP -------------------------------------------------------------------

# Préparation des packages utilisés ---------------------------------------
source("scripts/000_check_and_install.R")

check_and_install(pkgs = c("dplyr", "lubridate", "purrr", "vroom", "glue", "plotly", "stringr", "tidyr", "forcats", "ggplot2", "progress", "paletteer", "drake", "visNetwork", "future"))

'%>%' <- dplyr::'%>%'

## Chargement des scripts personnels ---------------------------------------

purrr::walk(
  list.files(path       = here::here("scripts"),
             pattern    = ".R",
             full.names = TRUE),
  function(f) {
    source(f, encoding = "UTF-8")
  }
)


# Définition du plan d'analyse --------------------------------------------

PlanAnalyses <- drake::drake_plan(
  
  ## Import des données ---------------------------------------
  DonneesOnde = vroom::vroom(file   = drake::file_in(here::here("data/prepared/DonneesOnde.csv")),
                             locale = vroom::locale(decimal_mark = ",")) %>% 
    dplyr::mutate(label_observation_national = factor(
      label_observation_national,
      levels = c("Observation impossible",
                 "Ecoulement visible",
                 "Ecoulement non visible",
                 "Assec")
    ),
    nom_bassin = factor(nom_bassin,
                        levels = c("ARTOIS-PICARDIE", "SEINE-NORMANDIE",
                                   "LOIRE-BRETAGNE", "RHIN-MEUSE",
                                   "ADOUR-GARONNE", "RHONE-MEDITERRANEE-CORSE")),
    mois_observation = factor(mois_observation,
                              levels = c("janvier", "fevrier", "mars",
                                         "avril", "mai", "juin",
                                         "juillet", "aout", "septembre",
                                         "octobre", "novembre", "decembre"),
                              ordered = TRUE)),
  
  ## Evolution temporelle des modalités d'écoulement -------------------------
  ModalitesEcoulement = plot_flow_status(
    data    = DonneesOnde,
    palette = c("#70a4a6", "#F08B36", "#870f0f"),
    width   = 550, 
    height  = 600
  ) %>% 
    plotly::layout(
      margin = list(
        t          = 40, 
        r          = 0, 
        l          = 35,
        b          = 45, 
        pad        = 0, 
        autoexpand = FALSE
        )
    ),
  
  EnregistrementModalitesEcoulement = save_plotly(
    ModalitesEcoulement, 
    file        = "outputs/01_ModalitesEcoulement.html",
    file_static = "outputs/img/01_ModalitesEcoulement.png",
    title       = "Evolution des modalites d'ecoulement par bassin hydrographique"
  ),
  
  ## Cartographie dynamique des assecs ---------------------------------------
  CarteAssecs = plot_drought_map(
    data    = DonneesOnde,
    palette = c("#269ED2", "#EEC900", "#F08B36", "#D3313F"),
    geojson = here::here("data/raw/contour-des-departements.geojson"),                             
    width   = 750, 
    height  = 600,
    zoom    = 4.3) %>% 
    plotly::layout(
      margin = list(
        t          = 40, 
        r          = 110,
        l          = 10,
        b          = 120,
        pad        = 0,
        autoexpand = FALSE
        )
    ),
  
  EnregistrementCarteAssecs = save_plotly(
    CarteAssecs,
    file        = "outputs/02_CarteAssecs.html",
    file_static = "outputs/img/02_CarteAssecs.png",
    title       = "Evolution annuelle de la prevalence departementale des assecs"
  ),
  
  # Dans le fichier HTML, remplacer `Part des sites<br>en assec (%)` par
  # Part des sites<br>en assec (%)
  
  ## Saisonnalité des assecs -------------------------------------------------
  SaisonnaliteAssecs = plot_drought_seasons(
    data       = DonneesOnde,
    palette    = c("#269ED2", "#EEC900", "#F08B36"),
    fill_break = 10, 
    fill_max   = 40,
    width      = 600, 
    height     = 600
  ) %>% 
    plotly::layout(
      margin = list(
        t          = 40,
        r          = 105,
        l          = 80,
        b          = 25,
        pad        = 0,
        autoexpand = FALSE
        )
    ),
  
  EnregistrementSaisonnaliteAssecs = save_plotly(
    SaisonnaliteAssecs,
    file        = "outputs/03_SaisonnaliteAssecs.html",
    file_static = "outputs/img/03_SaisonnaliteAssecs.png",
    title       = "Saisonnalite des assecs au niveau national"
  ),
  
  
  ## Récurrence mensuelle des assecs -----------------------------------------
  DonneesRecurrenceMensuelle = DonneesOnde                       %>%
    dplyr::mutate(mois_num = lubridate::month(date_observation)) %>%
    calc_state_duration(
      id    = code_site,
      state = label_observation_national,
      time  = mois_num,
      nom_bassin, nom_site, annee_observation
    ),
  
  RecurrenceMensuelleAssecs = plot_drought_duration(
    data    = DonneesRecurrenceMensuelle,
    palette = c("#B7D0A2", "#EEC900", "#F08B36", "#870f0f"),
    x_break = 2,
    y_break = .1,
    width   = 550,
    height  = 700
  ) %>% 
    plotly::layout(
      legend = list(
        x       = 0, 
        xanchor = "left",
        y       = -.18, 
        yanchor = "bottom"
      ),
      margin = list(
        t          = 45, 
        r          = 0, 
        l          = 30,
        b          = 100, 
        pad        = 0, 
        autoexpand = FALSE)
    ),
  
  EnregistrementRecurrencesMensuelles = save_plotly(
    RecurrenceMensuelleAssecs,
    file        = "outputs/04_RecurrenceMensuelleAssecs.html",
    file_static = "outputs/img/04_RecurrenceMensuelleAssecs.png",
    title       = "Recurrence mensuelle des assecs par bassin hydrographique"
  ),
  
  ## Récurrence annuelle des assecs ------------------------------------------
  
  RecurrenceAnnuelleAssecs = plot_drought_return(
    data    = DonneesOnde,
    palette = paletteer::paletteer_d(palette   = "RColorBrewer::Spectral", 
                                     n         = 9, 
                                     direction = -1) %>% 
      as.vector(),
    width  = 700,
    height = 700
  ) %>% 
    plotly::layout(
      margin = list(
        t          = 40,
        r          = 0,
        l          = 0,
        b          = 40, 
        pad        = 0,
        autoexpand = FALSE)
    ),
  
  EnregistrementRecurrencesAnnuelles = save_plotly(
    RecurrenceAnnuelleAssecs,
    file        = "outputs/05_RecurrenceAnnuelleAssecs.html",
    file_static = "outputs/img/05_RecurrenceAnnuelleAssecs.png",
    title       = "Recurrence annuelle des assecs"
  )
  
)
