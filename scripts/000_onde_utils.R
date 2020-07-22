download_onde <- function(years, dir = getwd()) {
  require(purrr)
  require(utils)
  
  base_url <- "https://onde.eaufrance.fr/sites/default/files/fichiers-telechargeables/onde_france_"

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  purrr::walk(
    years,
    function(year) {
      temp_file <- file.path(dir, paste0("onde_france_", year, ".zip"))

      utils::download.file(
        url      = paste0(base_url, year, ".zip"),
        destfile = temp_file,
        mode     = "wb"
      )
    }
  )
}

import_onde <- function(dir) {
  require(dplyr)
  require(purrr)
  require(utils)
  require(vroom)
  
  '%>%' <- dplyr::'%>%'
  
  temp_dir <- tempfile()

  files    <- list.files(path       = dir,
                         pattern    = ".zip",
                         full.names = TRUE)

  purrr::walk(
    files,
    function(x) {
      utils::unzip(
        zipfile = x,
        exdir   = temp_dir
      )
    }
  )

  data_years <- list.files(path       = temp_dir, 
                           pattern    = ".csv",
                           full.names = TRUE) %>% 
    vroom::vroom(
      file      = .,
      col_types = list(
        "<CdSiteHydro>"             = vroom::col_character(),
        "<LbSiteHydro>"             = vroom::col_character(),
        "<Annee>"                   = vroom::col_integer(),
        "<TypeCampObservations>"    = vroom::col_character(),
        "<DtRealObservation>"       = vroom::col_date(format = "%Y-%m-%d"),
        "<LbRsObservationDpt>"      = vroom::col_character(),
        "<RsObservationDpt>"        = vroom::col_character(),
        "<LbRsObservationNat>"      = vroom::col_character(),
        "<RsObservationNat>"        = vroom::col_number(),
        "<NomEntiteHydrographique>" = vroom::col_character(),
        "<CdTronconHydrographique>" = vroom::col_character(),
        "<LbCommune>"               = vroom::col_character(),
        "<CdCommune>"               = vroom::col_character(),
        "<CdDepartement>"           = vroom::col_character(),
        "<LbRegion>"                = vroom::col_character(),
        "<NomCircAdminBassin>"      = vroom::col_character(),
        "<CoordXSiteHydro>"         = vroom::col_number(),
        "<CoordYSiteHydro>"         = vroom::col_number(),
        "<ProjCoordSiteHydro>"      = vroom::col_character(),
        "FLG"                       = vroom::col_character()
    )  
  )

  unlink(temp_dir, recursive = TRUE)

  data_years
}

clean_onde <- function(data_onde) {
  require(dplyr)
  require(lubridate)
  require(stringr)
  
  '%>%' <- dplyr::'%>%'
  
  data_onde %>%
    dplyr::rename(
      .data                         = .,
      code_site                     = `<CdSiteHydro>`,
      nom_site                      = `<LbSiteHydro>`,
      annee_observation             = `<Annee>`,
      type_observation              = `<TypeCampObservations>`,
      date_observation              = `<DtRealObservation>`,
      label_observation_departement = `<LbRsObservationDpt>`,
      code_observation_departement  = `<RsObservationDpt>`,
      label_observation_national    = `<LbRsObservationNat>`,
      code_observation_national     = `<RsObservationNat>`,
      nom_entite_hydro              = `<NomEntiteHydrographique>`,
      code_entite_hydro             = `<CdTronconHydrographique>`,
      nom_commune                   = `<LbCommune>`,
      code_commune                  = `<CdCommune>`,
      code_departement              = `<CdDepartement>`,
      nom_region                    = `<LbRegion>`,
      nom_bassin                    = `<NomCircAdminBassin>`,
      coord_x                       = `<CoordXSiteHydro>`,
      coord_y                       = `<CoordYSiteHydro>`,
      coord_projection              = `<ProjCoordSiteHydro>`
    ) %>%
    dplyr::mutate(
      .data                      = .,
      nom_bassin                 = dplyr::if_else(
        condition = nom_bassin %in%
          c("RHONE-MEDITERRANEE-CORSE", "RHONE-MEDITERRANEE", "CORSE"),
        true      = "RHONE-MEDITERRANEE-CORSE",
        false     = nom_bassin
      ) %>% 
        factor(levels = c("ARTOIS-PICARDIE", "SEINE-NORMANDIE", "LOIRE-BRETAGNE", 
                          "RHIN-MEUSE", "ADOUR-GARONNE", "RHONE-MEDITERRANEE-CORSE")),
      annee_observation          = as.factor(annee_observation),
      mois_observation           = lubridate::month(
        date_observation,
        label  = TRUE,
        abbr   = FALSE,
        locale = "French_France.1252"
      )                %>% 
        as.character() %>% 
       stringr::str_replace_all(
         string      = .,
         pattern     = "février", 
         replacement = "fevrier"
         )             %>% 
        stringr::str_replace_all(
          string      = .,
          pattern     = "août",
          replacement = "aout"
          )            %>% 
        stringr::str_replace_all(
          string      = .,
          pattern     = "décembre",
          replacement = "decembre"
          ),
      label_observation_national = factor(
        label_observation_national, 
        levels = c("Observation impossible", "Ecoulement visible",
                   "Ecoulement non visible", "Assec")
        )
    )                   
}

save_plotly <- function(x, file, file_static = NULL, title) {
  require(plotly)
  require(purrr)
  require(glue)
  require(htmlwidgets)
  require(here)
  
  '%>%' <- plotly::'%>%'
  
  x$sizingPolicy$padding <- "0"
  
  x                          %>% 
    plotly::partial_bundle(p        = .,
                           local    = FALSE,
                           minified = TRUE) %>% 
    (function(x2) {
      x2$dependencies <- x2$dependencies %>% 
        purrr::map(
          function(dep_i) {
            if (dep_i$name == "jquery")
              dep_i$src <- list(href = glue::glue("https://cdnjs.cloudflare.com/ajax/libs/jquery/{dep_i$version}"))
            
            if (dep_i$name == "plotly-basic") {
              dep_i$src <-  list(href = glue::glue("https://cdnjs.cloudflare.com/ajax/libs/plotly.js/{dep_i$version}"))
              dep_i$script <- "plotly-basic.min.js"
            }
            
            if (dep_i$name == "plotly-cartesian") {
              dep_i$src <-  list(href = glue::glue("https://cdnjs.cloudflare.com/ajax/libs/plotly.js/{dep_i$version}"))
              dep_i$script <- "plotly-cartesian.min.js"
            }
            
            if (dep_i$name == "plotly-main") {
              dep_i$src <-  list(href = glue::glue("https://cdnjs.cloudflare.com/ajax/libs/plotly.js/{dep_i$version}"))
              dep_i$script <- "plotly.min.js"
            }
            
            dep_i$src <- purrr::map(dep_i$src, as.character)
            dep_i
          }
        )
      
      x2
    }) %>% 
    htmlwidgets::saveWidget(widget        = .,
                            file          = here::here(file),
                            title         = title,
                            selfcontained = FALSE,
                            libdir        = "www")
  
  if (!is.null(file_static))
    plotly::orca(x, file = file_static)
  
  c(file, file_static)
}

unquote2character <- function(x) {
  require(stringr)
  require(rlang)
  
  stringr::str_remove(rlang::expr_text(rlang::enquo(x)), pattern = "~")
}
