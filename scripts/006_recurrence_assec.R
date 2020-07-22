plot_drought_return <- function(data, palette, width = 750, height = 700) {
  require(dplyr)
  require(purrr)
  require(forcats)
  require(stringr)
  require(glue)
  require(plotly)
  
  '%>%' <- dplyr::'%>%'
  
  palette <- palette %>% 
    purrr::set_names(
      x  = .,
      nm = 0:8
      )
  
  data   %>%
    dplyr::group_by(
      .data = .,
      nom_bassin, code_site
      )  %>%
    dplyr::summarise(
      .data            = .,
      recurrence_assec = dplyr::n_distinct(annee_observation[label_observation_national == "Assec"]) %>% 
        as.character(),
      .groups          = "drop"
      )  %>% 
    (function(df){
      dplyr::bind_rows(
        df    %>%
          dplyr::mutate(
            .data         = .,
            total_station = dplyr::n_distinct(code_site)
            ) %>%
          dplyr::group_by(
            .data = .,
            recurrence_assec, total_station
            ) %>%
          dplyr::summarise(
            .data = .,
            nombre_sites = dplyr::n_distinct(code_site),
            .groups = "drop"
            ) %>% 
          dplyr::mutate(
            .data   = .,
            parents = "&#8617; Retour à la vue initiale",
            ids     = recurrence_assec
            ),
        df    %>%
          dplyr::group_by(
            .data = .,
            nom_bassin
            ) %>% 
          dplyr::mutate(
            .data         = .,
            total_station = dplyr::n_distinct(code_site)
            ) %>% 
          dplyr::group_by(
            .data = .,
            recurrence_assec
            ) %>% 
          dplyr::group_by(
            .data = .,
            recurrence_assec, nom_bassin, total_station
            ) %>% 
          dplyr::summarise(
            .data        = .,
            nombre_sites = dplyr::n_distinct(code_site),
            .groups = "drop"
            ) %>% 
          dplyr::mutate(
            .data   = .,
            parents = recurrence_assec, 
            ids     = paste0(recurrence_assec, "_", nom_bassin)
            )
        )
    })   %>%
    dplyr::mutate(
      .data             = .,
      label_recurrence  = dplyr::case_when(
        recurrence_assec == "0" ~ "<b>Pas d'assec</b>",
        recurrence_assec == "1" ~ "<b>1 année avec assec</b>",
        TRUE  ~ glue::glue("<b>{recurrence_assec} années avec assec</b>") %>%
          as.character()
      ) %>%
        forcats::fct_reorder(
          .f = .,
          .x = as.numeric(recurrence_assec)
          ),
      label_bassin      = as.character(nom_bassin) %>%
        stringr::str_replace_all(
          string      = .,
          pattern     = "-",
          replacement = "<br>"
          )
    )    %>%
    dplyr::mutate(
      .data  = .,
      labels = ifelse(
        test = is.na(nom_bassin),
        yes  = as.character(label_recurrence),
        no   = glue::glue("{label_bassin}<br><br>{nombre_sites}/{total_station} stations") %>% 
          as.character()
        ),
    hovertext = ifelse(
      test = is.na(nom_bassin),
      yes  = glue::glue("{label_recurrence}<br><br>{nombre_sites}/{total_station} stations"),
      no   = glue::glue("{label_recurrence}<br>{label_bassin}<br><br>{nombre_sites}/{total_station} stations")
      ) %>% 
      as.character(),
    colors = palette[recurrence_assec]
    )    %>%
    plotly::plot_ly(
      data         = .,
      ids          = ~ids,
      labels       = ~labels,
      parents      = ~parents,
      values       = ~nombre_sites,
      text         = ~hovertext,
      type         = "treemap",
      branchvalues = "total",
      marker       = list(colors = ~colors),
      textinfo     = "label",
      hoverinfo    = "text",
      width        = width, 
      height       = height
      )
}
