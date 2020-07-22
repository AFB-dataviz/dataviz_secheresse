plot_heatmap <- function(data, x, y, fill, label = NULL, facets = NULL, 
                         scale_fill = ggplot2::scale_fill_viridis_b, complete = FALSE) {
  require(tidyr)
  require(rlang)
  require(ggplot2)
  require(dplyr)
  
  '%>%' <- dplyr::'%>%'
  
  if ("function" %in% class(scale_fill)) {
    scale_fill <- scale_fill()
  }
  
  if (complete)
    data <- data %>%
      tidyr::complete(
        data = .,
        {{ x }},
        {{ y }}
        )
  
  if (unquote2character({{ label }}) == "NULL") {
    gg <- data %>%
      ggplot2::ggplot(
        data    = .,
        mapping = ggplot2::aes(
          x    = {{ x }}, 
          y    = {{ y }}, 
          fill = {{ fill }}
          )
        )
  } else {
    gg <- data %>%
      ggplot2::ggplot(
        data = .,
        mapping = ggplot2::aes(
          x    = {{ x }}, 
          y    = {{ y }}, 
          fill = {{ fill }}, 
          text = {{ label }}
          )
        )
  }
  
  gg <- gg                 +
    ggplot2::geom_tile()   +
    scale_fill             +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_text(
        hjust = 0,
        size = 10
        ),
      axis.text.x  = ggplot2::element_text(size = 10),
      axis.title   = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 10),
      legend.text  = ggplot2::element_text(size = 10)
    )
  
  facet_name <- unquote2character(facets)
  
  if (facet_name != "NULL") {
    gg <- gg  +
      ggplot2::facet_wrap(
        facets = ggplot2::vars({{facets}}),
        nrow   = 3
        )     +
      ggplot2::theme(
        strip.text = ggplot2::element_text(
          hjust  = 0, 
          vjust  = .5, 
          colour = "black"
          ),
        strip.background = ggplot2::element_rect(
          fill   = "white",
          colour = "white"
          )
        )
  }
  gg
}

plot_drought_seasons <- function(data, palette, fill_break = 20, fill_max = NULL, width = 800, height = 800) {
  require(dplyr)
  require(stringr)
  require(tidyr)
  require(glue)
  require(forcats)
  require(ggplot2)
  require(plotly)
  
  '%>%' <- dplyr::'%>%'
  
  mois_usuels <- c("mai", "juin", "juillet", 
                   "août", "septembre")
  
  SeasonsData <- data   %>%
    dplyr::mutate(
      .data = .,
      mois_observation = mois_observation %>% 
        stringr::str_replace_all(
          pattern = "aout", 
          replacement = "août"
          )
      )                 %>% 
    dplyr::filter(
      .data = .,
      mois_observation %in% mois_usuels,
      !is.na(nom_bassin)
      )                 %>%
    dplyr::mutate(
      .data = .,
      mois_observation = factor(
        x      = mois_observation, 
        levels = mois_usuels,
        ordered = TRUE
        )
      )                 %>% 
    dplyr::distinct(
      .data = .,
      nom_bassin, code_site, 
      annee_observation, mois_observation,
      label_observation_national
    )                   %>%
    dplyr::group_by(
      .data = .,
      nom_bassin, 
      annee_observation, 
      mois_observation
      )                 %>%
    dplyr::mutate(
      .data        = .,
      nombre_sites = dplyr::n_distinct(code_site),
      sites_assec  = dplyr::n_distinct(code_site[label_observation_national == "Assec"])
      )                 %>% 
    dplyr::ungroup()    %>% 
    dplyr::distinct(
      .data = .,
      nom_bassin, annee_observation, mois_observation, 
      label_observation_national, nombre_sites, sites_assec
      )                 %>% 
    tidyr::complete(
      data = .,
      nom_bassin, annee_observation, 
      mois_observation, label_observation_national, 
      fill = list(sites_assec = 0)
      )                 %>% 
    dplyr::group_by(
      .data = .,
      nom_bassin, annee_observation, mois_observation
      )                 %>% 
    dplyr::mutate(
      .data        = .,
      nombre_sites = 
        ifelse(
          test = length(unique(nombre_sites[!is.na(nombre_sites)])) == 0,
          yes  = NA,
          no   = unique(nombre_sites[!is.na(nombre_sites)])
          )
      )                 %>% 
    dplyr::mutate(
      .data       = .,
      sites_assec = ifelse(
        test = is.na(nombre_sites), 
        yes  = NA, 
        no   = sites_assec
        )
      )                 %>% 
    dplyr::ungroup()    %>% 
    dplyr::filter(
      .data = .,
      label_observation_national == "Assec"
      )                 %>% 
    dplyr::distinct(
      .data = .,
      nom_bassin, annee_observation, mois_observation, 
      nombre_sites, sites_assec
      )                 %>% 
    dplyr::mutate(
      .data             = .,
      annee_observation = annee_observation %>% 
        as.character()                      %>% 
        as.numeric()
      )                 %>%
    dplyr::mutate(
      .data            = .,
      pourcentage_site = 100 * sites_assec / nombre_sites
      )                 %>%
    dplyr::mutate(
      .data = .,
      label = ifelse(
        test = is.na(nombre_sites),
        yes  = glue::glue("<b>{mois_observation} {annee_observation}</b>\nPas de sites prospectés"),
        no   = glue::glue("<b>{mois_observation} {annee_observation}</b>\n{sites_assec} / {nombre_sites} sites ({round(pourcentage_site)}%) en assec.")
        )
      )                 %>% 
    dplyr::mutate(
      .data            = .,
      mois_observation = forcats::fct_rev(mois_observation)
      )
  
  if (is.null(fill_max)) {
    FillMax <- SeasonsData %>% 
      dplyr::pull(.data = .,
                  var   = pourcentage_site
                  )        %>% 
      max()                %>% 
      '/'(fill_break)      %>% 
      ceiling()            %>% 
      '*'(fill_break)
  } else {
    FillMax <- fill_max
  }
  
  plot_heatmap(
    data       = SeasonsData,
    x          = annee_observation, 
    y          = mois_observation, 
    fill       = pourcentage_site, 
    label      = label, 
    scale_fill = ggplot2::scale_fill_gradientn(
      name    = "Part de sites\nen assec (%)", 
      colours = palette, 
      breaks  = seq(from = 0, to = FillMax, by = fill_break),
      limits  = c(0, FillMax)
      ), 
    facets     = nom_bassin
    ) %>% 
    plotly::ggplotly(
      p       = .,
      tooltip = "text",
      width   = width,
      height  = height
      )
}
