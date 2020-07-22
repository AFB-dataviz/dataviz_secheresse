plot_flow_status <- function(data, palette, width, height) {
  require(dplyr)
  require(stringr)
  require(tidyr)
  require(glue)
  require(purrr)
  require(plotly)
  
  '%>%' <- dplyr::'%>%'
  
  FlowStatusData <- data %>%
    dplyr::filter(
      .data = .,
      label_observation_national != "Observation impossible"
      )                  %>%
    dplyr::mutate(
      .data = .,
      label_observation_national = label_observation_national %>% 
        as.character()                                        %>% 
        stringr::str_replace_all(
          string      = ., 
          pattern     = "E",
          replacement = "É")                                  %>% 
        paste0(., "     ")                                    %>% 
        factor(x = .,
               levels = c("Écoulement visible", 
                          "Écoulement non visible", 
                          "Assec")                            %>% 
                 paste0(., "     "))
        )                %>% 
    dplyr::group_by(
      .data = .,
      nom_bassin, annee_observation
      )                  %>%
    dplyr::mutate(
      .data   = .,
      n_sites = dplyr::n_distinct(code_site)
      )                  %>%
    dplyr::group_by(
      .data = .,
      nom_bassin, annee_observation, 
      label_observation_national, n_sites
      )                  %>%
    dplyr::summarise(
      .data = .,
      n_obs = dplyr::n(),
      .groups = "drop"
      )                  %>%
    tidyr::complete(
      data = .,
      nom_bassin, annee_observation, 
      label_observation_national, 
      fill = list(n_obs = 0)
      )                  %>%
    dplyr::group_by(
      .data = .,
      nom_bassin, annee_observation
      )                  %>%
    dplyr::mutate(
      .data     = .,
      n_sites   = as.integer(mean(n_sites, na.rm = TRUE)), 
      prop_obs  = n_obs / sum(n_obs), 
      hovertext = glue::glue("<b>{nom_bassin} ({annee_observation}: {n_sites} sites)</b><br>{round(100 * prop_obs, 2)}% d'observations en {tolower(as.character(label_observation_national))}") %>%
        stringr::str_replace(
          string      = ., 
          pattern     = "ecoulement", 
          replacement = "écoulement"
          )
      )                  %>%
    dplyr::group_by(
      .data = .,
      nom_bassin
      )                  %>%
    dplyr::group_split()
  
  FlowStatusPlot <- purrr::map(
    .x = seq(length(FlowStatusData)),
    .f = function(i) {
      plotly::plot_ly(
        width       = width, 
        height      = height,
        
      )   %>% 
        plotly::add_trace(
          data        = FlowStatusData[[i]] %>% 
            dplyr::filter(
              .data = .,
              label_observation_national == "Écoulement visible     "
              ), 
          x           = ~annee_observation, 
          y           = ~prop_obs, 
          hoverinfo   = "text", 
          text        = ~hovertext, 
          color       = ~label_observation_national,
          colors      = palette, 
          legendgroup = ~label_observation_national, 
          showlegend  = i == 1, 
          type        = "bar", 
          visible     = "legendonly"
        ) %>%
        plotly::add_trace(
          data        = FlowStatusData[[i]] %>% 
            dplyr::filter(label_observation_national != "Écoulement visible     "), 
          x           = ~annee_observation, 
          y           = ~prop_obs, 
          hoverinfo   = "text", 
          text        = ~hovertext, 
          color       = ~label_observation_national,
          colors      = palette, 
          legendgroup = ~label_observation_national, 
          showlegend  = i == 1, 
          type        = "bar"
        ) %>% 
      plotly::add_annotations(
        text      = ~ unique(nom_bassin),
        x         = .5, 
        y         = 1,
        yref      = "paper", 
        xref      = "paper",
        xanchor   = "center",
        yanchor   = "bottom",
        showarrow = FALSE, 
        font      = list(
          size  = 12,
          color = "black"
          )
        ) %>%
        plotly::layout(
          barmode = "stack", 
          shapes  = list(
            type      = "rect",
            x0        = 0, 
            x1        = 1, 
            xref      = "paper",
            y0        = 0, 
            y1        = 16, 
            yanchor   = 1, 
            yref      = "paper",
            ysizemode = "pixel", 
            fillcolor = "white", 
            line      = list(
              color = "transparent"
              )
            ),
          xaxis   = list(
            autotick = FALSE,
            tick0    = 2012,
            dtick    = 2,
            title    = list(
              text = NULL
              )
            ),
          yaxis   = list(
            autotick   = FALSE,
            tick0      = 0,
            dtick      = .25,
            title      = list(
              text = NULL
              ),
            tickformat = "%"
            )
          )
      }) %>%
    plotly::subplot(
      nrows  = 3, 
      margin = .04,
      shareX = FALSE, 
      shareY = TRUE
      )  %>% 
    plotly::layout(
      legend = list(
        orientation = "h",
        y           = -.05
        )
      )  %>%
    plotly::add_annotations(
      text      = "Part des<br>observations",
      x         = -.06,
      y         = 1.075,
      xref      = "paper", 
      yref      = "paper",
      xanchor   = "left",
      yanchor   = "top",
      align     = "left",
      showarrow = FALSE
      )
}
