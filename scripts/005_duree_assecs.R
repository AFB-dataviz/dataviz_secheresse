calc_state_duration <- function(data, id, state, time, ...) {
  require(dplyr)
  require(purrr)
  require(progress)
  require(rlang)

  '%>%' <- dplyr::'%>%'

  find_seq <- function(x) {
    breaks <- c(
      1,
      which(x != dplyr::lag(x)),
      length(x)
    ) %>%
      unique()
    
    purrr::map(
      .x = seq(length(breaks)),
      .f = function(i) {
        if (breaks[i] != length(x)) {
          rep(breaks[i], breaks[i + 1] - breaks[i])
        } else {
          breaks[i]
        }
      }
    ) %>% 
      do.call(what = c)
  }
  
  pb <- progress::progress_bar$new(
    total  = nrow(dplyr::distinct(data, ..., {{ id }})),
    format = "  [:bar] :percent (:current/:total) in :elapsed (estimated time: :eta)"
    )
  
  pb$tick(0)
  
  data                             %>%
    dplyr::group_by(
      .data = .,
      ..., {{ id }}
      ) %>%
    dplyr::group_modify(
      .data = .,
      .f    = function(df, ...) {
        out <- df  %>% 
          dplyr::select(
            .data = .,
            {{ time }}, {{ state }}
            )      %>%
          dplyr::arrange(
            .data = .,
            {{ time }}
            )      %>%
          dplyr::mutate(
            .data = .,
            g     = find_seq({{ state }})
            )      %>%
          dplyr::group_by(
            .data = .,
            {{ state }}, g
            )      %>%
          dplyr::summarise(
            .data    = .,
            duration = diff(range({{ time }})),
            .groups  = "drop"
            )      %>%
          dplyr::group_by(
            .data = .,
            {{ state }}
            )      %>%
          dplyr::summarise(
            .data         = .,
            min_duration  = min(duration),
            mean_duration = mean(duration),
            max_duration  = max(duration),
            .groups       = "drop"
            ) 
        
        pb$tick()
        
        out
    }
    )                             %>% 
    dplyr::ungroup()
}



plot_drought_duration <- function(data, palette, x_break = 2, y_break = 10, width = 900, height = 900) {
  require(dplyr)
  require(forcats)
  require(glue)
  require(purrr)
  require(plotly)
  
  '%>%' <- dplyr::'%>%'
  
  data_duration <- data %>%
    dplyr::filter(
      .data = .,
      label_observation_national == "Assec"
      )                 %>%
    dplyr::arrange(
      .data = .,
      max_duration
      )                 %>%
    dplyr::mutate(
      .data      = .,
      recurrence = dplyr::case_when(
        max_duration == 0 ~ "une fois",
        max_duration == 1 ~ "2 mois consécutifs",
        max_duration == 2 ~ "3 mois consécutifs",
        max_duration == 3 ~ "4 mois consécutifs"
        )
      )                 %>%
    dplyr::mutate(
      .data      = .,
      recurrence = forcats::fct_reorder(
        .f = recurrence, 
        .x = max_duration
        )
      )                 %>%
    dplyr::group_by(
      .data = .,
      nom_bassin, annee_observation, recurrence
      )                 %>%
    dplyr::summarise(
      .data          = .,
      nombre_station = dplyr::n_distinct(code_site), 
      .groups        = "drop"
      )                 %>%
    dplyr::left_join(
      x = .,
      y = data %>%
        dplyr::group_by(
          .data = .,
          nom_bassin, annee_observation
          )    %>%
        dplyr::summarise(
          .data                = .,
          nombre_total_station = dplyr::n_distinct(code_site),
          .groups              = "drop"
          ),
      by = c("nom_bassin", "annee_observation")
    )                   %>%
    dplyr::mutate(
      .data               = .,
      pourcentage_station = nombre_station / nombre_total_station
      )                 %>%
    dplyr::mutate(
      .data = .,
      texthover =  glue::glue("<b>{nom_bassin} ({annee_observation})</b><br>assec observé {recurrence}<br>{nombre_station} stations")
      )                 %>% 
    dplyr::group_by(
      .data = .,
      nom_bassin
      )                 %>%
    dplyr::group_split()

  y_limits <- c(
    0,
    data_duration        %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(
        .data = .,
        nom_bassin, annee_observation
        )                %>%
      dplyr::summarise(
        .data             = .,
        pourcentage_total = sum(pourcentage_station),
        .groups           = "drop"
      )                  %>%
      dplyr::pull(
        .data = .,
        var   = pourcentage_total
        )                %>%
      max()              %>%
      "/"(.05)           %>%
      ceiling()          %>%
      "*"(.05)
  )

  purrr::map(
    .x = seq(length(data_duration)),
    .f = function(i) {
      plotly::plot_ly(
        data        = data_duration[[i]],
        x           = ~annee_observation,
        y           = ~pourcentage_station,
        hoverinfo   = "text",
        text        = ~ texthover,
        color       = ~recurrence,
        colors      = palette,
        legendgroup = ~recurrence,
        showlegend  = i == 1,
        type        = "bar",
        width       = width,
        height      = height
      )   %>%
        plotly::add_annotations(
          p         = .,
          text      = ~ unique(nom_bassin),
          x         = .5,
          y         = .95,
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
          p       = .,
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
            line      = list(color = "transparent")
          ),
          xaxis = list(
            autotick = FALSE,
            tick0    = min(as.numeric(as.character(data$annee_observation))),
            dtick    = x_break,
            title    = list(text = NULL)
          ),
          yaxis = list(
            title    = list(text = NULL),
            autotick = FALSE,
            range    = y_limits,
            tick0    = y_limits[1],
            dtick    = y_break,
            tickformat = "%"
          )
        )
    }
  )   %>%
    plotly::subplot(
      nrows  = 3, 
      margin = .04,
      shareX = FALSE, 
      shareY = TRUE
    ) %>%
    plotly::layout(
      p      = .,
      legend = list(
        orientation = "h",
        title       = list(text = "<b>Assec observé</b>",
                           side = "top")
      )
      ) %>%
    plotly::add_annotations(
      p         = .,
      text      = "Part de sites<br>en assec",
      x         = -.06,
      xref      = "paper", 
      xanchor   = "left",
      y         = 1.075,
      yref      = "paper",
      yanchor   = "top",
      align     = "left",
      showarrow = FALSE
    )
}
