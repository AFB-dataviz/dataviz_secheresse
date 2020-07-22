plot_choropleth <- function(data, geojson, by, z, 
                            frame = 1, frame_label = NULL, step.duration = 1000,
                            opacity = .6, width = 800, height = width, ...,
                            style = "carto-positron", zoom = 3.75) {
  require(stats)
  require(dplyr)
  require(purrr)
  require(plotly)
  require(stringr)
  
  '%>%' <- dplyr::'%>%'
  
  by    <- stats::formula(paste0("~", unquote2character({{ by }})))
  z     <- stats::formula(paste0("~", unquote2character({{ z }})))
  frame <- stats::formula(paste0("~", unquote2character({{ frame }})))
  
  geojson_center <- geojson$features %>%
    purrr::map_df(.f = function(x) {
      x$geometry$coordinates           %>%
        unlist()                       %>%
        matrix(ncol = 2, byrow = TRUE) %>%
        as.data.frame()
    })                               %>%
    dplyr::summarise_all(.funs = function(x) mean(range(x)))
  
  if ("label" %in% colnames(data)) {
    p <- plotly::plot_ly(width = width, height = height) %>%
      plotly::add_trace(
        p         = .,
        data      = data,
        geojson   = geojson,
        type      = "choroplethmapbox",
        locations = by,
        z         = z,
        frame     = frame,
        text      = ~label,
        hoverinfo = "text",
        marker    = list(
          line    = list(width = .1),
          opacity = opacity
        ),
        ...
      )  
  } else {
    p <- plotly::plot_ly(width = width, height = height) %>%
      plotly::add_trace(
        p         = .,
        data      = data,
        geojson   = geojson,
        type      = "choroplethmapbox",
        locations = by,
        z         = z,
        frame     = frame,
        marker    = list(
          line    = list(width = .1),
          opacity = opacity
        ),
        ...
      )  
  }
  
  
  p <- p %>%
    plotly::layout(
      p      = .,
      mapbox = list(
        style  = style,
        center = list(
          lon = geojson_center$V1,
          lat = geojson_center$V2
          ),
        zoom = zoom,
        margin = list(
          t = 0, 
          r = 0,
          b = 0,
          l = 0
          )
        )
      ) %>% 
    plotly::animation_opts(
      p     = .,
      frame = step.duration
      )
  
  if (!is.null(frame_label)) {
    if (!stringr::str_detect(string = frame_label, pattern = "\\s$")) {
      frame_label <- paste0(frame_label, " ")
    }
    
    p <- p %>%
      plotly::animation_slider(
        p            = .,
        currentvalue = list(prefix = frame_label)
      ) %>% 
      plotly::animation_button(
        p       = .,
        label   = "&#9654;  Faire défiler",
        x       = 0, 
        xanchor = "left",
        y       = -.13,
        yanchor = "bottom"
      )
  }
  
  p
}


plot_drought_map <- function(data, palette, geojson, width = 850, height = 605, zoom = 4.3) {
  require(purrr)
  require(rjson)
  require(dplyr)
  require(rmapshaper)
  
  '%>%' <- dplyr::'%>%'
  
  colorvec2plotlycolorscale <- function(x) {
    purrr::map(
      .x = seq_along(x),
      .f = function(i) {
        list(
          (i - 1) / (length(x) - 1),
          x[[i]]
        )
      }
    )
  }
  
  geo_dpt <- rjson::fromJSON(file = geojson) %>%
    (function(x) {
      x$features <- purrr::map(
        .x = x$features, 
        .f = function(i) {
          i$id <- i$properties$code
          i
          }
        )
      
      class(x) <- "geo_list"
      
      x
    })                                       %>%
    rmapshaper::ms_simplify(
      input = .,
      keep  = 0.01
      )
  
  data   %>%
    dplyr::distinct(
      .data = .,
      code_departement, nom_departement, code_site, annee_observation,
      label_observation_national
    )    %>%
    dplyr::group_by(
      .data = .,
      code_departement, nom_departement, annee_observation
      )  %>%
    dplyr::summarise(
      .data        = .,
      nombre_sites = dplyr::n_distinct(code_site),
      sites_assec  = dplyr::n_distinct(code_site[label_observation_national == "Assec"]),
      .groups      = "drop"
      )  %>% 
    dplyr::mutate(
      .data             = .,
      pourcentage_assec = 100 * sites_assec / nombre_sites
      )  %>%
    dplyr::mutate(
      .data = .,
      label = glue("{nom_departement} ({code_departement}): {annee_observation}\n{sites_assec} sites en assec ({round(pourcentage_assec, 2)}%)")
      )  %>%
    dplyr::rename(
      .data                            = .,
      `Part des sites<br>en assec (%)` = pourcentage_assec
      )  %>%
    plot_choropleth(
      data        = ., 
      geojson     = geo_dpt, 
      by          = code_departement, 
      z           = `Part des sites<br>en assec (%)`, 
      zmin        = 0, 
      zmax        = 100,
      frame       = annee_observation,
      frame_label = "Année sélectionnée:",
      width       = width,
      height      = height,
      zoom        = zoom, 
      colorscale  = colorvec2plotlycolorscale(palette)
      )
}
