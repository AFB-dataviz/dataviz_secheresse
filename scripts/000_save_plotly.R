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

