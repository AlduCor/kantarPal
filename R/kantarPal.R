library(ggplot2)

theme_set(theme_minimal())

#' Kantar Corporate Colors
kt_colours <- c('Royal Blue' = "#0060FF",
                'Blue' = "#00B6FF",
                'Teal' = "#00E5BA",
                'Lime' = "#9EE900",
                'Green' = "#00B600",
                'Purple' = "#802AB7",
                'Violet' = "#C700D3",
                'Pink' = "#EB0064",
                'Orange' = "#FF5000",
                'Yellow' = "#FEDB00",
                'Dark Royal Blue' = "#191973",
                'Dark Blue' = "#005b81",
                'Dark Teal' = "#003a45",
                'Dark Lime' = "#416b18",
                'Dark Green' = "#00411e",
                'Dark Purple' = "#451356",
                'Dark Violet' = "#751159",
                'Dark Pink' = "#9b0028",
                'Dark Orange' = "#934900",
                'Dark Yellow' = "#867d00",
                'Warm grey 1' = "#858574",
                'Warm grey 2' = "#AEAE9F",
                'Warm grey 3' = "#D4D4C9",
                'Warm grey 4' = "#F2F1EF",
                'Significant red 1' = "#E10000",
                'Significant red 2' = "#ED6666",
                'Significant green 1' = "#00D200",
                'Significant green 2' = "#66E466")


#' Function to extract kantar colors as hex codes
#'
#' @param ... Character names of kantar_colors
#'
kantar_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (kantar_colors)

  kantar_colors[cols]
}
#' Create Kantar Pallets
kt_palettes <- list(
  `KT_Primary` = c("#858574", "#AEAE9F", "#D4D4C9", "#F2F1EF"),
  `KT_Secondary_bright` = c("#0060FF", "#00B6FF", "#00E5BA", "#9EE900","#00B600",
                            "#802AB7", "#C700D3", "#EB0064", "#FF5000", "#FEDB00"),
  `KT_Secondary_dark` = c("#191973", "#005b81", "#003a45", "#416b18", "#00411e",
                          "#451356", "#751159", "#9b0028", "#934900", "#867d00"),
  `KT_Significant` = c("#E10000", "#ED6666", "#00D200", "#66E466"),
  `KT_Light_Blue` = c("#0060ff", "#2674ff", "#4387ff", "#5d99ff", "#78abff"),
  `KT_Light_Green` = c("#009000", "#00ab00", "#00c600", "#01e201", "#01ff01"),
  `KT_Light_Violet` = c("#c700d3", "#d32ede", "#df47e9", "#eb5bf4", "#f76eff"),
  `KT_Teal` = c("#003a45", "#00616c", "#008b8d", "#00b8a8", "#00e5ba"),
  `KT_Light_Teal` = c("#007d66", "#009c7f", "#00bc99", "#00ddb4", "#00ffcf"),
  `KT_Blue_Teal` = c("#00b6ff", "#00c5fc", "#00d2ee", "#00ddd7", "#00e5ba"),
  `KT_Light_Purple` = c("#802ab7", "#9b57c8", "#b47ed8", "#cda6e7", "#e6cdf6"),
  `KT_grey`  = kantar_cols("Warm grey 1", "Warm grey 2","Warm grey 3","Warm grey 4"),
)
#' Return function to interpolate a kantar color palette
#'
#' @param palette Character name of palette in kantar_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
kantar_pal <-
  function(palette = "main",
           reverse = FALSE,
           expand_by = 0,
           ...) {
    pal <- kantar_palettes[[palette]]

    if (reverse)
      pal <- rev(pal)
    if (expand_by > 0) {
      colorRampPalette(pal, ...)(expand_by)
    } else {
      colorRampPalette(pal, ...)
    }
  }

#' Color scale constructor for kantar colors
#'
#' @param palette Character name of palette in kantar_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_color_kantar <-
  function(palette = "main",
           discrete = TRUE,
           reverse = FALSE,
           expand_by = 0,
           ...) {
    pal <- kantar_pal(palette = palette, reverse = reverse)

    if (discrete) {
      discrete_scale("colour", paste0("kantar_", palette), palette = pal, ...)
    } else {
      scale_color_gradientn(colours = pal(256), ...)
    }
  }

#' Fill scale constructor for kantar colors
#'
#' @param palette Character name of palette in kantar_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @export
scale_fill_kantar <-
  function(palette = "KT_Primary",
           discrete = TRUE,
           reverse = FALSE,
           expand_by = 0,
           ...) {
    pal <- kantar_pal(palette = palette, reverse = reverse)

    if (discrete) {
      discrete_scale("fill", paste0("kantar_", palette), palette = pal, ...)
    } else {
      scale_fill_gradientn(colours = pal(256), ...)
    }
  }
