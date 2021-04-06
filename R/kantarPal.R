library(ggplot2)

theme_set(theme_minimal())

#' Kantar Corporate Colors
kantar_colors <- c(
  `red`        =     "#f30020",
  `orange`      =    "#f9370a",
  `yellow`       =   "#fbd50b",
  `green`     =      "#17b003",
  `dark blue`  =     "#0544fd",
  `purple` =         "#6c09a9",
  `turquise`     =   "#1ee2ab",
  `light grey` =     "#9f9d8d",
  `dark grey` =      "#082724",
  `neon green` =     "#34d900",
  `brown` =          "#a22203",
  `maroon` =         "#890108",
  `muted blue` =     "#0d729d",
  `peach` =          "#eb893e",
  `salmon` =         "#e96b71",
  `light purple` =   "#9b75fe",
  `purple red` =     "#c70097",
  `light blue` =     "#87b7f0",
  `text grey` =      "#676766"
)

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
kantar_palettes <- list(
  `main`  = kantar_cols("red", "dark blue", "turquise"),

  `light`  = kantar_cols("yellow", "light grey", "turquise"),

  `dark`   = kantar_cols("red", "dark grey", "purple", "dark blue", "green"),

  `mixed` = kantar_cols(
    "red",
    "orange",
    "yellow",
    "green" ,
    "turquise",
    "light grey",
    "dark blue",
    "purple",
    "dark grey"
  ),

  `expanded` = kantar_cols(
    "red",
    "orange",
    "yellow",
    "green" ,
    "turquise",
    "light grey",
    "dark blue",
    "purple",
    "dark grey",
    "neon green",
    "brown",
    "maroon",
    "muted blue",
    "peach",
    "salmon",
    "light purple",
    "purple red",
    "light blue"
  ),

  `grey`  = kantar_cols("light grey", "dark grey"),

  `scale1` = kantar_cols("red", "maroon", "dark grey"),

  `scale2` = kantar_cols("red", "light blue"),

  `scale3` = kantar_cols("red", "orange", "yellow", "white")
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
  function(palette = "main",
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
