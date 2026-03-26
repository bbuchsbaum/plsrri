#' Modern PLS Plot Theme
#'
#' @description
#' Custom ggplot2 theme for PLS visualizations.
#' Designed for 2026 aesthetics: clean, minimal, publication-ready.
#'
#' @name pls-theme
NULL

#' PLS Plot Theme
#'
#' @description
#' A clean, modern ggplot2 theme suitable for neuroimaging publications.
#'
#' @param base_size Base font size (default 12)
#' @param base_family Base font family (default "")
#' @param base_line_size Base line size (default 0.5)
#' @param base_rect_size Base rect size (default 0.5)
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_pls()
theme_pls <- function(base_size = 12,
                       base_family = "",
                       base_line_size = 0.5,
                       base_rect_size = 0.5) {

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
  ggplot2::theme(
    # Panel
    panel.grid.major = ggplot2::element_line(color = "#E8E8E8", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
    panel.border = ggplot2::element_rect(color = "#CCCCCC", fill = NA, linewidth = 0.5),

    # Axis
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(color = "#333333", linewidth = 0.3),
    axis.ticks.length = ggplot2::unit(3, "pt"),
    axis.text = ggplot2::element_text(color = "#333333", size = base_size * 0.85),
    axis.title = ggplot2::element_text(color = "#111111", size = base_size,
                                        face = "bold"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),

    # Legend
    legend.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
    legend.key = ggplot2::element_rect(fill = NA, color = NA),
    legend.title = ggplot2::element_text(face = "bold", size = base_size * 0.9),
    legend.text = ggplot2::element_text(size = base_size * 0.85),
    legend.position = "right",
    legend.margin = ggplot2::margin(4, 4, 4, 4),

    # Strip (facets)
    strip.background = ggplot2::element_rect(fill = "#F0F0F0", color = "#CCCCCC"),
    strip.text = ggplot2::element_text(face = "bold", size = base_size * 0.9,
                                        color = "#333333",
                                        margin = ggplot2::margin(4, 4, 4, 4)),

    # Title
    plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2,
                                        color = "#111111",
                                        margin = ggplot2::margin(b = 8)),
    plot.subtitle = ggplot2::element_text(size = base_size * 0.95,
                                           color = "#555555",
                                           margin = ggplot2::margin(b = 12)),
    plot.caption = ggplot2::element_text(size = base_size * 0.8,
                                          color = "#888888",
                                          hjust = 1,
                                          margin = ggplot2::margin(t = 8)),

    # Plot margins
    plot.margin = ggplot2::margin(12, 12, 12, 12),
    plot.background = ggplot2::element_rect(fill = "white", color = NA)
  )
}

#' PLS Color Palettes
#'
#' @description
#' Color palettes designed for PLS visualizations.
#'
#' @param n Number of colors needed
#' @param palette Palette name: "diverging", "sequential", "qualitative"
#' @param reverse Logical, reverse palette order
#'
#' @return Character vector of hex colors
#' @export
#'
#' @examples
#' pls_colors(5, "diverging")
pls_colors <- function(n = 5, palette = "diverging", reverse = FALSE) {

  palettes <- list(
    # Blue to red for BSR/saliences
    diverging = c("#2166AC", "#4393C3", "#92C5DE", "#F7F7F7",
                  "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"),

    # Blue for positive loadings
    sequential_blue = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1",
                        "#6BAED6", "#4292C6", "#2171B5", "#084594"),

    # Red for negative loadings
    sequential_red = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272",
                       "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D"),

    # Qualitative for groups/conditions
    qualitative = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2",
                    "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7"),

    # Viridis-inspired for continuous
    viridis = c("#440154", "#482878", "#3E4A89", "#31688E",
                "#26828E", "#1F9E89", "#35B779", "#6ECE58", "#B5DE2B", "#FDE725")
  )

  pal <- palettes[[palette]]
  if (is.null(pal)) {
    stop("Unknown palette: ", palette)
  }

  if (reverse) {
    pal <- rev(pal)
  }

  if (n <= length(pal)) {
    # Sample from palette
    idx <- seq(1, length(pal), length.out = n)
    return(pal[round(idx)])
  } else {
    # Interpolate
    colorRampPalette(pal)(n)
  }
}

#' PLS Diverging Color Scale
#'
#' @description
#' ggplot2 color scale for diverging data (e.g., BSR, saliences).
#'
#' @param ... Arguments passed to scale_color_gradientn
#' @param midpoint Midpoint of diverging scale (default 0)
#' @param limits Scale limits
#'
#' @return A ggplot2 scale object
#' @export
scale_color_pls <- function(..., midpoint = 0, limits = NULL) {
  colors <- pls_colors(11, "diverging")

  ggplot2::scale_color_gradientn(
    colors = colors,
    values = scales::rescale(c(-5, -3, -1, 0, 1, 3, 5)),
    limits = limits,
    na.value = "#888888",
    ...
  )
}

#' PLS Diverging Fill Scale
#'
#' @description
#' ggplot2 fill scale for diverging data.
#'
#' @param ... Arguments passed to scale_fill_gradientn
#' @param midpoint Midpoint of diverging scale (default 0)
#' @param limits Scale limits
#'
#' @return A ggplot2 scale object
#' @export
scale_fill_pls <- function(..., midpoint = 0, limits = NULL) {
  colors <- pls_colors(11, "diverging")

  ggplot2::scale_fill_gradientn(
    colors = colors,
    values = scales::rescale(c(-5, -3, -1, 0, 1, 3, 5)),
    limits = limits,
    na.value = "#888888",
    ...
  )
}

#' PLS Qualitative Color Scale
#'
#' @description
#' ggplot2 color scale for discrete data (groups, conditions).
#'
#' @param ... Arguments passed to scale_color_manual
#'
#' @return A ggplot2 scale object
#' @export
scale_color_pls_discrete <- function(...) {
  ggplot2::scale_color_manual(
    values = pls_colors(8, "qualitative"),
    ...
  )
}

#' PLS Qualitative Fill Scale
#'
#' @description
#' ggplot2 fill scale for discrete data.
#'
#' @param ... Arguments passed to scale_fill_manual
#'
#' @return A ggplot2 scale object
#' @export
scale_fill_pls_discrete <- function(...) {
  ggplot2::scale_fill_manual(
    values = pls_colors(8, "qualitative"),
    ...
  )
}
