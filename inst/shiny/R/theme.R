# PLS Shiny Theme
# Modern bslib theme with 2026 aesthetics

#' Create PLS Shiny Theme
#'
#' @description
#' Creates a modern bslib theme for the PLS Shiny GUI.
#'
#' @return A bslib theme object
#' @keywords internal
pls_shiny_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",

    # Primary colors
    primary = "#4E79A7",
    secondary = "#6B7280",
    success = "#22C55E",
    warning = "#F59E0B",
    danger = "#EF4444",
    info = "#3B82F6",

    # Background and text
    bg = "#FFFFFF",
    fg = "#111111",

    # Typography
    base_font = bslib::font_google("Inter", wght = "300..600"),
    heading_font = bslib::font_google("Inter", wght = "600"),
    code_font = bslib::font_google("JetBrains Mono"),

    # Base sizes
    "font-size-base" = "0.9375rem",  # 15px
    "line-height-base" = "1.5",

    # Spacing based on 4px grid
    "spacer" = "1rem",

    # Border radius
    "border-radius" = "0.5rem",
    "border-radius-sm" = "0.25rem",
    "border-radius-lg" = "0.75rem",

    # Cards
    "card-border-width" = "1px",
    "card-border-color" = "#E5E7EB",
    "card-cap-bg" = "#F9FAFB",

    # Inputs
    "input-border-color" = "#D1D5DB",
    "input-focus-border-color" = "#4E79A7",

    # Navbar/Header
    "navbar-bg" = "#FFFFFF",
    "navbar-light-color" = "#374151"
  ) |>
    bslib::bs_add_rules("
      /* Custom CSS variables for 4px grid spacing */
      :root {
        --pls-space-xs: 4px;
        --pls-space-sm: 8px;
        --pls-space-md: 16px;
        --pls-space-lg: 24px;
        --pls-space-xl: 32px;

        /* Semantic colors */
        --pls-success: #22C55E;
        --pls-warning: #F59E0B;
        --pls-error: #EF4444;
        --pls-muted: #6B7280;
        --pls-border: #E5E7EB;

        /* Status dot colors */
        --pls-dot-complete: #22C55E;
        --pls-dot-active: #F59E0B;
        --pls-dot-error: #EF4444;
        --pls-dot-pending: #D1D5DB;
      }

      /* Base body styles */
      body {
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
      }

      /* Card enhancements */
      .card {
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.08);
        transition: box-shadow 0.2s ease;
      }

      .card:hover {
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
      }

      /* Input focus states */
      .form-control:focus,
      .form-select:focus {
        box-shadow: 0 0 0 3px rgba(78, 121, 167, 0.15);
      }
    ")
}

#' Semantic Color Palette
#'
#' @description
#' Color values for consistent use across the Shiny app.
#'
#' @keywords internal
pls_colors_semantic <- list(
  success = "#22C55E",
  warning = "#F59E0B",
  error = "#EF4444",
  muted = "#6B7280",
  border = "#E5E7EB",
  text_primary = "#111111",
  text_secondary = "#6B7280",
  bg_primary = "#FFFFFF",
  bg_secondary = "#F9FAFB",
  accent = "#4E79A7"
)

#' Spacing Values (4px grid)
#'
#' @keywords internal
pls_spacing <- list(
  xs = "4px",
  sm = "8px",

  md = "16px",
  lg = "24px",
  xl = "32px"
)
