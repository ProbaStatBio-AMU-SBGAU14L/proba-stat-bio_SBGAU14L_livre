# Reproduce two schematic figures illustrating sampling from a population
#
# The script uses only base R. No tidyverse and no X11/XQuartz-dependent
# package is required.
#
# Main functions:
#   draw_population_one_sample()
#   draw_population_some_samples()
#   export_population_sampling_figures()

# -------------------------------------------------------------------------
# Coordinates digitised from the original figures
# -------------------------------------------------------------------------

.page_height_px <- 993
.page_width_px  <- 1404
.page_ratio     <- .page_width_px / .page_height_px

.population_px <- data.frame(
  x = c(
    320.3, 411.5, 615.9, 1002.3, 729.5,
    547.7, 456.6, 1070.6, 275.1, 365.8,
    502.3, 593.1, 252.4, 956.6, 820.5,
    1047.9, 729.5, 1161.5, 297.7, 547.7,
    638.6, 684.1, 593.1, 979.5, 252.3,
    1025.1, 934.1, 525.1, 615.9, 456.6,
    820.5, 365.9, 752.3, 956.6, 502.3,
    411.5, 729.5, 593.1, 775.1, 638.7,
    956.7, 1047.9, 729.5, 456.7, 888.7,
    365.8, 411.5, 729.5, 502.3, 934.1,
    661.5, 343.1, 865.9, 434.1
  ),
  y = c(
    192.5, 215.4, 215.4, 260.5, 260.6,
    283.4, 283.4, 283.4, 283.4, 306.2,
    328.7, 328.8, 351.6, 351.6, 374.5,
    374.6, 374.6, 397.1, 397.2, 397.2,
    397.2, 442.5, 442.6, 442.6, 465.4,
    488.0, 488.0, 510.6, 510.6, 533.4,
    533.4, 533.4, 556.2, 556.2, 578.7,
    578.8, 624.1, 624.2, 669.7, 669.7,
    669.7, 669.8, 737.9, 738.0, 738.0,
    738.0, 783.4, 783.4, 783.4, 783.5,
    851.4, 851.4, 851.4, 851.5
  )
)

# Convert coordinates from image pixels to plotting coordinates.
.to_plot_coordinates <- function(x_px, y_px) {
  list(
    x = x_px / .page_height_px,
    y = 1 - y_px / .page_height_px
  )
}

.population <- with(
  .population_px,
  data.frame(
    x = x / .page_height_px,
    y = 1 - y / .page_height_px
  )
)

# The five observations highlighted in the first sample.
.sample_1_px <- data.frame(
  x = c(365.9, 456.6, 525.1, 411.5, 502.3),
  y = c(533.4, 533.4, 510.6, 578.8, 578.7)
)

.sample_1 <- with(
  .sample_1_px,
  data.frame(
    x = x / .page_height_px,
    y = 1 - y / .page_height_px
  )
)

# Ellipses used to represent five samples in the second figure.
.sample_ellipses_px <- data.frame(
  sample = 1:5,
  cx = c(440, 457, 552, 548, 979),
  cy = c(540, 773, 557, 271, 420),
  rx = rep(126, 5),
  ry = rep(103, 5)
)

.sample_ellipses <- transform(
  .sample_ellipses_px,
  cx = cx / .page_height_px,
  cy = 1 - cy / .page_height_px,
  rx = rx / .page_height_px,
  ry = ry / .page_height_px
)

# -------------------------------------------------------------------------
# Low-level drawing helpers
# -------------------------------------------------------------------------

.draw_ellipse <- function(cx, cy, rx, ry, col = "black", lwd = 2.2,
                          n = 240) {
  theta <- seq(0, 2 * pi, length.out = n)
  graphics::lines(
    cx + rx * cos(theta),
    cy + ry * sin(theta),
    col = col,
    lwd = lwd
  )
}

.draw_rounded_rectangle <- function(xleft, ybottom, xright, ytop,
                                    radius = 0.13,
                                    col = "black", lwd = 4.2,
                                    n = 40) {
  radius <- min(radius, (xright - xleft) / 2, (ytop - ybottom) / 2)

  tr <- seq(pi / 2, 0, length.out = n)
  br <- seq(0, -pi / 2, length.out = n)
  bl <- seq(-pi / 2, -pi, length.out = n)
  tl <- seq(pi, pi / 2, length.out = n)

  x <- c(
    xleft + radius,
    xright - radius,
    xright - radius + radius * cos(tr),
    xright,
    xright - radius + radius * cos(br),
    xleft + radius,
    xleft + radius + radius * cos(bl),
    xleft,
    xleft + radius + radius * cos(tl)
  )

  y <- c(
    ytop,
    ytop,
    ytop - radius + radius * sin(tr),
    ybottom + radius,
    ybottom + radius + radius * sin(br),
    ybottom,
    ybottom + radius + radius * sin(bl),
    ytop - radius,
    ytop - radius + radius * sin(tl)
  )

  graphics::lines(x, y, col = col, lwd = lwd)
}

.start_population_figure <- function() {
  old_par <- graphics::par(no.readonly = TRUE)

  graphics::par(
    mar = c(0, 0, 0, 0),
    xaxs = "i",
    yaxs = "i",
    xpd = NA,
    family = "sans"
  )

  graphics::plot.new()
  graphics::plot.window(
    xlim = c(0, .page_ratio),
    ylim = c(0, 1),
    asp = 1
  )

  .draw_rounded_rectangle(
    xleft = 172 / .page_height_px,
    ybottom = 1 - 931 / .page_height_px,
    xright = 1242 / .page_height_px,
    ytop = 1 - 112 / .page_height_px,
    radius = 0.135,
    lwd = 4.2
  )

  graphics::text(
    x = 707 / .page_height_px,
    y = 1 - 78 / .page_height_px,
    labels = bquote(Population~(italic(N)~elements)),
    cex = 1.65
  )

  invisible(old_par)
}

.draw_population_points <- function(exclude_sample_1 = FALSE) {
  population <- .population

  if (exclude_sample_1) {
    is_sample <- vapply(
      seq_len(nrow(population)),
      function(i) {
        any(
          abs(population$x[i] - .sample_1$x) < 0.002 &
            abs(population$y[i] - .sample_1$y) < 0.002
        )
      },
      logical(1)
    )
    population <- population[!is_sample, , drop = FALSE]
  }

  graphics::points(
    population$x,
    population$y,
    pch = 21,
    cex = 1.55,
    lwd = 1.1,
    col = "black",
    bg = "grey82"
  )
}

.draw_common_labels <- function() {
  graphics::text(
    x = 182 / .page_height_px,
    y = 1 - 600 / .page_height_px,
    labels = "Sample",
    adj = c(0, 0.5),
    cex = 1.45
  )

  graphics::text(
    x = 182 / .page_height_px,
    y = 1 - 640 / .page_height_px,
    labels = expression((italic(n)~elements)),
    adj = c(0, 0.5),
    cex = 1.40
  )

  graphics::text(
    x = 765 / .page_height_px,
    y = 1 - 455 / .page_height_px,
    labels = expression(mu * "," * sigma),
    cex = 2.35
  )
}

# -------------------------------------------------------------------------
# Public drawing functions
# -------------------------------------------------------------------------

#' Draw the population with one highlighted sample
#'
#' This function draws on the current graphics device.
draw_population_one_sample <- function() {
  old_par <- .start_population_figure()
  on.exit(graphics::par(old_par), add = TRUE)

  .draw_population_points(exclude_sample_1 = TRUE)

  # Sample ellipse
  ellipse <- .sample_ellipses[.sample_ellipses$sample == 1, ]
  .draw_ellipse(ellipse$cx, ellipse$cy, ellipse$rx, ellipse$ry)

  # Sample observations highlighted in black
  graphics::points(
    .sample_1$x,
    .sample_1$y,
    pch = 21,
    cex = 1.55,
    lwd = 1.1,
    col = "black",
    bg = "black"
  )

  graphics::text(
    x = 358 / .page_height_px,
    y = 1 - 470 / .page_height_px,
    labels = expression(bar(x)[1] * "," * s[1]),
    adj = c(0, 0.5),
    cex = 1.55
  )

  .draw_common_labels()
  invisible(NULL)
}

#' Draw the population with five possible samples
#'
#' This function draws on the current graphics device.
draw_population_some_samples <- function() {
  old_par <- .start_population_figure()
  on.exit(graphics::par(old_par), add = TRUE)

  .draw_population_points(exclude_sample_1 = FALSE)

  for (i in seq_len(nrow(.sample_ellipses))) {
    e <- .sample_ellipses[i, ]
    .draw_ellipse(e$cx, e$cy, e$rx, e$ry)
  }

  label_positions_px <- data.frame(
    sample = 1:5,
    x = c(355, 474, 578, 488, 880),
    y = c(465, 735, 548, 213, 380)
  )

  label_positions <- transform(
    label_positions_px,
    x = x / .page_height_px,
    y = 1 - y / .page_height_px
  )

  sample_labels <- list(
    expression(bar(x)[1] * "," * s[1]),
    expression(bar(x)[2] * "," * s[2]),
    expression(bar(x)[3] * "," * s[3]),
    expression(bar(x)[4] * "," * s[4]),
    expression(bar(x)[5] * "," * s[5])
  )

  for (i in seq_len(nrow(label_positions))) {
    graphics::text(
      x = label_positions$x[i],
      y = label_positions$y[i],
      labels = sample_labels[[i]],
      adj = c(0, 0.5),
      cex = 1.55
    )
  }

  .draw_common_labels()
  invisible(NULL)
}

# -------------------------------------------------------------------------
# Optional export helper
# -------------------------------------------------------------------------

#' Export both figures as vector PDF files
#'
#' Base R's pdf() device does not require XQuartz.
export_population_sampling_figures <- function(
    directory = "figures",
    width = 12,
    height = 8.5,
    also_png = FALSE,
    png_resolution = 180) {

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  file_one_pdf <- file.path(directory, "population_one_sample.pdf")
  file_some_pdf <- file.path(directory, "population_some_samples.pdf")

  grDevices::pdf(file_one_pdf, width = width, height = height,
                 family = "Helvetica", useDingbats = FALSE)
  draw_population_one_sample()
  grDevices::dev.off()

  grDevices::pdf(file_some_pdf, width = width, height = height,
                 family = "Helvetica", useDingbats = FALSE)
  draw_population_some_samples()
  grDevices::dev.off()

  output <- c(file_one_pdf, file_some_pdf)

  if (isTRUE(also_png)) {
    width_px <- round(width * png_resolution)
    height_px <- round(height * png_resolution)

    file_one_png <- file.path(directory, "population_one_sample.png")
    file_some_png <- file.path(directory, "population_some_samples.png")

    grDevices::png(file_one_png, width = width_px, height = height_px,
                   res = png_resolution, bg = "white")
    draw_population_one_sample()
    grDevices::dev.off()

    grDevices::png(file_some_png, width = width_px, height = height_px,
                   res = png_resolution, bg = "white")
    draw_population_some_samples()
    grDevices::dev.off()

    output <- c(output, file_one_png, file_some_png)
  }

  invisible(normalizePath(output, mustWork = FALSE))
}

# -------------------------------------------------------------------------
# Examples
# -------------------------------------------------------------------------

# In RStudio:
# source("reproduce_population_sampling_figures.R")
# draw_population_one_sample()
# draw_population_some_samples()

# To export vector PDF files:
# export_population_sampling_figures(directory = "figures")

# In a Quarto chunk:
#
# ```{r}
# #| echo: false
# #| fig-width: 12
# #| fig-height: 8.5
# source("R/reproduce_population_sampling_figures.R")
# draw_population_one_sample()
# ```
