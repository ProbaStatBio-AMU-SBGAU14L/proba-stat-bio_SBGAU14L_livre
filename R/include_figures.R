include_manual_figure <- function(
    filename,
    folder = "figures/manual_exports",
    html_format = "svg",
    check_file = TRUE) {
  
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Missing package: knitr")
  }
  
  extension <- if (knitr::is_latex_output()) {
    "pdf"
  } else if (knitr::is_html_output()) {
    html_format
  } else {
    "png"
  }
  
  image_absolute <- vapply(
    filename,
    function(x) {
      path <- normalizePath(
        file.path(folder, paste0(x, ".", extension)),
        winslash = "/",
        mustWork = FALSE
      )
      
      if (!file.exists(path) && extension != "png") {
        path <- normalizePath(
          file.path(folder, paste0(x, ".png")),
          winslash = "/",
          mustWork = FALSE
        )
      }
      
      if (check_file && !file.exists(path)) {
        stop("Figure not found: ", path)
      }
      
      path
    },
    character(1)
  )
  
  input_file <- knitr::current_input(dir = TRUE)
  
  input_dir <- dirname(
    normalizePath(
      input_file,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  image_relative <- vapply(
    image_absolute,
    xfun::relative_path,
    character(1),
    dir = input_dir
  )
  
  knitr::include_graphics(
    path = image_relative,
    rel_path = FALSE,
    error = FALSE
  )
}
