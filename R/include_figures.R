include_manual_figure <- function(
    filename,
    folder = "figures/manual_exports",
    html_format = "svg",
    check_file = TRUE) {
  
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Missing package: knitr")
  }
  
  # Choix du format graphique selon le format du document
  extension <- if (knitr::is_latex_output()) {
    "pdf"
  } else if (knitr::is_html_output()) {
    html_format
  } else {
    # DOCX et autres formats
    "png"
  }
  
  # Chemin absolu du fichier à inclure
  image_absolute <- normalizePath(
    file.path(folder, paste0(filename, ".", extension)),
    winslash = "/",
    mustWork = FALSE
  )
  
  # Repli sur PNG lorsque le format préféré n'existe pas
  if (!file.exists(image_absolute) && extension != "png") {
    extension <- "png"
    
    image_absolute <- normalizePath(
      file.path(folder, paste0(filename, ".", extension)),
      winslash = "/",
      mustWork = FALSE
    )
  }
  
  if (check_file && !file.exists(image_absolute)) {
    stop(
      "Figure not found: ",
      image_absolute,
      "\nExpected basename: ",
      filename
    )
  }
  
  # Le chemin transmis à Pandoc doit être relatif au fichier .qmd courant
  input_file <- knitr::current_input(dir = TRUE)
  
  if (!nzchar(input_file)) {
    stop("Unable to determine the current Quarto input file.")
  }
  
  input_dir <- dirname(
    normalizePath(
      input_file,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  image_relative <- xfun::relative_path(
    image_absolute,
    dir = input_dir
  )
  
  knitr::include_graphics(
    path = image_relative,
    rel_path = FALSE,
    error = FALSE
  )
}
