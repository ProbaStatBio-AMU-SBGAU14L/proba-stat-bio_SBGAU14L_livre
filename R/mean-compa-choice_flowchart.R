# Flowchart: choice of a mean comparison test
# Requires the R package DiagrammeR as well as the Graphviz/DOT language
# install.packages("DiagrammeR")
#
# To export in SVG, PNG ou PDF, also install DiagrammeRsvg
# install.packages(c("DiagrammeRsvg", "rsvg"))

# A first version of this script was implemented by ChatGPT based on a flowchart manually drawn by Jacques van Helden
# The script was then adapted manually by Jacques van Helden

# Plot the flowchart ---------------------------------------------------------
mean_comparison_test_flowchart <- function(width = NULL, height = NULL) {
  
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop(
      "Le package 'DiagrammeR' n'est pas installé. ",
      "Exécutez install.packages('DiagrammeR')."
    )
  }
  
  code_dot <- r"{
  digraph mean_test_choice_flowchart {

    graph [
      layout = dot,
      rankdir = TB,
      bgcolor = "white",
      splines = ortho,
      nodesep = 0.55,
      ranksep = 0.55,
      pad = 0.20,
      margin = 0.05,
      outputorder = edgesfirst
    ];

    node [
      fontname = "Helvetica",
      fontsize = 18,
      color = "#40536D",
      penwidth = 1.4,
      style = "filled",
      margin = "0.16,0.10"
    ];

    edge [
      color = "#40536D",
      penwidth = 1.5,
      arrowsize = 0.8,
      fontname = "Helvetica",
      fontsize = 15
    ];

    q_norm [
      label = "Populations normales ?",
      shape = box,
      fillcolor = "#FFF2CC",
      width = 3.30,
      height = 0.65
    ];

    d_norm [
      label = "",
      shape = diamond,
      fillcolor = "#F2F2F2",
      width = 0.75,
      height = 0.48,
      fixedsize = true
    ];

    oui_norm [
      label = "oui",
      shape = box,
      fillcolor = "#E2F0D9",
      color = "#548235",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    non_norm [
      label = "non",
      group = "large",
      shape = box,
      fillcolor = "#FCE4D6",
      color = "#C00000",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    param [
      label = "Test paramétrique",
      shape = box,
      fillcolor = "#D9EAF7",
      width = 6,
      height = 0.72
    ];

    q_large [
      label = "Grands échantillons\n(n ≳ 30 par groupe)",
      group = "large",
      shape = box,
      fillcolor = "#FFF2CC",
      width = 3.65,
      height = 0.90
    ];

    d_large [
      label = "",
      group = "large",
      shape = diamond,
      fillcolor = "#F2F2F2",
      width = 0.75,
      height = 0.48,
      fixedsize = true
    ];

    oui_large [
      label = "oui",
      group = "large",
      shape = box,
      fillcolor = "#E2F0D9",
      color = "#548235",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    non_large [
      label = "non",
      group = "large",
      shape = box,
      fillcolor = "#FCE4D6",
      color = "#C00000",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    nonparam [
      label = "Test non-paramétrique",
      group = "nonparam",
      shape = box,
      fillcolor = "#D9EAF7",
      width = 6,
      height = 0.72
    ];

  
    q_paired_nonparam [
      label = "Echantillons appariés ?",
      group = "nonparam",
      shape = box,
      fillcolor = "#FFF2CC",
      width = 3.55,
      height = 0.90
    ];
  
    d_paired_nonparam [
      label = "",
      group = "nonparam",
      shape = diamond,
      fillcolor = "#F2F2F2",
      width = 0.75,
      height = 0.48,
      fixedsize = true
    ];

    oui_paired_nonparam [
      label = "oui",
      group = "nonparam",
      group = "signed_ranks",
      shape = box,
      fillcolor = "#E2F0D9",
      color = "#548235",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    non_paired_nonparam [
      label = "non",
      group = "nonparam",
      group = "WMW",
      shape = box,
      fillcolor = "#FCE4D6",
      color = "#C00000",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];



    q_paired_param [
      label = "Echantillons appariés ?",
      group = "param",
      shape = box,
      fillcolor = "#FFF2CC",
      width = 3.55,
      height = 0.90
    ];
  
    d_paired_param [
      label = "",
      group = "param",
      shape = diamond,
      fillcolor = "#F2F2F2",
      width = 0.75,
      height = 0.48,
      fixedsize = true
    ];

    oui_paired_param [
      label = "oui",
      group = "param",
      shape = box,
      fillcolor = "#E2F0D9",
      color = "#548235",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    non_paired_param [
      label = "non",
      group = "param",
      group = "var",
      shape = box,
      fillcolor = "#FCE4D6",
      color = "#C00000",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];


    q_var [
      label = "Variances des deux\npopulations égales ?",
      group = "param",
      group = "var",
      shape = box,
      fillcolor = "#FFF2CC",
      width = 3.55,
      height = 0.90
    ];
  

    d_var [
      label = "",
      group = "param",
      group = "var",
      shape = diamond,
      fillcolor = "#F2F2F2",
      width = 0.75,
      height = 0.48,
      fixedsize = true
    ];

    oui_var [
      label = "oui",
      group = "param",
      group = "var",
      shape = box,
      fillcolor = "#E2F0D9",
      color = "#548235",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    non_var [
      label = "non",
      group = "param",
      group = "var",
      shape = box,
      fillcolor = "#FCE4D6",
      color = "#C00000",
      width = 0.78,
      height = 0.48,
      fixedsize = true
    ];

    student [
      label = "Student",
      group = "param",
      group = "var",
      shape = box,
      fillcolor = "#D9EAF7",
      width = 1.5,
      height = 0.72
    ];

    welch [
      label = "Welch",
      group = "param",
      group = "var",
      shape = box,
      fillcolor = "#D9EAF7",
      width = 1.5,
      height = 0.72
    ];


    student_paired [
      label = "Student apparié",
      group = "param",
      shape = box,
      fillcolor = "#D9EAF7",
      width = 2.55,
      height = 0.72
    ];
  
  
    wilcox_mann_whitney [
      label = "Wilcoxon–Mann–Whitney",
      group = "nonparam",
      group = "WMW",
      shape = box,
      fillcolor = "#D9EAF7",
      width = 3.8,
      height = 0.72
    ];
  
    wilcox_signed_rank [
      label = "Rangs signés de Wilcoxon",
      group = "nonparam",
      group = "signed_ranks",
      shape = box,
      fillcolor = "#D9EAF7",
      width = 3.8,
      height = 0.72
    ];


    # Normality ------------------------------------------------------------
    q_norm -> d_norm;
    { rank = same; oui_norm; d_norm; non_norm; }
    oui_norm -> d_norm [style = invis, weight = 20];
    d_norm -> non_norm [style = invis, weight = 20];

    d_norm -> oui_norm [
      color = "#548235",
      arrowhead = none,
      constraint = false
    ];

    d_norm -> non_norm [
      color = "#C00000",
      arrowhead = none,
      constraint = false
    ];

    oui_norm -> param [color = "#548235"];
    non_norm -> q_large [color = "#C00000"];

    # { rank = same; param; q_large; }
    # param -> q_large [style = invis, weight = 10];

    # Sample sizes ----------------------------------------------
    q_large -> d_large;

    { rank = same; oui_large; d_large; non_large; }
    oui_large -> d_large [style = invis, weight = 20];
    d_large -> non_large [style = invis, weight = 20];

    d_large -> oui_large [
      color = "#548235",
      arrowhead = none,
      constraint = false
    ];

    d_large -> non_large [
      color = "#C00000",
      arrowhead = none,
      constraint = false
    ];

    oui_large -> param [
      color = "#548235",
      constraint = false
    ];

    non_large -> nonparam [color = "#C00000"];

    # Align pairing or not questions
    { rank = same; non_paired_param; oui_paired_nonparam; }
    non_paired_param -> oui_paired_nonparam [style = invis, weight = 20];

  
  
    # Pairing or not for parametric test ? ------------------------------------------------------------
    q_paired_param -> d_paired_param;
    { rank = same; oui_paired_param; d_paired_param; non_paired_param; }
    oui_paired_param -> d_paired_param [style = invis, weight = 20];
    d_paired_param -> non_paired_param [style = invis, weight = 20];

    d_paired_param -> oui_paired_param [
      color = "#548235",
      arrowhead = none,
      constraint = false
    ];

    d_paired_param -> non_paired_param [
      color = "#C00000",
      arrowhead = none,
      constraint = false
    ];

    oui_paired_param -> student_paired [color = "#548235", weight = 20];
    non_paired_param -> q_var [color = "#C00000"];

    # Pairing or not for non-nonparametric test ? ------------------------------------------------------------
  
    nonparam -> q_paired_nonparam
  
    q_paired_nonparam -> d_paired_nonparam;
    { rank = same; oui_paired_nonparam; d_paired_nonparam; non_paired_nonparam; }
    oui_paired_nonparam -> d_paired_nonparam [style = invis, weight = 20];
    d_paired_nonparam -> non_paired_nonparam [style = invis, weight = 20];

    d_paired_nonparam -> oui_paired_nonparam [
      color = "#548235",
      arrowhead = none,
      constraint = false
    ];

    d_paired_nonparam -> non_paired_nonparam [
      color = "#C00000",
      arrowhead = none,
      constraint = false
    ];

    # oui_paired_nonparam -> wilcox_signed_rank [color = "#548235"];
    oui_paired_nonparam -> wilcox_signed_rank [
      color = "#548235",
      weight = 20
    ];
    non_paired_nonparam -> wilcox_mann_whitney [color = "#C00000"];

    # { rank = same; param; q_large; }
    # param -> q_large [style = invis, weight = 10];

  
    # Égalité des variances -------------------------------------------------
    param -> q_paired_param;
    q_var -> d_var;

    { rank = same; oui_var; d_var; non_var; }
    oui_var -> d_var [style = invis, weight = 20];
    d_var -> non_var [style = invis, weight = 20];

    d_var -> oui_var [
      color = "#548235",
      arrowhead = none,
      constraint = false
    ];

    d_var -> non_var [
      color = "#C00000",
      arrowhead = none,
      constraint = false
    ];

    oui_var -> student [color = "#548235"];
    non_var -> welch [color = "#C00000"];

    { rank = same; param; nonparam; }

  
  
    { rank = same; student_paired; student; welch; wilcox_signed_rank; wilcox_mann_whitney ; }
    student_paired -> student [style = invis, weight = 5];
    student -> welch [style = invis, weight = 5];
    welch -> wilcox_mann_whitney [style = invis, weight = 5];
    wilcox_signed_rank -> wilcox_mann_whitney [style = invis, weight = 5];
  }
  }"
  
  DiagrammeR::grViz(
    diagram = code_dot,
    width = width,
    height = height,
    engine = "dot"
  )
}


# Optional export ---------------------------------------------------------
#
# Examples :
#   flowchart <- mean_comparison_test_flowchart()
#   export_flowchart(flowchart, "logigramme.svg")
#   export_flowchart(flowchart, "logigramme.png")
#   export_flowchart(flowchart, "logigramme.pdf")
export_flowchart <- function(flowchart, filename) {
  
  extension <- tolower(tools::file_ext(filename))
  
  if (!extension %in% c("svg", "png", "pdf")) {
    stop("L'extension doit être .svg, .png ou .pdf.")
  }
  
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop(
      "Le package 'DiagrammeRsvg' est nécessaire pour l'export. ",
      "Exécutez install.packages('DiagrammeRsvg')."
    )
  }
  
  texte_svg <- DiagrammeRsvg::export_svg(flowchart)
  
  if (extension == "svg") {
    writeLines(text = texte_svg, con = filename, useBytes = TRUE)
  } else {
    if (!requireNamespace("rsvg", quietly = TRUE)) {
      stop(
        "Le package 'rsvg' est nécessaire pour l'export PNG ou PDF. ",
        "Exécutez install.packages('rsvg')."
      )
    }
    
    svg_brut <- charToRaw(enc2utf8(texte_svg))
    
    if (extension == "png") {
      rsvg::rsvg_png(svg = svg_brut, file = filename)
    } else {
      rsvg::rsvg_pdf(svg = svg_brut, file = filename)
    }
  }
  
  invisible(normalizePath(filename, mustWork = FALSE))
}


# Inclusion compatible with Quarto HTML and PDF -----------------------------
#
# This function never sends the htmlwidget object in a pdf rendering PDF.
# Instead, it exports the flowchart as a vectorial image, which is later inserted with knitr.
#
# In the Quarto chunk, the insersion size can be managed with the out-width parameter
#   #| out-width: 100%
include_mean_test_choice <- function(
    folder = "figures/generated",
    filename = "mean_test_choice_flowchart",
    regenerate = TRUE) {
  
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Missing package: knitr")
  }
  
  if (!requireNamespace("xfun", quietly = TRUE)) {
    stop("Missing package: xfun")
  }
  
  # Dossier de création, relatif à la racine d’exécution Quarto
  dir.create(
    folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (knitr::is_latex_output()) {
    extension <- "pdf"
  } else {
    # PNG pour HTML et DOCX
    extension <- "png"
  }
  
  # Chemin réel utilisé par R pour créer le fichier
  output_file <- normalizePath(
    file.path(folder, paste0(filename, ".", extension)),
    winslash = "/",
    mustWork = FALSE
  )
  
  if (regenerate || !file.exists(output_file)) {
    flowchart <- mean_comparison_test_flowchart()
    export_flowchart(flowchart, output_file)
  }
  
  if (!file.exists(output_file)) {
    stop("Figure not generated: ", output_file)
  }
  
  # Répertoire du fichier .qmd actuellement traité
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
  
  # Chemin de la figure relatif au fichier .qmd
  inclusion_file <- xfun::relative_path(
    output_file,
    dir = input_dir
  )
  
  knitr::include_graphics(
    path = inclusion_file,
    rel_path = FALSE,
    error = FALSE
  )
}

# Use in RStudio :
# flowchart <- mean_comparison_test_flowchart()
# flowchart

# In a chunk R Markdown HTML :
# mean_comparison_test_flowchart()
#
# For a PDF document / report, it is generalluy simpler to export the flowchart in pdf or png format and to insert it in the doc with knitr::include_graphics().

# Plot the diagram

# Exemple de chunk Quarto :
#
# ```{r}
# #| label: fig-choix-test-moyennes
# #| fig-cap: "Choix d'un test de comparaison de deux moyennes."
# #| echo: false
# #| out-width: 100%
#
# source("R/mean-compa-choice_flowchart.R")
# include_mean_test_choice(
#   folder = "figures/generated",
#   filename = "mean_test_choice_flowchart"
# )
# ```

