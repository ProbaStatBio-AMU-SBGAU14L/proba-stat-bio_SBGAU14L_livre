## Settings used by quarto for the whole book
## For this, each chapter needs to start by sourcing this file with the following command
##
## source("R/setup.R", local = knitr::knit_global())

library(knitr)

# knit_hooks$set(inline = function(x) {
#   prettyNum(x, big.mark = "\u202f", decimal.mark = ".")
# })

## Set global output options
options(
  digits = 4,
  scipen = 7,
  OutDec = "."
)

## Format values returned by inline R expressions
format_inline_output <- function(x) {
  
  ## Format numeric values according to the global R options
  if (is.numeric(x)) {
    formatted_values <- format(
      x,
      digits = getOption("digits"),
      scientific = NA,
      big.mark = "\u202F",
      decimal.mark = getOption("OutDec"),
      trim = TRUE
    )
    
    ## Separate the elements of a numeric vector with commas
    return(paste(formatted_values, collapse = ", "))
  }
  
  ## Preserve character strings exactly as they were generated
  if (is.character(x) && length(x) == 1L) {
    return(x)
  }
  
  ## Collapse other non-numeric vectors without altering their elements
  paste(as.character(x), collapse = ", ")
}

## Register the custom inline-output hook
knitr::knit_hooks$set(
  inline = format_inline_output
)
