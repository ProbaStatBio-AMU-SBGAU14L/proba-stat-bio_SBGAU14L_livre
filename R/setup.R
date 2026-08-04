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
  scipen = 12,
  OutDec = "."
)

## Format values returned by inline R expressions
format_inline_output <- function(x) {
  if (is.numeric(x)) {
    x <- format(
      x,
      digits = getOption("digits"),
      scientific = NA,
      big.mark = "\u202F",
      decimal.mark = getOption("OutDec"),
      trim = TRUE
    )
  }
  
  paste(x, collapse = ", ")
}

## Replace the default knitr hook for inline R expressions
knitr::knit_hooks$set(
  inline = format_inline_output
)
