#' Calculate annual energy budget, single function
#' @param biophys.inputs Object created from running \code{\link[biophys]{biophys.prep}}
#' @param PFT.out Hourly potential foraging times. Pass results from \code{\link[biophys]{pft}}
#' @param metabolism.out Hourly metabolic measures. Pass results from \code{\link[biophys]{metabolism}}
#'
#' @usage a.budget(biophys.inputs)
#'
#' @export
#' @author Bill Peterman <Bill.Peterman@@gmail.com>
#' @return A list that containing a raster stack that includes (1) annual energy budget; (2) total annual metabolic rate; (3) annual potential foraging time; (4) total annual consumption, as well as the annual values in vector format
#' @details This is a global convenience function to execute \code{\link[biophys]{op.temp.body}}, \code{\link[biophys]{pft}}, \code{\link[biophys]{metabolism}}, and \code{\link[biophys]{budget}}.
#'
a.budget <- function(biophys.inputs) {
  temp.out <- op.body.temp(biophys.inputs)

  PFT.out <- pft(biophys.inputs,
                 temp.out)

  metabolism.out <- metabolism(biophys.inputs,
                               temp.out)

  annual.budget <- budget(biophys.inputs,
                          PFT.out,
                          metabolism.out)
  return(annual.budget)
}
