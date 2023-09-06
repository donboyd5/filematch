#' Good approach
#'
#' Fast matching of records
#' @param adf dataframe with A records
#' @param bdf dataframe with B records
#' @param vida string, A id variable name default "ida"
#' @param vidb string, b id variable name default "idb"
#' @param vweighta string, A weight variable name default "weighta"
#' @param vweightb string, B weight variable name default "weightb"
#' @param vranka string, A rank variable name default "ranka"
#' @param vrankb string, B rank variable name default "rankb"
#' @return ab dataframe
#' @export
matchrecs <- function(adf, bdf, vida = "ida", vidb = "idb", vweighta = "weighta", vweightb = "weightb", vranka = "ranka", vrankb = "rankb") {
  .Call(`_filematch_internal_matchrecs`, adf, bdf, vida, vidb, vweighta, vweightb, vranka, vrankb)
}
