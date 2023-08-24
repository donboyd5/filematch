

get_distances <- function(afile, bfile, xvars, k=NULL){

  # determine how many near-neighbors to get, if not specified
  if(is.null(k)){
    maxrecs <- max(nrow(afile), nrow(bfile))
    minrecs <- min(nrow(afile), nrow(bfile))
    k = round(maxrecs) * .05
    k <- min(k, 1000) # k can never be more than 1000
    k <- max(k, 10) # k always must be at least 10
    k <- min(k, minrecs) # but k cannot be less than the number of rows in the shorter file
  }
  print(paste0("k: ", k))

  # distance computations:
  # scale input data to mean=0, sd=1 before computing distances
  # compute nearest neighbors two ways to ensure that we have:
  #   arcs from each B record to k A records,
  #   arcs from each A record to k B records
  # this does not guarantee feasibility but should help


  # k nearest distances for donating from file B to file A (dbtoa)
  # result matrices have same # rows as afile
  dbtoa <- FNN::get.knnx(bfile |> dplyr::select(!!xvars) |> scale(),
                         afile |> dplyr::select(!!xvars) |> scale(),
                         k=k, algorithm="brute")

  # k nearest distances for donating from file A to file B (datob)
  # result matrices have same # rows as bfile
  datob <- FNN::get.knnx(afile |> dplyr::select(all_of(xvars)) |> scale(), # scale to mean=0, sd=1 and compute distances
                         bfile |> dplyr::select(all_of(xvars)) |> scale(),
                         k=k, algorithm="brute") # brute is fastest algorithm based on testing


  return(list(dbtoa=dbtoa, datob=datob))
}
