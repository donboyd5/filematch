#' Create arcs for a minimum cost flow problem
#'
#' @param dbtoa list returned from `FNN:get.knnx()`.
#' @param datob list returned from `FNN:get.knnx()`.
#' @param nodes dataframe with one row per row in afile and one row per row in bfile, with columns file, node, abrow.
#'
#' @return dataframe with arcs for a minimum cost flow problem.
#' @export
#'
#' @examples
#' library(filematch)
#' data(afile)
get_arcs <- function(dbtoa, datob, nodes) {
  # create tibbles for btoa:
  #   idx has crossing of a_row=1:nrow(a), neighbor=1:k indexes, b_row=relevant row of b
  #   dist has crossing of a_row=1:nrow(a), neighbor=1:k indexes, dist=relevant distance
  # create tibble for btoa with a_row=1:nrow(a), neighbor=1:k, b_row=relevant row or b

  dbtoa_idx <- tibble::tibble(
    a_row = rep(1:nrow(dbtoa$nn.index), each = ncol(dbtoa$nn.index)),
    neighbor = rep(1:ncol(dbtoa$nn.index), nrow(dbtoa$nn.index)),
    b_row = c(t(dbtoa$nn.index))
  )

  dbtoa_dist <- tibble::tibble(
    a_row = rep(1:nrow(dbtoa$nn.dist), each = ncol(dbtoa$nn.dist)),
    neighbor = rep(1:ncol(dbtoa$nn.dist), nrow(dbtoa$nn.dist)),
    dist = c(t(dbtoa$nn.dist))
  )

  dbtoa_arcs <- dplyr::left_join(dbtoa_idx, dbtoa_dist,
    by = c("a_row", "neighbor")
  )
  # by = dplyr::join_by(
  #   x$a_row==y$a_row,
  #   x$neighbor==y$neighbor))
  # dbtoa_arcs

  datob_idx <- tibble::tibble(
    b_row = rep(1:nrow(datob$nn.index), each = ncol(datob$nn.index)),
    neighbor = rep(1:ncol(datob$nn.index), nrow(datob$nn.index)),
    a_row = c(t(datob$nn.index))
  )

  datob_dist <- tibble::tibble(
    b_row = rep(1:nrow(datob$nn.dist), each = ncol(datob$nn.dist)),
    neighbor = rep(1:ncol(datob$nn.dist), nrow(datob$nn.dist)),
    dist = c(t(datob$nn.dist))
  )

  datob_arcs <- dplyr::left_join(datob_idx, datob_dist,
    by = c("b_row", "neighbor")
  )
  # by = dplyr::join_by(
  #   x$b_row==y$b_row,
  #   x$neighbor==y$neighbor))

  # arcs: combine and keep unique arcs, keeping track of their neighbor status
  arcs1 <- dplyr::bind_rows(
    dbtoa_arcs |>
      dplyr::mutate(src = "btoa"),
    datob_arcs |>
      dplyr::mutate(src = "atob")
  ) |>
    dplyr::select("a_row", "b_row", "neighbor", "dist", "src")

  # keeping the neighbor number can help in figuring out the quality of a match
  # the fastest way I could find to do this is
  #   (1) get unique a_row, b_row arcs and their distances, which by definition are unique
  #   (2) merge back to get the atob and btoa neighbor numbers;
  # note that I waste some memory by creating interim files

  arcs_distinct <- arcs1 |>
    dplyr::select("a_row", "b_row", "dist") |>
    dplyr::distinct()

  arcs_neighbors <- arcs_distinct |>
    dplyr::left_join(
      arcs1 |>
        dplyr::filter(.data$src == "btoa") |>
        dplyr::select("a_row", "b_row", "btoa_neighbor" = "neighbor"),
      by = c("a_row", "b_row")
    ) |>
    # by = dplyr::join_by(
    #   x$a_row==y$a_row, x$b_row==y$b_row)) |>
    dplyr::left_join(
      arcs1 |>
        dplyr::filter(.data$src == "atob") |>
        dplyr::select("a_row", "b_row", "atob_neighbor" = "neighbor"),
      by = c("a_row", "b_row")
    ) |>
    # by = dplyr::join_by(x$a_row==y$a_row, x$b_row==y$b_row)) |>
    # prefer btoa_neighbor for later analysis of how far we had to go to find matches
    dplyr::mutate(neighbor = ifelse(
      is.na(.data$btoa_neighbor),
      .data$atob_neighbor,
      .data$btoa_neighbor))

  # create the final arcs file: bring in node numbers
  arcs <- arcs_neighbors |>
    dplyr::left_join(nodes |>
                       dplyr::filter(file == "B") |>
                       dplyr::select("b_node" = "node", "b_row" = "abrow"),
      by = c("b_row")
    ) |>
    # by = dplyr::join_by(x$b_row==y$b_row)) |>
    dplyr::left_join(nodes |>
                       dplyr::filter(file == "A") |>
                       dplyr::select("a_node" = "node", "a_row" = "abrow"),
      by = c("a_row")
    ) |>
    # by = dplyr::join_by(x$a_row==y$a_row)) |>
    dplyr::select(
      "a_node", "b_node", "a_row", "b_row", "dist",
      "neighbor", "btoa_neighbor", "atob_neighbor"
    ) |>
    # Convert distances, which are in standard deviation units because of scaling,
    # to integers because the minimum cost flow algorithms require integer inputs.
    # Multiply by 100 to spread them out (otherwise we might have 0, 1, 2, 3 standard deviations)
    # I use 100 rather than a larger number, to keep the costs relatively small because
    # small costs to be important for minimum cost flow solvers.
    dplyr::mutate(dist = as.integer(.data$dist * 100.)) |>
    dplyr::arrange(.data$a_node, .data$b_node)

  return(arcs)
}


#' Get k nearest neighbors for each of two dataframes
#'
#' @description
#' Get k nearest neighbors of afile and k nearest neighbors of bfile, based on distances between common xvars.
#'
#'
#' @param afile Dataframe with xvars; other columns allowed.
#' @param bfile Dataframe with xvars; other columns allowed.
#' @param xvars Character vector of column names.
#' @param k     Integer scalar: number of nearest neighbors to get distances for.
#'
#' @return A list with two elements, each of which has two matrices. The elements are dbtoa (distance from B donor records to A recipient records) and datob (distance from A donor to B recipient). Each element has two matrices, a matrix of indexes for the nearest neighbors and a matrix with distances.
#' @export
#'
#' @examples
#' data(afile)
#' data(bfile)
#' xvars <- c("age", "hoursworked", "income")
#' res <- get_distances(afile, bfile, xvars, k = 10)
#' str(res)
get_distances <- function(afile, bfile, xvars, k = NULL) {
  # determine how many near-neighbors to get, if not specified
  if (is.null(k)) {
    maxrecs <- max(nrow(afile), nrow(bfile))
    minrecs <- min(nrow(afile), nrow(bfile))
    k <- round(maxrecs) * .05
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

  # Uses the "brute" algorithm of FNN:get.knnx as that has proved fastest for this kind of problem in testing by Boyd.

  # k nearest distances for donating from file B to file A (dbtoa)
  # each A record has k nearest neighbors in the B file
  # result matrices have same # rows as afile
  dbtoa <- FNN::get.knnx(
    # select(!!xvars) also works
    bfile |> dplyr::select(tidyselect::all_of(xvars)) |> scale(),
    afile |> dplyr::select(tidyselect::all_of(xvars)) |> scale(),
    k = k, algorithm = "brute"
  )

  # k nearest distances for donating from file A to file B (datob)
  # each B record has k nearest neighbors in the A file
  # result matrices have same # rows as bfile
  datob <- FNN::get.knnx(
    afile |> dplyr::select(tidyselect::all_of(xvars)) |> scale(),
    bfile |> dplyr::select(tidyselect::all_of(xvars)) |> scale(),
    k = k, algorithm = "brute"
  )

  return(list(dbtoa = dbtoa, datob = datob))
}


#' Create nodes for a minimum cost flow problem
#'
#' @description
#' Creates a combined dataframe from afile and bfile. Its node column and supply column will be used as inputs to a minimum cost flow problem.
#'
#' @param afile dataframe with columns id, node, a_row, weight, weightadj, iweight
#' @param bfile dataframe with columns id, node, a_row, weight, weightadj, iweight
#'
#' @return dataframe with afile and bfile concatenated, with integer supply values for the minimum cost flow problem. The afile supplies will be negative because it will receive weights from the bfile. The bfile is the donor and its supplies will be positive. The supplies will sum to zero.
#' @export
#'
#' @examples
#' library(filematch)
#' data(afile)
get_nodes <- function(afile, bfile) {
  nodes <- dplyr::bind_rows(
    afile |>
      dplyr::select("id", "node", "abrow" = "a_row", "weight", "weightadj", "iweight") |>
      # note the minus sign below because the A file demands weights
      dplyr::mutate(file = "A", supply = -.data$iweight),
    bfile |>
      dplyr::select("id", "node", "abrow" = "b_row", "weight", "weightadj", "iweight") |>
      # note NO minus sign below because the B file supplies weights
      dplyr::mutate(file = "B", supply = .data$iweight)
  ) |>
    dplyr::select("id", "node", "file", "abrow", tidyselect::everything())

  return(nodes)
}

#' Construct the AB file
#'
#' @param arcs dataframe with columns...
#' @param nodes dataframe with columns
#' @param flows vector...
#' @param afile dataframe with ...
#' @param bfile dataframe with ...
#' @param idvar character column name
#' @param wtvar character column name
#' @param xvars character vector of column names
#' @param yvars character vector of column names
#' @param zvars character vector of column names
#'
#' @return list with ...
#' @export
#'
#' @examples
#' library(filematch)
#' data(afile)
get_abfile <- function(arcs, nodes, flows, afile, bfile, idvar, wtvar, xvars, yvars, zvars) {
  print("preparing base abfile...")

  # create an interleaved vector of xvars and the b xvars, to make them easy to view later
  ilxvars <- c(rbind(xvars, paste0("b_", xvars)))

  abfile <- arcs |>
    dplyr::mutate(weight = flows) |>
    dplyr::filter(.data$weight > 0) |> # drop potential matches that weren't used

    # get the id and weight variables for the a and b files from the nodes file
    dplyr::left_join(
      nodes |>
        dplyr::filter(file == "A") |>
        dplyr::select("a_id" = "id", "a_row" = "abrow", "a_weight" = "iweight"),
      by = c("a_row")
      # by = dplyr::join_by(a_row) # don't use join_by in this package
    ) |>
    dplyr::left_join(
      nodes |>
        dplyr::filter(file == "B") |>
        dplyr::select("b_id" = "id", "b_row" = "abrow", "b_weight" = "iweight"),
      by = c("b_row")
    ) |>

    # convert the a and b id variable names to user-recognizable names
    dplyr::select("a_node", "b_node", "a_id", "b_id", "neighbor", "a_weight", "b_weight",
                  "dist", "weight") |>
    dplyr::rename(
      # use the original idvar name, with a prefix
      !!paste0("a_", idvar) := "a_id",
      !!paste0("b_", idvar) := "b_id"
    ) |>
    # bring in each file's xvars, plus the yvars from a and zvars from b
    dplyr::left_join(
      afile |>
        dplyr::select(-tidyselect::all_of(wtvar)) |>
        dplyr::rename(!!paste0("a_", idvar) := tidyselect::all_of(idvar)), # rlang::sym(idvar)),
      by = dplyr::join_by(!!paste0("a_", idvar))
    ) |>
    dplyr::left_join(
      bfile |>
        dplyr::select(-tidyselect::all_of(wtvar)) |>
        dplyr::rename(!!paste0("b_", idvar) := tidyselect::all_of(idvar)) |> # rlang::sym(idvar)) |>
        dplyr::rename(!!!stats::setNames(xvars, paste0("b_", xvars))), # give bfile xvars a b prefix
      by = dplyr::join_by(!!paste0("b_", idvar))
    ) |>
    # move variables around so that it is easier visually to compare the afile xvars to the bfile xvars
    dplyr::relocate(dist, .after = paste0("b_", idvar)) |>  # rlang::sym(paste0("b_", idvar))) |>
    dplyr::relocate(tidyselect::all_of(ilxvars), .after = tidyselect::last_col()) |> # interleaved xvars
    dplyr::relocate(tidyselect::all_of(yvars), .after = tidyselect::last_col()) |>
    dplyr::relocate(tidyselect::all_of(zvars), .after = tidyselect::last_col()) |>
    dplyr::arrange(.data$a_node, .data$dist)

  return(abfile)
}


#' Prepare the afile and bfile
#'
#' @param afile Dataframe...
#' @param bfile Dataframe...
#' @param idvar Character column name
#' @param wtvar Character column name
#' @param xvars Character vector of column names
#' @param k Integer number of nearest neighbors to find
#'
#' @return a list with nodes, arcs, and preptime
#' @export
#'
#' @examples
#' library(filematch)
#' data(afile)
prepab <- function(afile, bfile, idvar, wtvar, xvars, k = NULL) {
  a <- proc.time()

  # flows are from B to A
  # create a node file
  awtsum <- sum(afile[[wtvar]])
  bwtsum <- sum(bfile[[wtvar]])
  abratio <- awtsum / bwtsum
  print(paste0("initial ratio of sum of afile weights to bfile weights is: ", round(abratio, digits = 3)))
  print("bfile weights will be adjusted as needed so that bfile weight sum equals afile weight sum")
  if (abratio < 0.75 || abratio > 1.25) {
    print("however, large difference in sums suggests caution needed")
  }

  afile1 <- afile |>
    dplyr::select(tidyselect::all_of(c(idvar, wtvar, xvars))) |>
    dplyr::rename(
      id = !!as.symbol(idvar), # investigate a consistent way of converting strings to symbols
      weight = !!as.symbol(wtvar)
    ) |>
    dplyr::mutate(
      file = "A",
      a_row = dplyr::row_number(),
      node = dplyr::row_number(),
      weightadj = .data$weight,
      iweight = round(.data$weightadj) |> as.integer()
    )

  bfile1 <- bfile |>
    dplyr::select(tidyselect::all_of(c(idvar, wtvar, xvars))) |>
    dplyr::rename(
      id = !!as.symbol(idvar),
      weight = !!as.symbol(wtvar)
    ) |>
    dplyr::mutate(
      file = "B",
      b_row = dplyr::row_number(),
      node = dplyr::row_number() + nrow(afile),
      weightadj = .data$weight * sum(afile[[wtvar]]) / sum(.data$weight),
      iweight = round(.data$weightadj) |> as.integer()
    )

  # print("balancing integer weights by adjusting bfile...")
  # this is rough - come up with a better way later
  awtsum <- sum(afile1$iweight)
  bwtsum <- sum(bfile1$iweight)
  diffba <- bwtsum - awtsum

  addval <- dplyr::case_when(
    diffba < 0 ~ 1,
    diffba > 0 ~ -1,
    TRUE ~ 0
  )

  bfile1 <- bfile1 |>
    dplyr::mutate(iweight = ifelse(dplyr::row_number() <= abs(diffba),
      .data$iweight + addval,
      .data$iweight
    ))

  # get distances
  dists <- get_distances(afile1, bfile1, xvars, k)

  nodes <- get_nodes(afile1, bfile1)
  arcs <- get_arcs(dbtoa = dists$dbtoa, datob = dists$datob, nodes = nodes)

  b <- proc.time()
  preptime <- (b - a)[3]

  return(list(nodes = nodes, arcs = arcs, preptime = preptime))
}

#' Match A and B files
#'
#' @param afile Dataframe ...
#' @param bfile Dataframe ...
#' @param idvar Character column name
#' @param wtvar Character column name
#' @param xvars Character vector of column names
#' @param yvars Character vector of column names
#' @param zvars Character vector of column names
#' @param k k Integer number of nearest neighbors to find
#'
#' @return list with prep_list, mcfresult, and abfile
#' @export
#'
#' @examples
#' library(filematch)
#' data(afile)
matchab <- function(afile, bfile, idvar, wtvar, xvars, yvars, zvars, k = NULL) {
  print("preparing nodes and arcs...")
  prep_list <- prepab(afile,
    bfile,
    idvar = idvar,
    wtvar = wtvar,
    xvars = xvars,
    k = k
  )
  print(paste0("# seconds to prepare nodes and arcs: ", round(prep_list$preptime, 3)))

  a <- proc.time()
  # allowable_algorithms <- c("NetworkSimplex", "CostScaling", "CapacityScaling", "CycleCancelling")
  mcfresult <- rlemon::MinCostFlow(
    # flows are from B to A -- B has supply nodes, A has demand nodes
    arcSources = prep_list$arcs$b_node,
    arcTargets = prep_list$arcs$a_node,
    arcCapacities = rep(max(abs(prep_list$nodes$supply)), nrow(prep_list$arcs)),
    arcCosts = prep_list$arcs$dist,
    nodeSupplies = prep_list$nodes$supply,
    numNodes = nrow(prep_list$nodes),
    algorithm = "NetworkSimplex" # NetworkSimplex seems fastest for these problems
  )
  b <- proc.time()
  mcfresult$mcftime <- (b - a)[3]
  print(paste0("# seconds to solve minimum cost flow problem: ", round(mcfresult$mcftime, 3)))
  print(paste0("Solution status: ", mcfresult$feasibility))

  abfile <- get_abfile(
    arcs = prep_list$arcs,
    nodes = prep_list$nodes,
    flows = mcfresult$flows,
    afile = afile, bfile = bfile, idvar = idvar, wtvar = wtvar,
    xvars = xvars, yvars = yvars, zvars = zvars
  )

  return(list(prep_list = prep_list, mcfresult = mcfresult, abfile = abfile))
}
