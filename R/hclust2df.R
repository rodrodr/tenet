#
#
# This is the function hclust2df.
#
#' @param data a tree network description in one of numerous forms (see
#' details).
#' @param ... other arguments that will be passed on to as_treenetdf
#'
#' @description
#' The `treeNetwork` function uses a 'native' data format that consists of a data
#' frame with minimally 2 vectors/columns, one named `'nodeId'` and one named
#' `'parentId'`. Other columns in the data frame are also passed on to the
#' JavaScript code and attached to the elements in the D3 visualization so that
#' they can potentially be accessed by other JavaScript functions. This is an
#' advantageous format because:
#' - it's an easy to use and understand R-like format
#' - a hierarchical network can be succinctly defined by a list of each unique
#' node and its parent node
#' - since each row defines a unique node, additional columns can be added to
#' add node-specific properties
#' - in a hierarchical network, every link/edge can be uniquely identified by
#' the node which it leads to, therefore each link/edge can also be specifically
#' addressed by adding columns for formatting of the incoming link
#'
#' `hclust2df` can convert from any of the following data types:
#' - `leafpathdf` (table)--`parent|parent|node`--`data.frame`
#' - hierarchical nested list (JSON)
#' - `hclust`
#' - `data.tree` Node
#' - igraph
#' - ape `phylo`
#'
#' @importFrom data.tree ToDataFrameNetwork
#' @importFrom igraph as_data_frame
#' @importFrom stats na.exclude
#' @importFrom stats setNames
#'
#' @import stats
#'
#' @md
hclust2df <- function(data, ...) {
  clustparents <-
    unlist(sapply(seq_along(data$height), function(i) {
      parent <- which(i == data$merge)
      parent <- ifelse(parent > nrow(data$merge),
                       parent - nrow(data$merge), parent)
      as.integer(ifelse(length(parent) == 0, NA_integer_, parent))
    }))
  
  leaveparents <-
    unlist(sapply(seq_along(data$labels), function(i) {
      parent <- which(i * -1 == data$merge)
      parent <- ifelse(parent > nrow(data$merge), parent -
                         nrow(data$merge), parent)
      as.integer(ifelse(length(parent) == 0, NA, parent))
    }))
  
  df <-
    data.frame(
      nodeId = 1:(length(data$height) + length(data$labels)),
      parentId = c(clustparents, leaveparents),
      name = c(rep('', length(data$height)), data$labels),
      height = c(data$height, rep(0, length(data$labels)))
    )
  
  return(df)
}
