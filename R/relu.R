#####  relu  #####
#' Compute relu function on vector
#' @description Compute relu function on vector
#' @usage relu(x)
#' @param x A numeric vector
#' @return A numeric vector the same length as x.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # No example yet
#' @export

relu <- function(x) pmax(x, 0)