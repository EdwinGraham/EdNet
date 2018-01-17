#####  predictedClass  #####
#' Converts class probabilities into a predicted class
#' @description For multi-class learning, takes matrix of class probabilities and returns the predicted (most probable) class.
#' @usage predictedClass(classProbs, classes=NULL)
#' @param classProbs A matrix of probabilities with rows representing train/dev/test examples and columns representing classes.
#' @param classes An optional character vector representing the names of the classes. If left blank the names are taken from the column names of classProbs.
#' @return A factor.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # No example yet
#' @export

predictedClass <- function(classProbs, classes=NULL){
  if(!is.matrix(classProbs)) stop("classProbs should be matrix")
  if(is.null(classes)){
    if(is.null(colnames(classProbs))) stop("classProbs should have column names denoting classes or else the classes parameter should be specified")
    classes <- colnames(classProbs)
  } else{
    if(!is.character(classes)) stop("classes should be character")
    if(ncol(classProbs) != length(classes)) stop("width of classProbs does not match length of classes")
  }

  return(factor(classes[apply(classProbs, 1, which.max)]))
}