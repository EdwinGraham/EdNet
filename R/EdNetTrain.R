#####  EdNetTrain  #####
#' Train a neural network model
#' @description Train a neural network model
#' @usage EdNetTrain(
#'   X,
#'   Y,
#'   family=NULL,
#'   learning_rate=0.05,
#'   num_epochs,
#'   hidden_layer_dims=NULL,
#'   hidden_layer_activations=NULL,
#'   optimiser="GradientDescent",
#'   keep_prob=NULL,
#'   input_keep_prob=NULL,
#'   alpha=0,
#'   lambda=0,
#'   mini_batch_size=NULL,
#'   dev_set=NULL,
#'   beta1=ifelse(optimiser \%in\% c("Momentum", "Adam"), 0.9, NULL),
#'   beta2=ifelse(optimiser \%in\% c("RMSProp", "Adam"), 0.999, NULL),
#'   epsilon=ifelse(optimiser \%in\% c("RMSProp", "Adam"), 1E-8, NULL),
#'   print_every_n=100L,
#'   seed=1984L,
#'   plot=TRUE,
#'   checkpoint=NULL,
#'   keep=FALSE
#' )
#' @param X A matrix with rows as training examples and columns as input features
#' @param Y A matrix with rows as training examples and columns as target values
#' @param family Type of regression to be performed. One of "binary", "multiclass", "gaussian", "poisson", "gamma", "tweedie".
#' Will be ignored if starting from a checkpoint model. Alternatively you can specify a named list with the following elements:
#' "family" - a character of length 1 for reference only (must use "multiclass" if target values have dimension > 1);
#' "link.inv" - a function (the inverse link function for activating the output layer);
#' "costfun" - a function with parameters 'Y'and 'Y_hat' representing the cost function to be minimised;
#' "gradfun" - a function with parameters 'Y'and 'Y_hat' representing the gradient of the cost function with respect to the linear, pre-activation, matrix in the output layer.
#' @param learning_rate Learning rate to use.
#' @param num_epochs Number of epochs (complete pass through training data) to be performed.
#' If using mini-batches the number of iterations may be much higher.
#' @param hidden_layer_dims Integer vector representing the dimensions of the hidden layers.
#' Should not be specified if starting from a checkpoint model.
#' @param hidden_layer_activations Character vector the same length as the \code{hidden_layer_dims} vector or length 1.
#' If length is 1 the same activation function will be used for all hidden layers.
#' Should only contain "relu" or "tanh" as these are the only supported activation functions for hidden layers.
#' Should not be specified if starting from a checkpoint model.
#' @param optimiser Type of optimiser to use. One of "GradientDescent", "Momentum", "RMSProp", "Adam".
#' @param keep_prob Keep probabilities for applying drop-out in hidden layers.
#' Either a constant or a vector the same length as the \code{hidden_layer_dims} vector. If NULL no drop-out is applied.
#' @param input_keep_prob Keep probabilities for applying drop-out in the input layer.
#' Needs to be a single constant. If NULL no drop-out is applied.
#' @param alpha L1 regularisation term.
#' @param lambda L2 regularisation term.
#' @param mini_batch_size Size of mini-batches to use. If NULL full training set is used for each iteration.
#' @param dev_set Integer vector representing hold-out data. Integers refer to individual training examples in the order presented in X.
#' @param beta1 Exponential weighting term for gradients when using Momentum or Adam optimisation.
#' @param beta2 Exponential weighting term for square pf gradients when using RMSProp or Adam optimisation.
#' @param epsilon Small number used for numerical stability to prevent division by zero when using RMSProp or Adam optimisation.
#' @param print_every_n Print info to the log every n epochs. If NULL, no printing is done.
#' @param seed Random seed to use for repeatability.
#' @param plot Plot cost function when printing to log.
#' @param checkpoint Rather than initialise new parameters, start from a checkpoint model.
#' @param keep keep X and Y data in final output.
#' @return An object of class EdNetModel.
#' @author Edwin Graham <edwingraham1984@gmail.com>
#' @examples
#' # No example yet
#' @export

EdNetTrain <- function(X,
                       Y,
                       family=NULL,
                       learning_rate=0.05,
                       num_epochs,
                       hidden_layer_dims=NULL,
                       hidden_layer_activations=NULL,
                       optimiser="GradientDescent",
                       keep_prob=NULL,
                       input_keep_prob=NULL,
                       alpha=0,
                       lambda=0,
                       mini_batch_size=NULL,
                       dev_set=NULL,
                       beta1,
                       beta2,
                       epsilon,
                       print_every_n=100L,
                       seed=1984L,
                       plot=TRUE,
                       checkpoint=NULL,
                       keep=FALSE){

  
  # Validation and setup of simple helper parameters ------------------------
  ## Check X and Y are of the correct format
  if(!is.matrix(X)) stop("X must be a matrix.")
  if(!is.matrix(Y)) stop("Y must be a matrix.")
  
  ## Function takes data formatted with columns as features and rows as observations
  ## Code throughout the function uses the opposite approach
  X <- t(X)
  Y <- t(Y)
  
  ## Number of training examples
  m <- dim(X)[2]
  
  ## Check family of regression task is supported
  if(dim(Y)[2] != m) stop("X and Y must have the same number of columns. These should be the number of training examples.")
  if(!is.null(family)){
    if(is.character(family)){
      if(length(family) != 1) stop("If specifying a character vector for 'family' it must be of length 1.")
      if(!family %in% c("binary", "multiclass", "gaussian", "poisson", "gamma", "tweedie")){
        stop("Only the following are supported for family: binary, multiclass, gaussian, poisson, gamma, tweedie.")
      } else{
        family <- outputFamily(family)
      }
    } else if(is.list(family)){
      if(!all(c("family", "link.inv", "costfun", "gradfun") %in% names(family))) stop("If specifying family as a list it must be named and include the following: family, link.inv, costfun, gradfun.")
      if(!is.character(family$family)){
        stop("The element named 'family' in the family parameter should be a character of length 1.")
      } else if(length(family$family) != 1){
        stop("The element named 'family' in the family parameter should be a character of length 1.")
      }
      if(!is.function(family$link.inv)) stop("The link.inv element in 'family' should be a function.")
      if(!is.function(family$costfun)) stop("The costfun element in 'family' should be a function.")
      if(!is.function(family$gradfun)) stop("The gradfun element in 'family' should be a function.")
      if(length(family) > 4){
        warning("family contains elements that are being ignored.")
        family <- family[c("family", "link.inv", "costfun", "gradfun")]
      }
    }
  } else if(is.null(checkpoint)){
      stop("The family must be specified if there is no checkpoint model.")
  } else{
    family <- checkpoint@model$family
    #tryCatch(family <- checkpoint@model$family, error=stop("Error retrieving family from checkpoint model"))
  }

  ## Check Y has correct dimensions
  if(family$family != "multiclass"){
    if(dim(Y)[1] != 1) stop("Y should be a single row matrix for this type of regression")
  } else{
    if(dim(Y)[1] == 1){
      stop("Y should be a mutli-row matrix for this type of regression")
    } else if(dim(Y)[1] == 2) warning("Softmax regression with only two classes. Suggest running a binary regression instead.")
  }
  
  ## Check learning_rate and hidden_layer_dims inputs
  if(!is.numeric(learning_rate)) stop("Learning rate should be a number.")
  if(length(learning_rate) != 1) stop("Learning rate should be a scalar.")
  if(!is.null(hidden_layer_dims)){
    if(!is.numeric(hidden_layer_dims)) stop("hidden_layer_dims should be numeric.")
    if(!all(hidden_layer_dims==as.integer(hidden_layer_dims))) stop("hidden_layer_dims should be integers.")
  } else if(is.null(checkpoint)){
    stop("hidden_layer_dims must be specified if there is no checkpoint model.")
  }
  
  ## Number of non-input layers
  L <- length(hidden_layer_dims)+1
  
  ## Check hidden_layer_activations
  if(!is.null(hidden_layer_activations)){
    if(!length(hidden_layer_activations) %in% c(1, L-1)) stop("hidden_layer_activations should be length 1 or the same length as hidden_layer_dims.")
    if(!all(hidden_layer_activations %in% c("relu", "tanh"))) stop("Only relu and tanh supported for hidden layer activations.")
  } else if(is.null(checkpoint)){
    stop("hidden_layer_activations must be specified if there is no checkpoint model.")
  }
  
  ## Check optimiser
  if(!length(optimiser)==1) stop("optimiser should have length 1.")
  if(!optimiser %in% c("GradientDescent", "Momentum", "RMSProp", "Adam")) stop("optimiser should be one of GradientDescent, Momentum, RMSProp, Adam.")
  
  ## Check keep_prob
  if(!is.null(keep_prob)){
    if(!is.numeric(keep_prob)) stop("keep_prob should be numeric")
    if(!length(keep_prob) %in% c(1, L-1)) stop("keep_prob should be length 1 or the same length as hidden_layer_dims.")
    if(!all(keep_prob>0)) stop("keep_prob contains zero or negative numbers.")
    if(!all(keep_prob<=1)){
      warning("keep_prob contains numbers greater than 1. These will be treated as 1.")
      keep_prob <- pmin(keep_prob, 1)
    }
    if(all(keep_prob==1)){
      print("keep_prob contains only 1s so no drop-out will be performed on hidden layers.")
      keep_prob <- NULL
    }
  }
  
  ## Check input_keep_prob
  if(!is.null(input_keep_prob)){
    if(!is.numeric(input_keep_prob)) stop("input_keep_prob should be numeric")
    if(length(input_keep_prob) != 1) stop("input_keep_prob should be length 1.")
    if(input_keep_prob <= 0) stop("input_keep_prob is zero or negative.")
    if(input_keep_prob >= 1){
      if(input_keep_prob > 1){
        warning("input_keep_prob is greater than 1 and will be ignored")
      }else print("input_keep_prob is 1 so no drop-out will be performed on the input layer.")
      input_keep_prob <- NULL
    }
  }
  
  ## Check alpha and lambda
  if(!is.numeric(alpha)) stop("alpha should be numeric")
  if(length(alpha) != 1) stop("alpha should be length 1.")
  if(alpha < 0) stop("alpha is negative.")
  if(!is.numeric(lambda)) stop("lambda should be numeric")
  if(length(lambda) != 1) stop("lambda should be length 1.")
  if(lambda < 0) stop("lambda is negative.")
  
  ## Check mini_batch_size
  if(!is.null(mini_batch_size)){
    if(!is.numeric(mini_batch_size)) stop("mini_batch_size should be numeric")
    if(length(mini_batch_size) != 1) stop("mini_batch_size should be length 1.")
    if(mini_batch_size != as.integer(mini_batch_size)) stop("mini_batch_size should be an integer.")
    if(mini_batch_size <= 0) stop("mini_batch_size should be an postive.")
    if(!mini_batch_size < m){
      warning("mini_batch_size is greater than or equal to the number of training examples and will be ignored.")
      mini_batch_size <- NULL
    }
  }
  
  ## Check dev_set
  if(!is.null(dev_set)){
    if(!is.numeric(dev_set)) stop("dev_set should be numeric")
    if(!all(dev_set==as.integer(dev_set))) stop("Non-integer values detected in dev_set.")
    if(min(dev_set)<1) stop("Zero or negative numbers detected in dev_set.")
    if(max(dev_set)>m) stop("dev_set contains numbers greater than the number of training examples.")
    if(length(unique(dev_set)) != length(dev_set)){
      warning("dev_set contains repeated column numbers which will be ignored.")
      dev_set <- unique(dev_set)
    }
  }
  
  ## Setup beta1, beta2 and epsilon if required
  if(missing(beta1)) if(optimiser %in% c("Momentum", "Adam")) beta1 <- 0.9 else beta1 <- NULL
  if(missing(beta2)) if(optimiser %in% c("RMSProp", "Adam")) beta2 <- 0.999 else beta2 <- NULL
  if(missing(epsilon)) if(optimiser %in% c("RMSProp", "Adam")) epsilon <- 1E-8 else epsilon <- NULL
  
  ## Check beta1
  if(!is.null(beta1)){
    if(!optimiser %in% c("Momentum", "Adam")){
      warning("beta1 is only used when optimiser is Momentum or Adam and will be ignored")
      beta1 <- NULL
    } else{
      if(!is.numeric(beta1)) stop("beta1 should be numeric")
      if(length(beta1) != 1) stop("beta1 should be length 1.")
      if(beta1 <= 0) stop("beta1 is negative or zero. Rather than set beta1 to zero change the optimiser.")
      if(beta1 >= 1) stop("beta1 is greater than or equal to 1.")
    }
  }
  
  ## Check beta2
  if(!is.null(beta2)){
    if(!optimiser %in% c("RMSProp", "Adam")){
      warning("beta2 is only used when optimiser is RMSProp or Adam and will be ignored")
      beta2 <- NULL
    } else{
      if(!is.numeric(beta2)) stop("beta2 should be numeric")
      if(length(beta2) != 1) stop("beta2 should be length 1.")
      if(beta2 <= 0) stop("beta2 is negative or zero. Rather than set beta2 to zero change the optimiser.")
      if(beta2 >= 1) stop("beta2 is greater than or equal to 1.")
    }
  }
  
  ## Check epsilon
  if(!is.null(epsilon)){
    if(!optimiser %in% c("RMSProp", "Adam")){
      warning("epsilon is only used when optimiser is RMSProp or Adam and will be ignored")
      epsilon <- NULL
    } else{
      if(!is.numeric(epsilon)) stop("epsilon should be numeric")
      if(length(epsilon) != 1) stop("epsilon should be length 1.")
      if(epsilon <= 0) stop("epsilon is negative or zero.")
      if(epsilon >= 1E-5) warning("epsilon may be too large.")
    }
  }
  
  ## Check other parameters
  if(!is.null(print_every_n)){
    if(!is.numeric(print_every_n)) stop("print_every_n should be numeric")
    if(length(print_every_n) != 1) stop("print_every_n should be length 1.")
    print_every_n <- as.integer(print_every_n)
  }
  if(!is.logical(plot)) stop("plot should be logical")
  if(length(plot) != 1) stop("plot should be length 1.")
  
  ## Helper parameters
  ## layer dimensions
  layer_dims <- c(dim(X)[1], hidden_layer_dims, dim(Y)[1])
  
  ## keep_prob for dropout
  if(is.null(keep_prob)) keep_prob <- 1
  if(length(keep_prob) < L-1) keep_prob <- rep(keep_prob, L-1)
  keep_prob <- c(keep_prob, 1)
  
  ## input_keep_prob for dropout
  if(is.null(input_keep_prob)) input_keep_prob <- 1
  
  ## activation functions for hidden layers
  if(length(hidden_layer_activations) < L-1) hidden_layer_activations <- rep(hidden_layer_activations, L-1)
  
  ## initialise betas and epsilon if NULL
  if(is.null(beta1)) beta1 <- 0
  if(is.null(beta2)) beta2 <- 0
  if(is.null(epsilon)) epsilon <- 1E-8
  
  ## Deal with batch processing by just setting mini-batch size to m (number of training examples)
  if(is.null(mini_batch_size)) mini_batch_size <- m
  
  ## Set activation function for output layer
  activations <- c(as.list(hidden_layer_activations), family$link.inv)

  
  # Initialise model ready for learning -------------------------------------
  
  ## Initialise all model parameters
  if(is.null(checkpoint)){
    model <- vector(mode="list")
    model$Params <- initialiseParams(layer_dims, activations, seed)
    if(optimiser %in% c("Momentum", "Adam")) model$v <- initialise_v(model$Params)
    if(optimiser %in% c("RMSProp", "Adam")) model$s <- initialise_s(model$Params)
    model$family <- family
  } else{
    model <- checkpoint@model
    if(!is.null(hidden_layer_dims)) warning("hidden_layer_dims is specified but will be ignored since starting learning from checkpoint model")
    if(!is.null(hidden_layer_activations)) warning("hidden_layer_activations is specified but will be ignored since starting learning from checkpoint model")
    if(!is.null(family)) {
      if(!identical(family$family, model$family$family)) stop("Mis-match between family specified and family from checkpoint model.")
    }
  }
  
  ## Extract development set
  if(!is.null(dev_set)){
    X_dev <- X[, dev_set, drop=FALSE]
    Y_dev <- Y[, dev_set, drop=FALSE]
    X <- X[, -dev_set, drop=FALSE]
    Y <- Y[, -dev_set, drop=FALSE]
    m <- m-length(dev_set)
  }
  
  ## Set a seed
  set.seed(seed)
  
  ## Split into mini-batches
  mini_batches <- lapply(seq(0, ceiling(m / mini_batch_size)-1), function(i){
    permutate <- sample(seq(1, m))
    X <- X[, permutate, drop=FALSE]
    Y <- Y[, permutate, drop=FALSE]
    return(list(X=X[, seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m)), drop=FALSE],
                Y=Y[, seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m)), drop=FALSE]))
  })
  
  num_batches <- length(mini_batches)

  ## Initialise t and Costs
  t <- 0
  if(!is.null(dev_set)){
    Costs <- data.frame(trainCost=numeric(), 
                        devLoss=numeric())
  } else{
    Costs <- data.frame(trainCost=numeric())
  }
  
  ## First run of forward prop
  model$Cache <- forwardPropagation(mini_batches[[t%%num_batches + 1]][["X"]], model$Params, input_keep_prob, keep_prob)
  
  ## Compute cost
  J <- computeCost(model, mini_batches[[t%%num_batches + 1]][["Y"]], alpha, lambda)
  if(!is.null(dev_set)){
    dev_model <- model
    dev_model$Cache <- forwardPropagation(X_dev, dev_model$Params, 1, rep(1, L))
    dev_loss <- computeCost(dev_model, Y_dev, 0, 0)
    Costs <- rbind(Costs, data.frame(trainCost=J, devLoss=dev_loss))
  } else{
    Costs <- rbind(Costs, data.frame(trainCost=J))
  }
  
  ## Print initial costs
  if(!is.null(dev_set)){
    print(paste0("Cost after 0 epochs: train-", prettyNum(J), ", dev-", prettyNum(dev_loss)))
  } else{
    print(paste0("Cost after 0 epochs: ", prettyNum(J)))
  }
  
  ## Loop through epochs
  for(e in seq(1, num_epochs)){
    ## For each epoch loop through each mini-batch
    for(t in seq(t+1, t+num_batches)){
      
      ## Compute Gradients with backprop
      model$Grads <- backwardPropagation(mini_batches[[(t-1)%%num_batches + 1]][["Y"]], model, alpha, lambda, keep_prob)
      
      ## Update model parameters
      model <- updateParameters(model, learning_rate, optimiser, t, beta1, beta2, epsilon)
      
      ## Run forward prop
      model$Cache <- forwardPropagation(mini_batches[[t%%num_batches + 1]][["X"]], model$Params, input_keep_prob, keep_prob)
    }
    
    ## Compute cost
    J <- computeCost(model, mini_batches[[t%%num_batches + 1]][["Y"]], alpha, lambda)
    if(!is.null(dev_set)){
      dev_model <- model
      dev_model$Cache <- forwardPropagation(X_dev, dev_model$Params, 1, rep(1, L))
      dev_loss <- computeCost(dev_model, Y_dev, 0, 0)
      Costs <- rbind(Costs, data.frame(trainCost=J, devLoss=dev_loss))
    } else{
      Costs <- rbind(Costs, data.frame(trainCost=J))
    }
    
    ## Print output
    if(!is.null(print_every_n)){
      if(!e%%print_every_n | e==num_epochs){
        
        if(!is.null(dev_set)){
          print(paste0("Cost after ", prettyNum(e, big.mark=","), " epochs: train-", prettyNum(J), ", dev-", prettyNum(dev_loss)))
          if(e==num_epochs){
            print(paste0("Best dev score achieved was ", prettyNum(min(Costs$devLoss)), " after ", which.min(Costs$devLoss)-1, " epochs."))
          }
        } else{
          print(paste0("Cost after ", prettyNum(e, big.mark=","), " epochs: ", prettyNum(J)))
        }
        if(plot){
          if(!is.null(dev_set)){
            plot(seq(0, e), Costs$trainCost, type="l", ylab="Cost Function", xlab="Epochs", ylim=range(c(Costs$trainCost, Costs$devLoss)))
            lines(Costs$devLoss, col="red")
            legend("topright", legend=c("Cost (train)", "Cost (dev)"), col=c("black", "red"), lty=1)
          } else{
            plot(seq(0, e), Costs$trainCost, type="l", ylab="Cost Function", xlab="Epochs")
          }
        }
      }
    }
    
    ## redfine mini-batches for next epoch
    mini_batches <- lapply(seq(0, ceiling(m / mini_batch_size)-1), function(i){
      permutate <- sample(seq(1, m))
      X <- X[, permutate, drop=FALSE]
      Y <- Y[, permutate, drop=FALSE]
      return(list(X=X[, seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m)), drop=FALSE],
                  Y=Y[, seq(mini_batch_size*i+1, min(mini_batch_size*(i+1), m)), drop=FALSE]))
    })
  }
  
  # Drop Cache
  model <- model[setdiff(names(model), "Cache")]
  
  # Put output together
  if(keep){
    final <- EdNetModel(model=model, Costs=Costs, data=list(X=X, Y=Y))
  } else{
    final <- EdNetModel(model=model, Costs=Costs, data=list())
  }
  
  # Return final model
  return(final)
}