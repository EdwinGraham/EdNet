updateParameters <- function(model, learning_rate, optimiser="GradientDescent", t=NULL, beta1=NULL, beta2=NULL, epsilon=NULL){
  L <- length(model$Params)
  
  # Initialise bias correction lists
  if(optimiser %in% c("Momentum", "Adam")){
    v_BiasCorrected <- vector(mode="list", length=length(model$v))
    names(v_BiasCorrected) <- names(model$v)
  }
  
  if(optimiser %in% c("RMSProp", "Adam")){
    s_BiasCorrected <- vector(mode="list", length=length(model$s))
    names(s_BiasCorrected) <- names(model$s)
  }
  
  for(i in seq(1, L)){
    # Update velocity
    if(optimiser %in% c("Momentum", "Adam")){
      model$v[[paste0("l", i)]]$vdW <- beta1 * model$v[[paste0("l", i)]]$vdW + (1 - beta1) * model$Grads[[paste0("l", i)]]$dW
      model$v[[paste0("l", i)]]$vdb <- beta1 * model$v[[paste0("l", i)]]$vdb + (1 - beta1) * model$Grads[[paste0("l", i)]]$db
      
      # Perform bias correction
      v_BiasCorrected[[paste0("l", i)]]$vdW <- model$v[[paste0("l", i)]]$vdW/(1 - beta1^t)
      v_BiasCorrected[[paste0("l", i)]]$vdb <- model$v[[paste0("l", i)]]$vdb/(1 - beta1^t)
    }
    
    # Update moving average of squared gradients
    if(optimiser %in% c("RMSProp", "Adam")){
      model$s[[paste0("l", i)]]$sdW <- beta2 * model$s[[paste0("l", i)]]$sdW + (1 - beta2) * model$Grads[[paste0("l", i)]]$dW^2
      model$s[[paste0("l", i)]]$sdb <- beta2 * model$s[[paste0("l", i)]]$sdb + (1 - beta2) * model$Grads[[paste0("l", i)]]$db^2
      
      # Perform bias correction
      s_BiasCorrected[[paste0("l", i)]]$sdW <- model$s[[paste0("l", i)]]$sdW/(1 - beta2^t)
      s_BiasCorrected[[paste0("l", i)]]$sdb <- model$s[[paste0("l", i)]]$sdb/(1 - beta2^t)
    }
    
    # Update Parameters
    if(optimiser == c("GradientDescent")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * model$Grads[[paste0("l", i)]]$dW
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * model$Grads[[paste0("l", i)]]$db
    } else if(optimiser == c("Momentum")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdW
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdb
    } else if(optimiser == c("RMSProp")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * model$Grads[[paste0("l", i)]]$dW / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdW) + epsilon)
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * model$Grads[[paste0("l", i)]]$db / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdb) + epsilon)
    } else if(optimiser == c("Adam")){
      model$Params[[paste0("l", i)]]$W <- model$Params[[paste0("l", i)]]$W - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdW / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdW) + epsilon)
      model$Params[[paste0("l", i)]]$b <- model$Params[[paste0("l", i)]]$b - learning_rate * v_BiasCorrected[[paste0("l", i)]]$vdb / (sqrt(s_BiasCorrected[[paste0("l", i)]]$sdb) + epsilon)
    } else stop("Only GradientDescent, Momentum, RMSProp and Adam supported.")
  }
  return(model)
}