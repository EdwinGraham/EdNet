# Class EdNetModel
setClass(
  "EdNetModel",
  representation(
    model="list",
    Costs="data.frame",
    data="list"
  ),
  prototype = prototype(model=list(),
                        Costs=data.frame(),
                        data=list()),
  validity=function(object){

    return(TRUE)
  }
) -> EdNetModel
##########################