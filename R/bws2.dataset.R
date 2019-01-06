bws2.dataset <-
function(
  data,  
  id,
  response,
  choice.sets,
  attribute.levels,
  base.attribute = NULL,
  base.level = NULL,
  type = c("paired", "marginal", "sequential"),
  reverse = TRUE,
  attribute.variables = NULL,
  effect = NULL,
  delete.best = FALSE,
  ...)
{

# check arguments

## deprecated arguments

### attribute.variables
  if (!is.null(attribute.variables)) {
    if (attribute.variables == "reverse") {
      warning("argument attribute.variables is deprecated. Please use reverse instead. Argument reverse was set as TRUE.")
      reverse <- TRUE
    } else if (attribute.variables == "constant") {
      warning("argument attribute.variables is deprecated. Please use reverse instead. Argument reverse was set as TRUE.")
      reverse <- FALSE
    }
  }

### effect
  if (!is.null(effect)) {
    warning("argument effect is deprecated. Please use base.level instead. Values in argument effect were assigned to argument base.level")
    base.level <- effect
  }

### delete.best
  if (isTRUE(delete.best)) {
    warning("argument delete.best is deprecated. Please use type instead. Argument type was set as 'sequential'")
    type <- "sequential"
  }

## invalid combinations of arguments

### type and base.attribute
  if(type == "paired" && !is.null(base.attribute)) {
    stop("effect-coded attribute variables are unavailable for paired model")
  }

### reverse and base.attribute
 if (reverse == FALSE && !is.null(base.attribute)) {
   stop("effect-coded attribute variables need that reverse = TRUE")
 }


# call internal function

## paired model
  if (type == "paired") {
    bws2.dataset.paired(
      data                = data,
      id                  = id,
      response            = response,
      choice.sets         = choice.sets,
      attribute.levels    = attribute.levels,
      reverse             = reverse,
      base.level          = base.level)

## marginal or marginal sequential model
  } else {
    bws2.dataset.marginal(
      data                = data,
      id                  = id,
      response            = response,
      choice.sets         = choice.sets,
      attribute.levels    = attribute.levels,
      type                = type,
      base.attribute      = base.attribute,
      base.level          = base.level,
      reverse             = reverse)
  }

}
