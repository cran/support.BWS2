bws2.dataset <-
function(
  data,  
  id,
  response,
  choice.sets,
  attribute.levels,
  base.attribute = NULL,
  base.level = NULL,
  reverse = TRUE,
  model = "paired",
  attribute.variables = NULL,
  effect = NULL,
  delete.best = FALSE,
  type = c("paired", "marginal", "sequential"),
  ...)
{

# check arguments

## deprecated arguments

### type
  if (length(type) == 1) {
    model <- match.arg(type)
    warning("argument type is deprecated. Please use model instead. Argument model was set as '", model, "'")
  }

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
    warning("argument delete.best is deprecated. Please use model instead. Argument model was set as 'sequential'")
    model <- "sequential"
  }

## invalid combinations of arguments

### type (model) and base.attribute
  if(model == "paired" && !is.null(base.attribute)) {
    stop("effect-coded attribute variables are unavailable for paired model")
  }

### reverse and base.attribute
 if (reverse == FALSE && !is.null(base.attribute)) {
   stop("effect-coded attribute variables need that reverse = TRUE")
 }


# call internal function

## paired model
  if (model == "paired") {
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
      type                = model,
      base.attribute      = base.attribute,
      base.level          = base.level,
      reverse             = reverse)
  }

}
