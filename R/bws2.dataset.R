bws2.dataset <-
function(
  data,  
  id,
  response,
  choice.sets,
  attribute.levels,
  attribute.variables = c("reverse", "constant"),
  effect = NULL, 
  delete.best = FALSE,
  type = c("paired", "marginal"),
  ...)
{

  attribute.variables <- match.arg(attribute.variables)  
  type <- match.arg(type)

  if (type == "paired") {
    bws2.dataset.paired(
      data                = data,
      id                  = id,
      response            = response,
      choice.sets         = choice.sets,
      attribute.levels    = attribute.levels,
      attribute.variables = attribute.variables,
      effect              = effect)
  } else if (type == "marginal") {
    bws2.dataset.marginal(
      data                = data,
      id                  = id,
      response            = response,
      choice.sets         = choice.sets,
      attribute.levels    = attribute.levels,
      attribute.variables = attribute.variables,
      effect              = effect,
      delete.best         = delete.best)
  }

}
