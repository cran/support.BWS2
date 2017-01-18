barplot.bws2.count <-
function(
  height,
  score = c("bw", "b", "w"), 
  output = c("level", "attribute"),
  mfrow = NULL,
  ...)
{
  score  <- match.arg(score)
  output <- match.arg(output)

  SCOREtable <- bws2.table(x = height, score = score, output = output)

  if (is.null(mfrow)) {
    if (isTRUE(output == "level")) {
      mfrow <- c(3, ceiling(length(attributes(height)$freq.levels)/3))
    } else {
      mfrow <- c(1, length(attributes(height)$attribute.levels))
    }
  }

  par(mfrow = mfrow)

  for(i in 1:length(SCOREtable)){
    barplot(height = SCOREtable[[i]],
            main = names(SCOREtable)[i],
            xlab = "Score",
            ylab = "Respondents",
            ylim = c(0, max(unlist(SCOREtable))),
            ...)
  }

  invisible(SCOREtable)
}
