bws2.questionnaire <-
function (
  choice.sets,
  attribute.levels = NULL, 
  position = c("left", "center", "right")) 
{
  sets <- choice.sets

  if (!is.null(attribute.levels)) {
    sets <- data.frame(sets)
    for (i in 1:ncol(sets)) {
      sets[, i] <- factor(x = sets[, i], 
                          levels = sort(unique.default(sets[, i])),
                          labels = attribute.levels[[i]])
    }
  }

  numQuestions  <- nrow(sets)
  numAttributes <- ncol(sets)

  sets <- apply(sets, 2, as.character)
  sets <- t(sets)

  position <- match.arg(position)

  if (position == "left") {
    pos <- 1
    coltitle <- c("Attribute", "Best", "Worst")
  } else if (position == "center") {
    pos <- 2
    coltitle <- c("Best", "Attribute", "Worst")
  } else {
    pos <- 3
    coltitle <- c("Best", "Worst", "Attribute")
  }

  for (i in 1:numQuestions) {
    cat("\n")
    cat("Q", i, "\n", sep = "")
    dsp <- matrix(c("[ ]"), nrow = numAttributes, ncol = 3)
    dsp[, pos] <- sets[, i]
    colnames(dsp) <- coltitle
    rownames(dsp) <- rep(c(""), numAttributes)
    print(noquote(dsp))
  }

  cat("\n")
}
