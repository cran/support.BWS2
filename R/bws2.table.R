bws2.table <-
function(
  x,
  score = c("bw", "b", "w"), 
  output = c("level", "attribute"),
  ...)
{

  score <- match.arg(score)
  if (score == "bw") {
    SCORE <- x[, attributes(x)$bw.names]
  } else if (score == "b") {
    SCORE <- x[, attributes(x)$b.names]
  } else {
    SCORE <- x[, attributes(x)$w.names]
  }  

  output <- match.arg(output)

  freq.levels <- attributes(x)$freq.levels
  num.levels <- length(freq.levels)

  for (i in 1:num.levels) {
    SCORE[, i] <- factor(SCORE[, i], 
                         levels = if (score == "bw") {
                                    c(-freq.levels[i]:freq.levels[i])
                                  } else {c(0:freq.levels[i])})
  }
  SCOREtable <- lapply(SCORE, table)

  if (isTRUE(output == "attribute")) {
    attributeSCOREtable <- SCOREtable
    scoredot <- paste(score, ".", sep = "")
    names(attributeSCOREtable) <- sub(scoredot, "", names(attributeSCOREtable))

    attr.lev <- attributes(x)$attribute.levels

    tmp <- vector("list", length(attr.lev))
    names(tmp) <- names(attr.lev)

    for (i in names(tmp)){
      tmp[[i]] <- as.table(rowSums(sapply(attributeSCOREtable[attr.lev[[i]]],
                                          "+")))
    }

    names(tmp) <- paste(scoredot, names(tmp), sep = "")
    SCOREtable <- tmp
  }
  
  return(SCOREtable)
}
