sum.bws2.count <-
function(
  x,
  output = c("level", "attribute"),
  ...)
{
  output <- match.arg(output)

  B <- colSums(x[, attributes(x)$b.names])
  W <- colSums(x[, attributes(x)$w.names])
  names.B <- sub("b.", "", names(B))
  names.W <- sub("w.", "", names(W))
  if (!isTRUE(all.equal(names.B, names.W))) {
    stop("Names of B scores are different from those of W scores")
  }
  BW <- B - W
  stdBW <- BW / (nrow(x) * attributes(x)$freq.lev)
  rownames <- names.B

  if (isTRUE(output == "attribute")) {
    attributeB  <- B
    attributeW  <- W
    attributeBW <- BW

    names(attributeB)  <- sub("b.",  "", names(attributeB))
    names(attributeW)  <- sub("w.",  "", names(attributeB))
    names(attributeBW) <- sub("bw.", "", names(attributeB))

    attr.lev <- attributes(x)$attribute.levels

    tmpBW <- tmpW <- tmpB <- rep(0, times = length(attr.lev))
    names(tmpBW) <- names(tmpW) <- names(tmpB) <- names(attr.lev)

    for (i in names(tmpB)){
      tmpB[i]  <- sum(attributeB[attr.lev[[i]]]) 
      tmpW[i]  <- sum(attributeW[attr.lev[[i]]])
      tmpBW[i] <- sum(attributeBW[attr.lev[[i]]])
    }

    B  <- tmpB
    W  <- tmpW
    BW <- tmpBW
    stdBW <- BW / (attributes(x)$nquestions * attributes(x)$nrespondents)
    rownames <- names(tmpB)
  }

  rtn <- data.frame(B = B, 
                    W = W,
                    BW = BW,
                    stdBW = stdBW)

  row.names(rtn) <- rownames

  return(rtn)
}
