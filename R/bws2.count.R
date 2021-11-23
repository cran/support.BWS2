bws2.count <-
function (
  data,
  ...) 
{
  attr.lev      <- attributes(data)$attribute.levels
  variableNames <- unlist(attr.lev)
  freq.lev      <- attributes(data)$freq.levels
  id.variable   <- attributes(data)$id

if (attributes(data)$type == "paired")  {

  dataset <- subset(data, data$RES == 1)
  dataset.bw <- subset(dataset, 
                       select = c(id.variable, "Q", "BEST.LV", "WORST.LV"))
  dsgn.best  <- matrix(dataset.bw$BEST.LV,
                       nrow = length(dataset.bw$BEST.LV),
                       ncol = length(variableNames))
  dsgn.worst <- matrix(dataset.bw$WORST.LV,
                       nrow = length(dataset.bw$WORST.LV),
                       ncol = length(variableNames))
  dsgn.ch <- matrix(variableNames,
                    nrow = nrow(dsgn.best),
                    ncol = ncol(dsgn.best),
                    byrow = TRUE)

  dsgn.mat <- dsgn.best == dsgn.ch
  storage.mode(dsgn.mat) <- "integer"
  colnames(dsgn.mat) <- variableNames
  BW <- rep(1, nrow(dsgn.mat))
  B <- cbind(dataset.bw[, c(id.variable, "Q", "BEST.LV")], BW, dsgn.mat)

  dsgn.mat <- dsgn.worst == dsgn.ch
  storage.mode(dsgn.mat) <- "integer"
  colnames(dsgn.mat) <- variableNames
  BW <- rep(-1, nrow(dsgn.mat))
  W <- cbind(dataset.bw[, c(id.variable, "Q", "WORST.LV")], BW, dsgn.mat)

} else {

# delete lev.var.wo.ref variables from data
  dataset <- data[, !colnames(data) %in% attributes(data)$lev.var.wo.ref]

# add level variables to dataset
  level.variables.mat <- matrix(dataset$LEV.cha,
                                nrow = length(dataset$LEV.cha),
                                ncol = length(variableNames))
  level.variable.names.mat <- matrix(variableNames,
                                     nrow = nrow(level.variables.mat),
                                     ncol = ncol(level.variables.mat),
                                     byrow = TRUE)
  level.variables.mat <- level.variables.mat == level.variable.names.mat
  storage.mode(level.variables.mat) <- "integer"
  colnames(level.variables.mat) <- variableNames
  dataset <- cbind(dataset, level.variables.mat)
  B <- subset(dataset, 
              dataset$BW ==  1 & dataset$RES == 1,
              select = c(id.variable, "Q", "BW", variableNames))
  W <- subset(dataset,
              dataset$BW == -1 & dataset$RES == 1,
              select = c(id.variable, "Q", "BW", variableNames))
}

  disaggreB <- do.call(rbind,
                       by(B[, c(id.variable, variableNames)],
                          B[, id.variable],
                          colSums))
  disaggreB[, id.variable] <- as.numeric(row.names(disaggreB))
  disaggreW <- do.call(rbind,
                       by(W[, c(id.variable, variableNames)],
                          W[, id.variable],
                          colSums))
  disaggreW[, id.variable] <- as.numeric(row.names(disaggreW))
  disaggreB <- data.frame(disaggreB)
  disaggreW <- data.frame(disaggreW)
  if (!all.equal(row.names(disaggreB), row.names(disaggreW))) stop()

  diffBW <- disaggreB - disaggreW
  diffBW[, id.variable] <- disaggreB[, id.variable]
  IDvar <- disaggreB[, id.variable]
  std.diffBW <- sweep(x = diffBW, MARGIN = 2, STATS = c(1, freq.lev), FUN = "/")
  std.diffBW[, id.variable] <- disaggreB[, id.variable]

  b.names  <- paste("b",  names(disaggreB)[-1], sep = ".")
  w.names  <- paste("w",  names(disaggreW)[-1], sep = ".")
  bw.names <- paste("bw", names(diffBW)[-1],    sep = ".")
  sbw.names <- paste("sbw", names(std.diffBW)[-1],    sep = ".")
  names(disaggreB)[-1] <- b.names
  names(disaggreW)[-1] <- w.names
  names(diffBW)[-1]    <- bw.names
  names(std.diffBW)[-1] <- sbw.names
  rtn <- merge(x = disaggreB, y = disaggreW, by = id.variable)
  rtn <- merge(x = rtn, y = diffBW, by = id.variable)
  rtn <- merge(x = rtn, y = std.diffBW, by = id.variable)

  if (!isTRUE(all.equal(length(attributes(data)$respondent.characteristics), 0))) {
    resp.cha.vars <- attributes(data)$respondent.characteristics
    if (attributes(data)$type == "paired")  {
      dataset.tmp <- subset(data,
                            data$Q == 1 & data$PAIR == 1,
                            select = c(id.variable, resp.cha.vars))
    } else {
      dataset.tmp <- subset(data,
                            data$Q == 1 & data$ALT == 1 & data$BW == 1,
                            select = c(id.variable, resp.cha.vars))
    }
      rtn <- merge(x = rtn, y = dataset.tmp, by = id.variable)
  }


  attributes(rtn)$nquestions   <- nrow(attributes(data)$choice.sets)
  attributes(rtn)$nrespondents <- length(IDvar)
  attributes(rtn)$freq.levels  <- freq.lev
  attributes(rtn)$attribute.levels <- attr.lev
  attributes(rtn)$vnames       <- variableNames
  attributes(rtn)$b.names      <- b.names
  attributes(rtn)$w.names      <- w.names
  attributes(rtn)$bw.names     <- bw.names
  attributes(rtn)$sbw.names    <- sbw.names

  class(rtn) <- c("bws2.count", "data.frame")

  return(rtn)
}
