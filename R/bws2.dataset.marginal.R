bws2.dataset.marginal <-
function(
  data, 
  id,
  response,
  choice.sets,
  attribute.levels,
  type,
  base.attribute,
  base.level,
  reverse)
{


# set variables

### added v 0.2-0 below ------------------------------------------------------
## delete.best
  if (type == "sequential") {
    delete.best <- TRUE
  } else {
    delete.best <- FALSE
  }
## effect ver
  effect <- base.level
## attribute.variables
  if (isTRUE(reverse)) {
    attribute.variables <- "reverse"
  } else {
    attribute.variables <- "constant"
  }
### added v 0.2-0 above ------------------------------------------------------

## respondent dataset
  if (!is.null(data)) {
    resp.data <- data  ## modified ver 0.2-0
    colnames(resp.data)[which(colnames(resp.data) == id)] <- "ID"
  }

## attributes and their levels
  attr.lev <- attribute.levels

## number of attributes
  num.attr <- length(attr.lev)

## number of levels in each attribute
  num.lev <- sapply(attr.lev, length) 

## number of questions (scenarios)
  num.ques <- nrow(choice.sets) 

## attribute.variables
  attr.var <- names(attr.lev) 

## level variables
  lev.var <- unlist(attr.lev)

## change level values in choice sets (serial number starting from 1)
  temp <- matrix(data = c(0, cumsum(num.lev)[-num.attr]),
                 nrow = num.ques, ncol = num.attr, byrow = TRUE)
  choice.set.serial <- choice.sets + temp

## level variables without the reference level in each attribute
  original.attr.lev <- attr.lev
  if (!is.null(effect)){
    for (i in attr.var) {
      attr.lev[[i]] <- attr.lev[[i]][attr.lev[[i]] != effect[[i]]]
      attr.lev[[i]] <- c(attr.lev[[i]], effect[[i]])
    }
    lev.var.wo.ref <- unlist(attr.lev)[-cumsum(num.lev)]
  } else {
    lev.var.wo.ref <- unlist(attr.lev)
  }



# creat a design matrix

  des.mat <- matrix(0L, nrow = 2 * num.attr * num.ques,
                        ncol = 7 + num.attr + length(lev.var.wo.ref))
  des.mat <- data.frame(des.mat)
                           
  colnames(des.mat) <- 
    c("Q",              # question number
      "ALT",            # attribute number in each question
      "BW",             # best and worst indicator (1 = best, -1 = worst)
      "ATT.cha", "ATT", # attribute variables (AT.cha: charactor, AT: integer)
      "LEV.cha", "LEV", # level variables (LV.cha: charactors, LV: integer)
      attr.var,         # attribute variables
      lev.var.wo.ref)   # level variables

## create "Q" variable: serial number starting from 1
  des.mat[, 1] <- rep(1:num.ques, each = 2 * num.attr)

## create "ALT" variable: serial number starting from 1
  des.mat[, 2] <- rep(1:num.attr, times = 2 * num.ques)

## create "BW" variable
  des.mat[, 3] <- rep(c(rep(1, times = num.attr), rep(-1, times = num.attr)),
                      times = num.ques) 

## create ATT.cha and ATT variables
  des.mat[, 4] <- rep(attr.var, times = 2 * num.ques)
  des.mat[, 5] <- rep(1:num.attr, times = 2 * num.ques)

## create LEV.cha and LEV variables
  choice.sets.cha <- choice.sets
  for (i in 1:num.attr){
    choice.sets.cha[, i] <- original.attr.lev[[i]][choice.sets[, i]] 
    # Using attr.lev[[i]] is not appropriate because bese.level may be changed
  }
  des.mat[, 6] <- as.vector(t(cbind(choice.sets.cha, choice.sets.cha)))
  des.mat[, 7] <- as.vector(t(cbind(choice.sets, choice.sets)))

## create attribute variables

### added v 0.2-0 below ------------------------------------------------------
  ATTR <- factor(des.mat[, 4], levels = attr.var)
  temp <- model.matrix(~ ATTR - 1)
  colnames(temp) <- substring(text = colnames(temp), first = 5)
  ### effect coding
  if (!is.null(base.attribute)) {
    rows2ref <- temp[, base.attribute] == 1
    temp[rows2ref, ] <- -1
  }
### added v 0.2-0 above ------------------------------------------------------

  if (isTRUE(attribute.variables == "reverse")) {
    temp <- temp * des.mat[, "BW"] 
  }
  storage.mode(temp) <- "integer"
  des.mat[, 8:(7 + num.attr)] <- temp

## create level variables
  if (!is.null(effect)) {
### effect coding
    best.lev  <- mapply(contr.sum, num.lev, SIMPLIFY = FALSE)
    for (i in attr.var) {
      rownames(best.lev[[i]]) <- attr.lev[[i]]
    }

    for (i in 1:nrow(des.mat)) {
      y <- attr.lev[[des.mat[i, "ATT.cha"]]][-num.lev[des.mat[i, "ATT.cha"]]]
      des.mat[i, y] <- 
        as.integer(best.lev[[des.mat[i, "ATT.cha"]]][des.mat[i, "LEV.cha"], ] *
                   des.mat[i, "BW"]) 
      # des.mat[i, "BW"] is used to multiply level variables by -1
    }
  } else {
### dummy coding
    temp <- model.matrix(~ factor(des.mat[, 6], levels = lev.var.wo.ref) - 1)
    temp <- temp * des.mat[, "BW"]
    storage.mode(temp) <- "integer"
    des.mat[, (8 + num.attr):(7 + num.attr + length(lev.var.wo.ref))] <- temp
  }

## calculate frequency of each level
  temp <- table(subset(des.mat, des.mat$BW == 1, select = "LEV.cha"))
  freq.levels <- as.vector(temp)
  names(freq.levels) <- names(temp)
  freq.levels <- freq.levels[lev.var]

### added ver 0.2-0 below -----------------------------------------------------
  if (!is.null(base.attribute)) {
    delete.column.ref <- colnames(des.mat) != base.attribute
    des.mat <- des.mat[, delete.column.ref]
  }
### added ver 0.2-0 above -----------------------------------------------------

## store design matrix
  design.matrix <- des.mat



# return design matrix
 if (is.null(data)) {
    return(des.mat)
 }



# create respondent dataset

## extract the names of respondents' characteristic variables
respondent.characteristics <- 
  colnames(resp.data)[!(colnames(resp.data) %in% c("ID", response))] 

## reshape the dataset into long format
  resp.data.long <- reshape(resp.data, 
                            idvar = "ID", 
                            varying = response, 
                            sep = "", 
                            direction = "long")
  temp <- which(colnames(resp.data.long) == "time")
  storage.mode(resp.data.long$time) <- "integer"
  colnames(resp.data.long)[temp:(temp + 2)] <- c("Q", "RES.B", "RES.W")

## expand respondent dataset according to possible pairs in each BWS question
  temp <- data.frame(
    ID  = rep(resp.data$ID, each = 2 * num.attr * num.ques),
    Q   = rep(1:num.ques, each = 2 * num.attr),
    ALT = rep(1:num.attr, times = 2 * num.ques),
    BW  = rep(c(rep(1, times = num.attr), rep(-1, times = num.attr)),
                      times = num.ques))
  exp.resp.data <- merge(temp, resp.data.long, by = c("ID", "Q"))
  exp.resp.data <- exp.resp.data[order(exp.resp.data$ID, 
                                 exp.resp.data$Q,
                                 -1 * exp.resp.data$BW,
                                 exp.resp.data$ALT), ]


# create dataset for discrete choice models

  dataset <- merge(exp.resp.data, des.mat, by = c("Q", "BW", "ALT"))
  dataset$RES <- (dataset$RES.B == dataset$ALT) * (dataset$BW ==  1) + 
                 (dataset$RES.W == dataset$ALT) * (dataset$BW == -1)
  dataset$STR <- dataset$ID * 1000 + dataset$Q * 10 + (dataset$BW == 1) + 
                 (dataset$BW == -1) * 2
  dataset <- dataset[order(dataset$STR, dataset$ALT), ]

  if (delete.best == TRUE) {
    select <- !(dataset$BW == -1 & dataset$ALT == dataset$RES.B)
    dataset <- dataset[select,]
  } 

  row.names(dataset) <- NULL


# change order of variables

### added ver 0.2-0 below -----------------------------------------------------
  if (!is.null(base.attribute)) {
    attr.var <- attr.var[attr.var != base.attribute]
  }
### added ver 0.2-0 above -----------------------------------------------------

  covariate.names <- colnames(resp.data)
  covariate.names <- 
    covariate.names[!covariate.names %in% c("ID", response)]
  dataset <- dataset[, c("ID", "Q", "ALT", "BW",  
                         "ATT.cha" ,"ATT", "LEV.cha", "LEV", 
                         attr.var, lev.var.wo.ref,
                         "RES.B", "RES.W", "RES", "STR", covariate.names)]


# change name of id variable

  colnames(dataset)[which(colnames(dataset) == "ID")] <- id


# set attributes

  attributes(dataset)$id                  <- id
  attributes(dataset)$response            <- response
  attributes(dataset)$choice.sets         <- choice.sets
  attributes(dataset)$attribute.levels    <- attribute.levels
  attributes(dataset)$reverse             <- reverse
  attributes(dataset)$base.attribute      <- base.attribute
  attributes(dataset)$base.level          <- base.level
  attributes(dataset)$attribute.variables <- attribute.variables
  attributes(dataset)$effect              <- effect
  attributes(dataset)$delete.best         <- delete.best
  attributes(dataset)$type                <- type
  attributes(dataset)$design.matrix       <- design.matrix
  attributes(dataset)$lev.var.wo.ref      <- lev.var.wo.ref
  attributes(dataset)$freq.levels         <- freq.levels
  attributes(dataset)$respondent.characteristics <- respondent.characteristics

# set S3 class bws2dataset

  class(dataset) <- c("bws2dataset", "data.frame")


# return dataset

  return(data = dataset)
}
