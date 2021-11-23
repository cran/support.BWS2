bws2.dataset.paired <-
function(
  data, 
  id,
  response, 
  choice.sets, 
  attribute.levels, 
  reverse,
  base.level)
{


# set variables
  choice.sets.original <- choice.sets
  choice.sets <- as.matrix(choice.sets)

### added ver 0.2-0 below -----------------------------------------------------
## effect
  effect <- base.level
## attribuet.variables
  if (isTRUE(reverse)) {
    attribute.variables <- "reverse"
  } else {
    attribute.variables <- "constant"
  }
### added ver 0.2-0 above -----------------------------------------------------

## respondent dataset
  if (!is.null(data)) {
    resp.data <- data
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

## attribute variables
  attr.var <- names(attr.lev) 

## level variables
  lev.var <- unlist(attr.lev)

## level variables without the reference level in each attribute
  if (!is.null(effect)){
    for (i in attr.var) {
      attr.lev[[i]] <- attr.lev[[i]][attr.lev[[i]] != effect[[i]]]
      attr.lev[[i]] <- c(attr.lev[[i]], effect[[i]])
    }
    lev.var.wo.ref <- unlist(attr.lev)[-cumsum(num.lev)]
  } else {
    lev.var.wo.ref <- unlist(attr.lev)
  }

## change level values in choice sets (serial number starting from 1)
  temp <- matrix(data = c(0, cumsum(num.lev)[-num.attr]),
                 nrow = num.ques, ncol = num.attr, byrow = TRUE)
  choice.set.serial <- choice.sets + temp



# creat a design matrix

  des.mat <- matrix(0L, nrow = num.attr * (num.attr - 1) * num.ques,
                        ncol = 8 + num.attr + length(lev.var.wo.ref))
  des.mat <- data.frame(des.mat)
                           
  colnames(des.mat) <- 
    c("Q",            # question number
      "PAIR",         # all possible pair number
      "BEST",         # best in a possible pair
      "WORST",        # worst in a possible pair
      "BEST.AT",      # attribute of level treated as best in a possible pair
      "WORST.AT",     # attribute of level treated as worst in a possible pair
      "BEST.LV",      # level treated as best in a possible pair
      "WORST.LV",     # level treated as worst in a possible pair
      attr.var,       # attribute variables
      lev.var.wo.ref) # level variables


## create "Q" variable: serial number starting from 1
  des.mat[, 1] <- rep(1:num.ques, each = num.attr*(num.attr - 1))

## create "PAIR" variable: serial number starting from 1
  des.mat[, 2] <- rep(1:(num.attr * (num.attr - 1)), time = num.ques)

## create "BEST" and "WORST" variables
  lastRow <- 0
  for (i in 1:num.ques) {
    ## create all combinations of levels shown in a BWS question
    temp <- expand.grid(WORST = choice.set.serial[i, ], 
                        BEST  = choice.set.serial[i, ])
    ## remove an impossible pair from the combination
    temp <- data.matrix(subset(temp, temp$BEST != temp$WORST))
    storage.mode(temp) <- "integer"
    ## assign values to elements of design.matrix
    des.mat[(1 + lastRow):(lastRow + nrow(temp)), 3] <- temp[,2]
    des.mat[(1 + lastRow):(lastRow + nrow(temp)), 4] <- temp[,1]
    lastRow <- lastRow + nrow(temp)
  }

## create "BEST.AT" and "WORST.AT" variables
  temp <- unlist(mapply(rep, attr.var, num.lev))
  des.mat[, 5] <- temp[des.mat[, 3]]
  des.mat[, 6] <- temp[des.mat[, 4]]

## create "BEST.LV" and "BEST.LV" variable
  des.mat[, 7] <- lev.var[des.mat[, "BEST"]]
  des.mat[, 8] <- lev.var[des.mat[, "WORST"]]

## create attribute variables
### attribute variables: 1 = treated as best, -1 = treated as worst
  temp <- model.matrix(~ factor(des.mat[, 5], levels = attr.var) - 1) -
          model.matrix(~ factor(des.mat[, 6], levels = attr.var) - 1)
### attribute variables: attribute-specific constants (= 1)
  if (reverse == FALSE) {
    temp <- abs(temp)
  }

  storage.mode(temp) <- "integer"
  des.mat[, 9:(8 + num.attr)] <- temp

## create level variables 
  if (!is.null(effect)) {
### effect coding
    best.lev  <- mapply(contr.sum, num.lev, SIMPLIFY = FALSE)
    for (i in attr.var) {
      rownames(best.lev[[i]]) <- attr.lev[[i]]
    }
    worst.lev <- sapply(best.lev, function(f) -f, simplify = FALSE)
    for (i in 1:nrow(des.mat)) {
      y <- attr.lev[[des.mat[i, "BEST.AT"]]][-num.lev[des.mat[i, "BEST.AT"]]]
      des.mat[i, y] <- 
        as.integer(best.lev[[des.mat[i, "BEST.AT"]]][des.mat[i, "BEST.LV"], ])  
      y <- attr.lev[[des.mat[i, "WORST.AT"]]][-num.lev[des.mat[i, "WORST.AT"]]]
      des.mat[i, y] <- 
        as.integer(worst.lev[[des.mat[i, "WORST.AT"]]][des.mat[i, "WORST.LV"], ])
    }
  } else {
  ### dummy coding
    temp <- model.matrix(~ factor(des.mat[, 7], levels = lev.var.wo.ref) - 1) -
            model.matrix(~ factor(des.mat[, 8], levels = lev.var.wo.ref) - 1)
    storage.mode(temp) <- "integer"
      des.mat[, (9 + num.attr):(8 + num.attr + length(lev.var.wo.ref))] <- temp
  }

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
    ID   = rep(resp.data$ID,
               each = num.attr * (num.attr - 1) * num.ques),
    Q    = rep(1:num.ques, each = num.attr * (num.attr - 1)),
    PAIR = rep(1:(num.attr * (num.attr - 1)), times =num.ques))
  exp.resp.data <- merge(temp, resp.data.long, by = c("ID", "Q"))
  exp.resp.data <- exp.resp.data[order(exp.resp.data$ID, 
                                 exp.resp.data$Q,
                                 exp.resp.data$PAIR), ]


# create dataset for discrete choice models

  dataset <- merge(exp.resp.data, des.mat, by = c("Q", "PAIR"))
  TRUEorFALSE.B <- attr.var[dataset$RES.B] == dataset$BEST.AT
  TRUEorFALSE.W <- attr.var[dataset$RES.W] == dataset$WORST.AT
  dataset$RES <- as.integer((TRUEorFALSE.B + TRUEorFALSE.W) == 2)
  dataset$STR <- dataset$ID * 100 + dataset$Q
  dataset <- dataset[order(dataset$STR, dataset$PAIR), ]

  row.names(dataset) <- NULL


# change order of variables

  covariate.names <- colnames(resp.data)
  covariate.names <- 
    covariate.names[!covariate.names %in% c("ID", response)]
  dataset <- dataset[, c("ID", "Q", "PAIR", "BEST", "WORST", "BEST.AT",
                         "WORST.AT", "BEST.LV", "WORST.LV", attr.var,
                         lev.var.wo.ref, "RES.B", "RES.W", "RES", "STR",
                         covariate.names)]


# change name of id variable

  colnames(dataset)[which(colnames(dataset) == "ID")] <- id


# calculate frequency of each level

  tmp1 <- tmp2 <- choice.sets
  for (i in 1:ncol(tmp1)) {
    tmp2[, i] <- attribute.levels[[i]][tmp1[, i]]
  }
  tbl <- table(tmp2)
  freq.levels <- as.vector(tbl)
  names(freq.levels) <- names(tbl)
  freq.levels <- freq.levels[unlist(attribute.levels)]


# set attributes

  attributes(dataset)$id                  <- id
  attributes(dataset)$response            <- response
  attributes(dataset)$choice.sets         <- choice.sets.original
  attributes(dataset)$attribute.levels    <- attribute.levels
  attributes(dataset)$reverse             <- reverse
  attributes(dataset)$base.level          <- base.level
  attributes(dataset)$attribute.variables <- attribute.variables
  attributes(dataset)$effect              <- effect
  attributes(dataset)$type                <- c("paired")
  attributes(dataset)$design.matrix       <- design.matrix
  attributes(dataset)$lev.var.wo.ref      <- lev.var.wo.ref
  attributes(dataset)$freq.levels         <- freq.levels
  attributes(dataset)$respondent.characteristics <- respondent.characteristics



# return dataset

  return(data = dataset)

}
