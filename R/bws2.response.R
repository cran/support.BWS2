bws2.response <- function(
 design,
 attribute.levels,
 base.level = NULL,
 b,
 n,
 detail = FALSE,
 seed = NULL)
{
# Set variables
  nR  <- n                                 # Number of respondents
  nA  <- length(attribute.levels)          # Number of attributes
  nL  <- length(unlist(attribute.levels))  # Number of all levels
  nPP <- ncol(design) * (ncol(design) - 1) # Number of possible pairs
  AL  <- attribute.levels

# Create design matrix for a respondent according to Model 1 or 2
  D <- bws2.dataset(data = NULL,
                    choice.sets = design,
                    attribute.levels = attribute.levels, 
                    base.level = base.level,
                    reverse = TRUE,
                    model = "paired")

# Calculate utilities for levels
  id <- rep(1:nR, each = nrow(D))
  X  <- data.frame(id, D)
  if (is.null(base.level)) {
    col.f <- 10 + nA
  } else {
    col.f <- 10
  }
  Xb <- sweep(x = X[, col.f:ncol(X)], MARGIN = 2, STATS = b, FUN = "*") 
  V  <- rowSums(Xb)
  if (!is.null(seed)) set.seed(seed)
  e  <- -log(-log(runif(n = length(V))))
  U  <- V + e

# Search best and worst levels according to U
  Umat <- matrix(data = U, ncol = nPP, byrow = TRUE) 
  columns.max <- max.col(Umat)
  BWelement <- cbind(R = 1:nrow(Umat), C = columns.max)
  RESmat <- matrix(data = 0L, nrow = nrow(Umat), ncol = ncol(Umat))
  RESmat[BWelement] <- 1L
  RES <- as.vector(t(RESmat))

# Construct and return detailed dataset
  dataset <- data.frame(X, RES = RES)
  if (detail == TRUE) {
    dataset$STR <- 100 * dataset$id + dataset$Q
    return(dataset)
  }

# Construct simple dataset
  simple.dataset <- dataset[dataset$RES == 1,
                            c("id", "Q", "BEST.LV", "WORST.LV")]
  colnames(simple.dataset)[c(3, 4)] <- c("B", "W")

  design_all <- kronecker(X = matrix(rep(1, times = nR), nrow = nR, ncol = 1),
                          Y = design)
  character.design_all <- design_all
  for (i in 1:nA) {
    character.design_all[, i] <- AL[[i]][design_all[, i]]
  }

  colB <- t(t(simple.dataset[, "B"]))
  tmpB <- sweep(x = character.design_all, MARGIN = 1, STATS = colB, FUN = "==")
  tmpB <- which(tmpB == TRUE, arr.ind = TRUE)
  B <- tmpB[order(tmpB[, 1]), ]
  B <- B[, 2]
  colW <- t(t(simple.dataset[, "W"]))
  tmpW <- sweep(x = character.design_all, MARGIN = 1, STATS = colW, FUN = "==")
  tmpW <- which(tmpW == TRUE, arr.ind = TRUE)
  W <- tmpW[order(tmpW[, 1]), ]
  W <- W[, 2]
  simple.dataset <- cbind(simple.dataset[, c(1, 2)], B = B, W = W)

# Return simple dataset
  rtn <- reshape(simple.dataset, v.names = c("B", "W"), idvar = "id",
                 timevar = "Q", sep = "", direction = "wide")
  return(data.frame(rtn))
}


