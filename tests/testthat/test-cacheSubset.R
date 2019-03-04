setwd("C:/Users/tmichele/GitHub/reproducible")
devtools::load_all()

test_that("test Cache works when using a subset of the cached object", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  opt <- options("reproducible.cachePath" = c(tmpdir, tmpCache))

  A <- c(1, 2, 3, 4, 5, 6)
  B <- 2*A + c(0.2, 0.4, -0.3, 0.4, 0.9, -0.5)
  C <- c("G1", "G1", "G2", "G2", "G3", "G3")

  lmWithAttribute <- function(formula, data, attribute){
    mod <- lm(formula = formula,
              data = data)
    attributes(mod) <- list(group = attribute)
    return(mod)
  }
  a <- data.frame(A = A, B = B, C = C)
  modCache <- Cache(FUN = lmWithAttribute, formula = A ~ B, data = a,
                    attribute = C, cacheRepo = tmpCache, omitArgs = "data")

  # FROM HERE ON: implement in the showSimilar:
  # 0. Only if cacheId is NOT supplied
  # 1. test if the arguments that differ can be compared
  # 2. Compare them setdiff(subset, fromOriginalObject$full)
  # 3. if length(setdiff)==0, they are inside, get the cacheId
  # 4. pass the cacheId to the call with a message

  on.exit(options(opt), add = TRUE)
})
