setwd("C:/Users/Tati/GitHub/reproducible")
devtools::load_all()
library("LandR")

# test_that("test Cache works when using a subset of the cached object", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  opt <- options("reproducible.cachePath" = c(tmpdir, tmpCache))

  A <- c(1, 2, 3, 4, 5, 6)
  B <- 2*A + c(0.2, 0.4, -0.3, 0.4, 0.9, -0.5)
  C <- c("G1", "G1", "G2", "G2", "G3", "G3")

  a <- data.frame(A = A, B = B, C = C)
  b <- a[a$C %in% c("G1", "G2"), ]

  modCache <- Cache(FUN = LandR::statsModel, form = "A ~ B + (1|C)", .specialData = a,
                         uniqueEcoregionGroups = C, argsToPreserve = "uniqueEcoregionGroups",
                         cacheRepo = tmpCache, omitArgs = ".specialData")
  # Expect to have an attribute named X with Values Y

  C <- c("G1", "G1", "G2", "G2")

  modCacheSub <- Cache(FUN = LandR::statsModel, formula = "A ~ B + (1|C)", .specialData = b, uniqueEcoregionGroups = C,
                       expectSubset = "uniqueEcoregionGroups", cacheRepo = tmpCache,
                       omitArgs = c(".specialData", "expectSubset"))

  # FROM HERE ON: implement in the showSimilar:
  # 0. Only if cacheId is NOT supplied
  # 1. test if the arguments that differ can be compared
  # 2. Compare them setdiff(subset, fromOriginalObject$full)
  # 3. if length(setdiff)==0, they are inside, get the cacheId
  # 4. pass the cacheId to the call with a message

  on.exit(options(opt), add = TRUE)
# })


# here cache will check if the supplied function/arguments are a subset of
# a cached object
browser()
# Here I need to:
# 0. Only if cacheId is NOT supplied
# 1. test if the arguments that differ can be compared
# 2. Compare them setdiff(subset, fromOriginalObject$full)
# 3. if length(setdiff)==0, they are inside, get the cacheId
# 4. pass the cacheId to the call with a message\
