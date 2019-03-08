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

  # Case with only one argument
  modCache <- Cache(FUN = LandR::statsModel, form = "A ~ B + (1|C)", .specialData = a,
                         uniqueEcoregionGroups = C, argsToPreserve = "uniqueEcoregionGroups",
                         cacheRepo = tmpCache, omitArgs = ".specialData")
  testthat::expect_true(!is.null(attributes(modCache)$argsToPreserve))
  testthat::expect_true(names(attributes(modCache)$argsToPreserve) == "uniqueEcoregionGroups")
  testthat::expect_identical(attributes(modCache)$argsToPreserve[["uniqueEcoregionGroups"]], C)

  D <- c("G1", "G1", "G2", "G2")

  testthat::expect_message(modCacheSub <- Cache(FUN = LandR::statsModel, formula = "A ~ B + (1|C)", .specialData = b,
                                                uniqueEcoregionGroups = D,
                                                expectSubset = "uniqueEcoregionGroups", cacheRepo = tmpCache,
                                                omitArgs = c(".specialData", "expectSubset")),
                           regexp = "All expected subsets are found in the cached object")

  testthat::expect_true(!is.null(attributes(modCacheSub)$argsToPreserve))
  testthat::expect_true(names(attributes(modCacheSub)$argsToPreserve)=="uniqueEcoregionGroups")
  testthat::expect_identical(attr(modCacheSub, "tags"), attr(modCacheSub, "tags"))

  # Case with two arguments, but one is being ignored as it doesn't belong to the function
  modCache2 <- Cache(FUN = LandR::statsModel, form = "A ~ B + (1|C)", .specialData = a,
                    uniqueEcoregionGroups = C, secondArg = c("A1", "A2", "A3"),
                    argsToPreserve = c("uniqueEcoregionGroups", "secondArg"),
                    cacheRepo = tmpCache, omitArgs = ".specialData")
  testthat::expect_true(!is.null(attributes(modCache)$argsToPreserve))
  testthat::expect_identical(names(attributes(modCache2)$argsToPreserve), c("uniqueEcoregionGroups", "secondArg"))
  testthat::expect_identical(attributes(modCache2)$argsToPreserve[["uniqueEcoregionGroups"]], C)
  testthat::expect_identical(attributes(modCache2)$argsToPreserve[["secondArg"]], c("A1", "A2", "A3"))

  D <- c("G1", "G1", "G2", "G2")

  testthat::expect_message(modCacheSub2 <- Cache(FUN = LandR::statsModel, formula = "A ~ B + (1|C)", .specialData = b,
                                                uniqueEcoregionGroups = D, secondArg = c("A1", "A3"),
                                                expectSubset = "uniqueEcoregionGroups", cacheRepo = tmpCache,
                                                omitArgs = c(".specialData", "expectSubset")),
                           regexp = "All expected subsets are found in the cached object")

  testthat::expect_true(!is.null(attributes(modCacheSub2)$argsToPreserve))
  # As secondArg is being ignored (as it doesn't exist in the function), Cache returns the first object
  testthat::expect_identical(attr(modCache, "tags"), attr(modCacheSub2, "tags"))

  # Case with two arguments, but both belong to the function

  funny <- function(I, am, awesome){
    aintI <- I*100
    return(aintI)
  }

  funnyCache2args <- Cache(FUN = funny, I = 1, am = c("great", "positive"),
                           awesome = c("absolutely", 10),
                     argsToPreserve = c("am", "awesome"),
                     cacheRepo = tmpCache, omitArgs = "I")
  testthat::expect_true(!is.null(attributes(funnyCache2args)$argsToPreserve))
  testthat::expect_identical(names(attributes(funnyCache2args)$argsToPreserve), c("am", "awesome"))
  testthat::expect_identical(attributes(funnyCache2args)$argsToPreserve[["am"]], c("great", "positive"))
  testthat::expect_identical(attributes(funnyCache2args)$argsToPreserve[["awesome"]], c("absolutely", 10)) # works with numeric or
  testthat::expect_identical(attributes(funnyCache2args)$argsToPreserve[["awesome"]], c("absolutely", "10")) # character

  # If all arguments are different we have a new cached call
  testthat::expect_message(funnyCacheSub2args <- Cache(FUN = funny, I = 3, am = c("amazing", "cute"),
                                                       awesome = c("surely", 3),
                                                       expectSubset = c("am", "awesome"),
                                                       cacheRepo = tmpCache, omitArgs = "I"),
                           regexp = "The arguments passed are not a subset of the most similar cached object")
# TEST: test that the cache differs from funnyCache2

  # If only one of two arguments is the same we have a new cached call ### [ FIX ] Should show which arguments differ!
  testthat::expect_message(funnyCacheSub2args2 <- Cache(FUN = funny, I = 3, am = c("great", "positive"),
                                                       awesome = c("surely", 3),
                                                       expectSubset = c("am", "awesome"),
                                                       cacheRepo = tmpCache, omitArgs = "I"),
                           regexp = "The arguments passed are not a subset of the most similar cached object")
  # TEST: test that the cache differs from funnyCache2

  # Make a test for when it could get a similar cache without attributes

  testthat::expect_true(!is.null(attributes(modCacheSub2)$argsToPreserve))

  on.exit(options(opt), add = TRUE)
# })

