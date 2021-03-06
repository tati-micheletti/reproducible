test_that("git-related functions work", {
  skip_on_os("windows")
  skip_on_os("mac")
  if (requireNamespace("git2r")) {
    gitConfigOK <- tryCatch(.checkGitConfig(), error = function(e) FALSE)

    ## don't bother running checks if they'll all fail due to .gitconfig SSH issue
    if (gitConfigOK) {
      tmpDir <- tempdir2("test_git")
      on.exit({
        unlink(tmpDir, recursive = TRUE)
      }, add = TRUE) # nolint

      ## dir doesn't exist; repo doesn't exist -- need to checkout
      checkoutVersion("PredictiveEcology/reproducible", localRepoPath = tmpDir, progress = FALSE)
      testRepo <- git2r::repository(tmpDir)
      expect_true(dir.exists(tmpDir))
      expect_false(git2r::is_empty(testRepo))
      expect_false(git2r::is_bare(testRepo))
      expect_false(git2r::is_detached(testRepo))
      expect_true(git2r::is_local(git2r::branches(testRepo)$master))
      expect_identical(git2r::remote_url(testRepo),
                       "https://github.com/PredictiveEcology/reproducible.git")
      rm(testRepo)
      unlink(tmpDir, force = TRUE, recursive = TRUE)

      ## dir exists; repo doesn't exist -- need to checkout
      expect_true(dir.create(tmpDir))
      specificCommit <- "de5853669d801223306eaaa34d9299b0a846dae1"
      checkoutVersion(paste0("PredictiveEcology/reproducible@", specificCommit),
                      localRepoPath = tmpDir, progress = FALSE)
      testRepo <- git2r::repository(tmpDir)
      expect_false(git2r::is_empty(testRepo))
      expect_false(git2r::is_bare(testRepo))
      expect_true(git2r::is_detached(testRepo))
      expect_true(git2r::is_local(git2r::branches(testRepo)$master))
      expect_identical(git2r::remote_url(testRepo),
                       "https://github.com/PredictiveEcology/reproducible.git")

      ## Backwards compatibility with git2r
      ## S4 <= 0.21.0
      ## S3 >= 0.22.0
      if (isS4(testRepo)) {
        expect_identical(git2r::head(testRepo)@sha, specificCommit)
      } else {
        expect_identical(git2r::sha(git2r::repository_head(testRepo)), specificCommit)
      }

      rm(testRepo)
      unlink(tmpDir, force = TRUE, recursive = TRUE)

      ## dir exists; repo exists -- fetch/checkout to update                # nolint
      checkoutVersion("PredictiveEcology/reproducible", localRepoPath = tmpDir, progress = FALSE)
      testRepo <- git2r::repository(tmpDir)
      expect_false(git2r::is_empty(testRepo))
      expect_false(git2r::is_bare(testRepo))
      expect_false(git2r::is_detached(testRepo))
      expect_true(git2r::is_local(git2r::branches(testRepo)$master))
      expect_identical(git2r::remote_url(testRepo),
                       "https://github.com/PredictiveEcology/reproducible.git")

      ## Backwards compatibility with git2r
      ## S4 <= 0.21.0
      ## S3 >= 0.22.0
      if (isS4(testRepo)) {
        expect_identical(git2r::head(testRepo)@name, "master")
      } else {
        expect_identical(git2r::repository_head(testRepo)$name, "master")
      }

      rm(testRepo)
      unlink(tmpDir, force = TRUE, recursive = TRUE)

      ## dir exists; another repo exsits -- error
      checkoutVersion("PredictiveEcology/SpaDES.addins", localRepoPath = tmpDir)
      expect_error(git2r::checkoutVersion("PredictiveEcology/reproducible", localRepoPath = tmpDir))
      unlink(tmpDir, force = TRUE, recursive = TRUE)
    }
  }
})
