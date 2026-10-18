test_that("CleanUpArtifacts removes built files and reports it", {
  local_temp_workdir()
  writeLines("h5", "vars.h5")
  writeLines("duckdb", "obs.duckdb")
  prepped <- list(build_succeeded = TRUE, vars_h5_path = "vars.h5", obs_duckdb_path = "obs.duckdb")

  expect_true(CleanUpArtifacts(prepped))
  expect_false(file.exists("vars.h5"))
  expect_false(file.exists("obs.duckdb"))
  # Running it again is harmless.
  expect_true(CleanUpArtifacts(prepped))
})

test_that("CleanUpArtifacts does nothing when the files were not built", {
  local_temp_workdir()
  writeLines("h5", "vars.h5")

  result <- withVisible(CleanUpArtifacts(list(build_succeeded = FALSE, vars_h5_path = "vars.h5")))

  expect_false(result$value)
  expect_false(result$visible)
  expect_true(file.exists("vars.h5"))
})
