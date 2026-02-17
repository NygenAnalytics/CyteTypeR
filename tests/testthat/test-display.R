test_that(".display_cluster_status_cli returns invisibly with empty list", {
  expect_silent(CyteTypeR:::.display_cluster_status_cli(list(), is_final = TRUE, spinner_frame = 0))
})

test_that(".display_cluster_status_cli runs without error with cluster status", {
  status <- list("1" = "completed", "2" = "processing")
  expect_error(capture.output(CyteTypeR:::.display_cluster_status_cli(status, is_final = TRUE, spinner_frame = 0)), NA)
})

test_that(".display_cluster_status_cli produces output containing cluster info when final", {
  status <- list("1" = "completed", "2" = "completed")
  out <- capture.output(CyteTypeR:::.display_cluster_status_cli(status, is_final = TRUE, spinner_frame = 0))
  expect_length(out, 1L)
  expect_true(grepl("2/2", out[1]) || grepl("completed", out[1]))
})
