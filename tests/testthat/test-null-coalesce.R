test_that("%||% returns right side when left is NULL", {
  expect_identical(CyteTypeR:::`%||%`(NULL, "default"), "default")
  expect_identical(CyteTypeR:::`%||%`(NULL, 1L), 1L)
})

test_that("%||% returns left side when not NULL", {
  expect_identical(CyteTypeR:::`%||%`("x", "default"), "x")
  expect_identical(CyteTypeR:::`%||%`(0, 1), 0)
})
