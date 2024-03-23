test_that("dur() works", {
  expect_no_error(dur(st))
  expect_type(dur(st),"character")
})
