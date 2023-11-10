test_that("formatting large numbers works", {
  expect_equal(format_big(10000), "10,000")
  expect_equal(format_big(0), "0")
  expect_equal(format_big(1), "1")
  expect_equal(format_big(-1), "-1")
  expect_equal(format_big(10^7), "10,000,000")
  expect_equal(format_big(123456789), "123,456,789")
})
