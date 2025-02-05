test_that("jrvFinance functions are accessible", {
  # Test bond price calculation
  expect_no_error(
    bond.price(settle = "2024-02-05",
               mature = "2029-02-05",
               coupon = 0.05,
               yield = 0.06,
               freq = 2)
  )


  # Test internal rate of return
  expect_no_error(
    irr(cf = c(-100, 50, 60))
  )
})
