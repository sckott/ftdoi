context("members")

test_that("members sends email correctly", {
  skip_on_cran()
  
  a <- ftd_members()
  
  expect_named(a, c('data', 'journals'))
  expect_is(a, 'list')
  expect_is(a$data, 'data.frame')
  expect_is(a$journals, 'data.frame')
})

test_that("members fails correctly", {
  skip_on_cran()
  
  expect_error(ftd_members(4444), 'that member not supported yet')
})
