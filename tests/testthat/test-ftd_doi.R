skip_on_cran()

context("ftd_doi")

test_that("ftd_doi: karger", {
  # dois_karger=rcrossref::cr_members(127, works=TRUE, limit=5)$data$doi
  # save(dois_karger, file="tests/testthat/dois/dois_karger.rda", version=2)
  load("dois/dois_karger.rda")
  a <- ftd_doi(dois_karger)
  expect_is(a, 'list')
  expect_length(a, 5)
  expect_equal(a[[1]]$doi, dois_karger[1])
  expect_is(a[[1]]$links, 'list')
  expect_match(a[[1]]$links[[1]]$url, 'https://www.karger.com')
})

test_that("ftd_doi: pensoft", {
  dois_pensoft=c('10.3897/zookeys.594.8768', '10.3897/mycokeys.54.34571',
    '10.3897/phytokeys.99.26489', '10.3897/subtbiol.13.6719')
  a <- ftd_doi(dois_pensoft)
  expect_is(a, 'list')
  expect_length(a, 4)
  expect_equal(a[[1]]$doi, dois_pensoft[1])
  expect_is(a[[1]]$links, 'list')
  expect_match(a[[1]]$links[[1]]$url, 'pensoft.net')
})

test_that("ftd_doi: frontiers", {
  # dois_frontiers=rcrossref::cr_members(1965, works=TRUE, limit=5)$data$doi
  # save(dois_frontiers, file="tests/testthat/dois/dois_frontiers.rda", version=2)
  load("dois/dois_frontiers.rda")
  a <- ftd_doi(dois_frontiers)
  expect_is(a, 'list')
  expect_length(a, 5)
  expect_equal(a[[1]]$doi, dois_frontiers[1])
  expect_is(a[[1]]$links, 'list')
  expect_match(a[[1]]$links[[1]]$url, 'frontiersin.org')
})

test_that("ftd_doi: PNAS", {
  # dois_pnas=rcrossref::cr_members(341, works=TRUE, limit=5)$data$doi
  # save(dois_pnas, file="tests/testthat/dois/dois_pnas.rda", version=2)
  load("dois/dois_pnas.rda")
  a <- ftd_doi(dois_pnas)
  expect_is(a, 'list')
  expect_length(a, 5)
  expect_equal(a[[1]]$doi, dois_pnas[1])
  expect_is(a[[1]]$links, 'list')
  expect_match(a[[1]]$links[[1]]$url, 'pnas.org')
})

test_that("ftd_doi fails correctly", {
  skip_on_cran()
  
  expect_error(ftd_doi())
})
