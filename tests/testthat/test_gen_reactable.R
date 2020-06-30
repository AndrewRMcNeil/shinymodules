test_that("gen_reactable throws if .data isn't a tibble", {

  expect_error(
    gen_reactable(
      .data = data.frame(a = 1)
    )
  )

})

test_that("gen_reactable returns an object of class reactable", {

  react_table <- gen_reactable(
    .data = tibble(a = 1)
  )

  expect_class(react_table, "reactable")

})
