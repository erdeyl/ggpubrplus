test_that("stat_regline_equation preserves legacy defaults and supports custom digits", {
  dat <- data.frame(x = mtcars$wt, y = mtcars$mpg)
  fit <- stats::lm(y ~ x, dat)

  legacy <- ggpubrplus:::.stat_lm(y ~ x, dat, output.type = "text")
  custom <- ggpubrplus:::.stat_lm(
    y ~ x, dat,
    output.type = "text",
    rr.digits = 4, adj.rr.digits = 4,
    aic.digits = 4, bic.digits = 4, eq.digits = 4
  )

  expect_equal(legacy$rr, signif(summary(fit)$r.squared, 2))
  expect_equal(legacy$adj.rr, signif(summary(fit)$adj.r.squared, 2))
  expect_equal(legacy$AIC, signif(stats::AIC(fit), 2))
  expect_equal(legacy$BIC, signif(stats::BIC(fit), 2))

  expect_equal(custom$rr, signif(summary(fit)$r.squared, 4))
  expect_equal(custom$adj.rr, signif(summary(fit)$adj.r.squared, 4))
  expect_equal(custom$AIC, signif(stats::AIC(fit), 4))
  expect_equal(custom$BIC, signif(stats::BIC(fit), 4))
})

test_that("stat_regline_equation layer accepts new precision parameters", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    stat_regline_equation(
      rr.digits = 4, adj.rr.digits = 4,
      aic.digits = 4, bic.digits = 4, eq.digits = 4
    )

  expect_no_error(
    ggplot2::ggplot_build(p)
  )
})
