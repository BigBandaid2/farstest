library(testthat)
library(magrittr)
library(farstest)

testthat::expect_that(farstest::make_filename(2013) %>%
                        farstest::fars_read() %>%
                        nrow(),
                      testthat::equals(30202))
