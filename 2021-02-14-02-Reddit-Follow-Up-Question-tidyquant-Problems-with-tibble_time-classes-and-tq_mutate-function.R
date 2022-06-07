# Reddit Question
# https://www.reddit.com/r/rprogramming/comments/lioehi/unable_to_reproduce_code_from_a_book_called/
# 2021-02-12

pkgsToLoad <- c("quantmod", "tidyquant", "purrr", "dplyr", "tidyr", "tibbletime", "timetk", "lubridate")

nullVar <- lapply(pkgsToLoad, function(pkgName) {
  if (!require(pkgName, character.only = TRUE)) {
    install.packages(pkgName, character.only = TRUE)
  } else {
    cat(pkgName, "was already installed. Package was loaded.", "\n")
  }
})

# Get prices from Yahoo

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

prices <-
  getSymbols(symbols, src = 'yahoo',
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

head(prices)

# To Monthly Returns using tibbletime

(
  asset_returns_tbltime <-
    prices %>%
    tk_tbl(preserve_index = TRUE,
           rename_index = "date") %>%
    # this is the tibbletime function
    as_tbl_time(index = date) %>%
    as_period(period = "month",
              side = "end")
)

(
  asset_returns_tbltime1 <- prices %>%
    tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
    pivot_longer(cols = -date, names_to = "symbol", values_to = "close") %>%
    as_tbl_time(index = date) %>%
    relocate(symbol, .before = date)
)

(
  asset_returns_tbltime2 <- asset_returns_tbltime1 %>%
    split(.$symbol) %>%
    map(
      function(x) {
        x %>%
          as_period(period = "month", side = "end") %>%
          tq_mutate(
            select = everything(),
            mutate_fun = periodReturn,
            period = "monthly",
            type = "log")
      }
    ) %>%
    dplyr::bind_rows()
)

(
  asset_returns_tbltime3 <- asset_returns_tbltime2 %>%
    pivot_wider(
      id_cols = date,
      names_from = symbol,
      values_from = monthly.returns)
)
