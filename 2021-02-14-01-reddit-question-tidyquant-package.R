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

    prices <- tibble::tibble(
      symbol   = character(0),
      date     = Date(0),
      open     = numeric(0),
      high     = numeric(0),
      low      = numeric(0),
      close    = numeric(0),
      volume   = numeric(0),
      adjusted = numeric(0)
    )

    for (stockSymbol in symbols) {
      rslts <- tq_get(
        stockSymbol,
        get = "stock.prices",
        from = "2012-12-31",
        to = "2017-12-31"
      )
      prices <- rbind(
        prices, rslts
      )
    }

    asset_returns_tbltime <- prices %>%
        as_tbl_time(index = date)        %>%        # this is the tibbletime function
        tk_tbl(
          preserve_index = TRUE,
          rename_index = "date")         %>%
        as_period(
          period = "month",
          side = "end")

    asset_returns_tbltime <- asset_returns_tbltime %>%
        gather(asset, returns, -date)    %>%
        group_by(asset)                  %>%
        tq_transmute(
         mutate_fun = periodReturn,
         type = "log")                   %>%
        # tq_mutate(
        #   mutate_fun = periodReturn,
        #   type = "log")                %>%
        spread(asset, monthly.returns)   %>%
        select(date, symbols)            %>%
        slice(-1)

    # prices <- getSymbols(
    #   symbols, src = 'yahoo',
    #   from = "2012-12-31",
    #   to = "2017-12-31",
    #   auto.assign = TRUE,
    #   warnings = FALSE)     %>%
    #   map(~Ad(get(.))) %>%
    #   reduce(merge) %>%
    #   `colnames<-`(symbols)

    # To Monthly Returns using tibbletime
    (
      asset_returns_tbltime <- prices %>%
        tk_tbl(
          preserve_index = TRUE,
          rename_index = "date")       %>%
        as_tbl_time(index = date)      %>%        # this is the tibbletime function
        as_period(
          period = "month",
          side = "end")  %>%
        gather(asset, returns, -date)  %>%
        group_by(asset)                %>%
      # tq_transmute(
      #  mutate_fun = periodReturn,
      #  type = "log")                  %>%
        tq_mutate(
          mutate_fun = periodReturn,
          type = "log")                %>%
      spread(asset, monthly.returns)   %>%
      select(date, symbols)            %>%
      slice(-1)
    )
