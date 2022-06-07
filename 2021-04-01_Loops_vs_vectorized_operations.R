# https://www.reddit.com/r/Rlanguage/comments/mi6gli/is_it_bad_practice_to_use_lapply_to_build/
#
# I'm trying to embrace more of the functional aspects of R's design
# and I'm finding that I use lapply() to essentially move through a
# vector and build a list of 1 row data frames that I then rbind()
# together. To me this feels very "for-loop-y" and I'm wondering if
# it's not a very good practice. Or maybe put differently, am I
# working against the design of the language instead of with it?

# Here's a simple example of taking two vectors of dates, one
# which only contains weekdays, and then aligning them by finding
# the most recent weekday date for a given date:

library(assertthat)

targetDayOfWk <- "Friday"

GetLastWeekDay <- function(ptestDate, ptargetDayOfWk = "Sunday") {

  assert_that(
    is.date(ptestDate),
    msg = "ptestDate must be of type 'date'")

  assert_that(
    length(ptargetDayOfWk) == 1,
    msg = "ptargetDayOfWk must be a single value (i.e. not a vector or list)")

  assert_that(
    ptargetDayOfWk %in% c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
    msg = "ptargetDayOfWk of must be a string")

  rslts <- vector("numeric", length(ptestDate))
  ctr <- 0

  for (dateIndex in ptestDate) {
    ctr <- ctr + 1

    while (weekdays(as.Date(dateIndex, origin = "1970-01-01")) != ptargetDayOfWk) {
      dateIndex <- dateIndex - 1
    }

    rslts[ctr] <- as.Date(dateIndex, origin = "1970-01-01")
  }

  return(rslts)
}

dates1 <- seq(Sys.Date() - 14, Sys.Date(), by = "day")
# dates2 <- dates1[!weekdays(dates1) %in% c("Saturday", "Sunday")]
#
# dates_aligned <- do.call(
#   "rbind",
#   lapply(dates1, function(x) {
#     xdate2 <- dates2[dates2 <= x]
#     xdate2 <- xdate2[rev(order(xdate2))][1]
#     data.frame(date1 = x, date2 = xdate2)
#   })
# )

dfDates <- data.frame(
  datevals = dates1,
  dayOfWk = weekdays(dates1),
  targetDayFlag = weekdays(dates1) == targetDayOfWk,
  lastTargetDate = as.Date(GetLastWeekDay(dates1, targetDayOfWk), origin = "1970-01-01")
)

