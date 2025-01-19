# check overall output for both haversine and vincenty method
test_that("output makes sense when haversine method is specified", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1),
    latitude = c(52.0, 52.1),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 12:01:00"))
  )
  result <- speed(geolocations, method = "haversine", unit = "m/s")
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)
})

test_that("output makes sense when vincenty method is specified", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1),
    latitude = c(52.0, 52.1),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 12:01:00"))
  )
  result <- speed(geolocations, method = "vincenty", unit = "m/s")
  expect_type(result, "double")
  expect_length(result, 1)
  expect_gt(result, 0)
})

# check that km/h unit equals m/s * 3.6
test_that("conversion from m/s to km/h works well", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1),
    latitude = c(52.0, 52.1),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 12:01:00"))
  )
  result_ms <- speed(geolocations, method = "haversine", unit = "m/s")
  result_kmh <- speed(geolocations, method = "haversine", unit = "km/h")
  expect_equal(result_kmh, result_ms * 3.6, tolerance = 1e-6)
})

# check errors: incorrect column
test_that("correct error is given when timestamp column is missing", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1, 5.2),
    latitude = c(52.0, 52.1, 52.2)
  )
  expect_error(
    speed(geolocations, method = "haversine"),
    "'geolocations' data must contain 'longitude', 'latitude', and 'timestamp' columns."
  )
})

# check errors: non-numeric longitude
test_that("correct error is given when longitude is not numeric", {
  geolocations <- data.frame(
    longitude = c("5.0", "5.1"),
    latitude = c(52.0, 52.1),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 12:01:00"))
  )
  expect_error(speed(geolocations, method = "haversine"),
               "'longitude' and 'latitude' columns must be numeric.")
})

# check errors: less than two geolocations
test_that("correct error is given when when there are less than two geolocations", {
  geolocations <- data.frame(
    longitude = c(5.0),
    latitude = c(52.0),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00"))
  )
  expect_error(speed(geolocations, method = "haversine"),
               "at least two geolocation points are required to calculate speed.")
})

# check errors: timestamp incorrect format
test_that("correct error is given when timestamps are not in POSIXct format", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1),
    latitude = c(52.0, 52.1),
    timestamp = c("2023-01-01 12:00:00", "2023-01-01 12:01:00")  # Not POSIXct
  )
  expect_error(speed(geolocations, method = "haversine"),
               "'timestamp' column must be in POSIXct format.")
})

# check errors: duplicate timestamps
test_that("correct error is given for duplicate timestamps", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1),
    latitude = c(52.0, 52.1),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 12:00:00"))  # No time difference
  )
  expect_error(speed(geolocations, method = "haversine"),
               "'timestamp' values must be increasing, so no duplicate or backwards timestamps.")
})

# check errors: misspelled method
test_that("correct error is given for misspelled method", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1),
    latitude = c(52.0, 52.1),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 12:01:00"))
  )
  expect_error(
    speed(geolocations, method = "vincentry"),
    "'method' must be either 'haversine' or 'vincenty'."
  )
})

# check errors: wrong unit
test_that("correct error is given for wrong output unit", {
  geolocations <- data.frame(
    longitude = c(5.0, 5.1),
    latitude = c(52.0, 52.1),
    timestamp = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 12:01:00"))
  )
  expect_error(
    speed(geolocations, unit = "km/s"),
    "'unit' must be either 'm/s' or 'km/h'."
  )
})
