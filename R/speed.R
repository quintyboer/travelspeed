#' Calculate travel speed between geolocation points
#'
#' The `speed` function calculates the travel speed between two consecutive
#' geolocation measurements using either the Haversine great-circle distance or
#' the Vincenty ellipsoid distance from the `geosphere` package functions
#' \code{\link[geosphere]{distHaversine}} and \code{\link[geosphere]{distVincentyEllipsoid}}.
#'
#' @param geolocations A data frame or matrix with columns: longitude, latitude, and timestamp.
#'                     Longitude and latitude must be numeric. Timestamp must be in POSIXct format.
#'                     At least two rows are required to calculate travel speed.
#' @param method A string specifying the distance calculation method. Either "haversine" or "vincenty".
#'               Default is "haversine".
#' @param unit A string specifying the speed output unit. Either "m/s" (meters per second)
#'                    or "km/h" (kilometers per hour). Default is "m/s".
#' @returns A numeric vector of speeds in the specified output unit between consecutive points.
#' @import geosphere
#' @export
#' @examples
#' # generate example data
#' set.seed(42)
#' data <- data.frame(
#'   longitude = -122.4194 + cumsum(runif(10, -0.0001, 0.0002)),
#'   latitude = 37.7749 + cumsum(runif(10, -0.0001, 0.0002)),
#'   timestamp = as.POSIXct("2024-12-06 14:00:00",
#'                          tz = "UTC") +
#'     cumsum(sample(1:10, 10, replace = TRUE))
#' )
#' speed(geolocations = data, method = "haversine", unit = "km/h")
speed <- function(geolocations, method = "haversine", unit = "m/s") {

  # check for correct input
  if (!all(c("longitude", "latitude", "timestamp") %in% colnames(geolocations))) {
    stop("'geolocations' data must contain 'longitude', 'latitude', and 'timestamp' columns.")
  }
  if (!is.numeric(geolocations$longitude) || !is.numeric(geolocations$latitude)) {
    stop("'longitude' and 'latitude' columns must be numeric.")
  }
  if (length(geolocations$longitude) < 2 || length(geolocations$latitude) < 2) {
    stop("at least two geolocation points are required to calculate speed.")
  }
  if (!inherits(geolocations$timestamp, "POSIXct")) {
    stop("'timestamp' column must be in POSIXct format.")
  }
  if (any(diff(geolocations$timestamp) <= 0)) {
    stop("'timestamp' values must be increasing, so no duplicate or backwards timestamps.")
  }
  if (!method %in% c("haversine", "vincenty")) {
    stop("'method' must be either 'haversine' or 'vincenty'.")
  }
  if (!unit %in% c("m/s", "km/h")) {
    stop("'unit' must be either 'm/s' or 'km/h'.")
  }

  # separate columns
  lon <- geolocations$longitude
  lat <- geolocations$latitude
  time <- geolocations$timestamp

  # calculate distance using haversine or vincenty distance
  distance <- if (method == "haversine") {
    geosphere::distHaversine(cbind(lon[-length(lon)], lat[-length(lat)]),
                             cbind(lon[-1], lat[-1]))
  } else {
    geosphere::distVincentyEllipsoid(cbind(lon[-length(lon)], lat[-length(lat)]),
                                     cbind(lon[-1], lat[-1]))
  }

  # calculate time differences in seconds
  time_diff <- as.numeric(difftime(time[-1], time[-length(time)], units = "secs"))

  # calculate m/s
  speed <- distance / time_diff

  # calculate km/h
  if (unit == "km/h") {
    speed <- speed * 3.6
  }

  return(speed)
}
