#' Read FARS data
#'
#' This function reads the raw data out from a .csv file and returns a data.frame.
#' The raw data file is provided by the \strong{US National Highway Traffic Safety Administration's}
#' \emph{Fatality Analysis Reporting System} (FARS)
#'
#' @details Additional information about FARS is available at the following reference links:
#' \itemize{
#'   \item{\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}}
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#'
#' @param filename this is the filename of the file that will be loaded
#'
#' @return returns a dataframe of the loaded data
#'
#' @examples
#' library(magrittr)
#' yr <- 2015
#' data <- yr %>%
#'   make_filename %>%
#'   fars_read
#' head(data)
#' @note To generate file name use: \code{\link{make_filename}}
#' @seealso \link{make_filename}
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Builds the name of the file based on the year
#'
#' This function converts a simple \code{year} into the name format of a FARS .csv file.
#' It does not check whether that file is available.
#'
#' @param year a string or integer representing a 4 digit year.
#'
#' @return returns a filename concatenating "accident_" with the year and file extensions.
#'
#' @examples
#' make_filename(2013)
#' @seealso \link{fars_read}
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file(
    "extdata",
    sprintf("accident_%d.csv.bz2", year),
    package = "farstest",
    mustWork = T
  )
}

#' Read all files that correspond to a vector of years
#'
#' A helper function for reading mulitple FARS files simultaneously
#'
#' @param years a vector of years
#'
#' @return returns a list of data.frames, one data.frame for each year.
#'   Will provide a NULL value and error message for any year where no corresponding file can be found
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @seealso \link{fars_read}
#'
#' @examples
#' library(magrittr)
#' c(2014,2015,2016) %>%
#'   fars_read_years()
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {

    tryCatch({
      file <- make_filename(year)
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Collect and summarize data over multiple years
#'
#' This function collects data from multiple years and summarizes
#'
#' @param years vector of years
#'
#' @return summary table showing the count of measurements per month and year
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @seealso \link{fars_read_years}
#' @examples
#' library(magrittr)
#' c(2013,2014) %>%
#'   fars_summarize_years() %>%
#'   plot()
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Generate a map showing the number of measurements in a specified state
#'
#' Displays a plot with a state map including the accidents location by year
#'
#' @param state.num An Integer with the State Code
#' \tabular{cc}{
#'   \strong{State Code} \tab \strong{State Name}    \cr
#'   01 \tab  Alabama              \cr
#'   02 \tab  Alaska               \cr
#'   04 \tab  Arizona              \cr
#'   05 \tab  Arkansas             \cr
#'   06 \tab  California           \cr
#'   08 \tab  Colorado             \cr
#'   09 \tab  Connecticut          \cr
#'   10 \tab  Delaware             \cr
#'   11 \tab  District of Columbia \cr
#'   12 \tab  Florida              \cr
#'   13 \tab  Georgia              \cr
#'   15 \tab  Hawaii               \cr
#'   16 \tab  Idaho                \cr
#'   17 \tab  Illinois             \cr
#'   18 \tab  Indiana              \cr
#'   19 \tab  Iowa                 \cr
#'   20 \tab  Kansas               \cr
#'   21 \tab  Kentucky             \cr
#'   22 \tab  Louisiana            \cr
#'   23 \tab  Maine                \cr
#'   24 \tab  Maryland             \cr
#'   25 \tab  Massachusetts        \cr
#'   26 \tab  Michigan             \cr
#'   27 \tab  Minnesota            \cr
#'   28 \tab  Mississippi          \cr
#'   29 \tab  Missouri             \cr
#'   30 \tab  Montana              \cr
#'   31 \tab  Nebraska             \cr
#'   32 \tab  Nevada               \cr
#'   33 \tab  New Hampshire        \cr
#'   34 \tab  New Jersey           \cr
#'   35 \tab  New Mexico           \cr
#'   36 \tab  New York             \cr
#'   37 \tab  North Carolina       \cr
#'   38 \tab  North Dakota         \cr
#'   39 \tab  Ohio                 \cr
#'   40 \tab  Oklahoma             \cr
#'   41 \tab  Oregon               \cr
#'   42 \tab  Pennsylvania         \cr
#'   43 \tab  Puerto Rico          \cr
#'   44 \tab  Rhode Island         \cr
#'   45 \tab  South Carolina       \cr
#'   46 \tab  South Dakota         \cr
#'   47 \tab  Tennessee            \cr
#'   48 \tab  Texas                \cr
#'   49 \tab  Utah                 \cr
#'   50 \tab  Vermont              \cr
#'   51 \tab  Virginia             \cr
#'   52 \tab  Virgin Islands       \cr
#'   53 \tab  Washington           \cr
#'   54 \tab  West Virginia        \cr
#'   55 \tab  Wisconsin            \cr
#'   56 \tab  Wyoming
#' }
#' @param year a string or integer representing a 4-digit year
#'
#' @return returns a map showing the number of accidents in a state
#'
#' @seealso \link{fars_read}
#' @references 2014 FARS/NASS GES Coding and Validation Manual
#' @examples
#' fars_map_state(49, 2015)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
