#' Read FARS data
#'
#' This function reads the raw data out and returns a data.frame
#'
#' @param filename this is the filename of the file that will be loaded
#'
#' @return returns a dataframe of the loaded data
#' @importFrom magrittr %>%
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' This builds the name of the file based on the year
#'
#' @param year integer
#'
#' @return returns a filename concatenating "accident_" with the year and file extensions
#' @export
#'
#' @examples
#' make_filename(2019)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read all files that correspond to a vector of years
#'
#' @param years a vector of years
#'
#' @return returns a list of data.frames, one data.frame for each year. Will provide a NULL value and error message for any year where no corresponding file can be found
#' @export
#'
#' @examples
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
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
#' @param years vector of years
#'
#' @return summary table showing the count of measurements per month and year
#' @export
#'
#' @examples
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Generate a map showing the number of measurements in a specified state
#'
#' @param state.num integer
#' @param year integer
#'
#' @return returns a map showing the number of accidents in a state
#' @export
#'
#' @examples
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
