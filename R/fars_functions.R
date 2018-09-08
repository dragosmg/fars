#' Read Fatality Analysis Reporting System (FARS) data into a tibble
#'
#' The function takes a file name or a file path pointing to a FARS data set ,
#' checks that the file exists and then reads it into a data frame format. This
#' is a lightweight wrapper around \code{readr::read_csv()}.
#'
#' @param filename a character string giving either the name of or the path to a
#'   file containing FARS data
#'
#' @return This function returns a tibble data frame.
#' @export
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @note An error is thrown if the file indicated does not exist.
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv")
#' }
fars_read <- function(filename) {
        if (!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a filename using the FARS file naming convention
#'
#' Take a year and build a character vector representing the file name.
#'
#' @param year a numeric value indicating the year
#'
#' @return a character vector representing a file name
#' @export
#'
#' @examples
#' \dontrun{
#' make_filename(2014)
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Extract the month from every accident in the data set
#'
#' @param years a numeric vector representing one or more years
#'
#' @return a list of data frames (a data frame for every year) with the month
#'   for every accident in the FARS data set data for each the selected years.
#' @export
#'
#' @importFrom dplyr mutate select
#' @importFrom rlang .data
#'
#' @note If one of the years used as input does not exist the function will
#'   throw a warning, but will read correctly for all the valid values. The
#'   invalid year will have a corresponding empty data frame in the output list.
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(.data$MONTH, .data$year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Count the monthly number of accidents for selected years
#'
#' @param years a numeric vector representing one or more years
#'
#' @return a data frame with the number of accidents for every month for the
#'   selected years
#' @export
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2015))
#' }
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(.data$year, .data$MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(.data$year, .data$n)
}

#' Draw the Map of a State and Plot the Location of the Accidents
#'
#' Select a state and a year and draw the map of the states and overlay the
#' locations of accidents
#'
#' @param state.num a number designating a state
#' @param year a numeric value indicating the year
#'
#' @return the function does not return anything. It plots a map if the input
#'   are in the expected range.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom rlang .data
#' @note If the state number used as input does not exist in the data set, the
#'   function will throw an error indicating the selection was not valid. If for
#'   the chosen combination of state and year there are no accidents the
#'   function will display a message informing there are no accidents to plot.
#' @examples
#' \dontrun{
#' fars_map_state(state.num = 1, year = 2013)
#' }
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, .data$STATE == state.num)
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
