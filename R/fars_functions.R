
library(magrittr)
library(maps)

#' Import Accident Raw Data
#'
#' This function takes a filename and load the file into a R tibble dataframe.
#' If the file path/name is not found or valid, an error message is retrieved.
#'
#' This function uses and imports readr and dplyr packages form tidyverse package repository.
#'
#' @param filename A character value representing the filename to import.
#' @return A tibble dataframe representing the file \code{filename} loaded.
#'
#' @examples
#' fars_read("filename.csv")

fars_read <- function(filename) {

  filePath = system.file("extdata", filename, package = "AccidentR")

  if(!file.exists(filePath))
    stop("file ", filename, " does not exist")
  data <- suppressMessages({
  readr::read_csv(filePath, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create filename
#'
#' This function takes a (\code{year}) as input and retrieves a character output consisting of a file name.
#'
#' @param year A character variable representing the year to pass to the returned filename.
#' @return A file name (character value) using the year passed in the format 'accident_\code{year}.csv.bz2'.
#'
#' @examples
#' make_filename("2015")


make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Import all files to a list of dataframes
#'
#' This function takes a vector of years and returns a list of R tibble dataframes consisting of the files loaded.
#' The function loops over the vector of years to load each of the Accident files on the directory whilst manipulating the data on them
#' to create a new collumn "year" and select only the columns "MONTH" and "year".
#'
#' The function uses fars_read function (\code{\link{fars_read}}) and make_filename (\code{\link{make_filename}}) to execute the described process.
#'
#' If an invalid year is retrieved (i.e. if there is no file with the parsed year input), an error is retrieved.
#'
#' This function uses dplyr package form tidyverse package repository.
#'
#' @param years A character vector of years.
#' @return A list of R tibble dataframes.
#'
#' @examples
#' years <- c("2013","2014","2015")
#' fars_read_years(years)

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


#' Import all files
#'
#' This function takes a vector of years (\code{years}) to create an R tibble dataframe consisting of the respective files combined (for each year passed in \code{years}).
#' It then combines together these dataframes into one (using function (\code{\link{fars_read_years}})) to perform aggregations on volumen of accidents per year/month.
#'
#' The function uses fars_read_years function (\code{\link{fars_read_years}}) to execute the described process.
#'
#' This function uses dplyr and tidyr packages form tidyverse package repository.
#'
#' @param years A character vector of years.
#' @return A R tibble (wide format) dataframe with 'Years' on the rows and 'Months' as columns with the volume of accidents as values.
#'
#' @examples
#' years <- c("2013","2014","2015")
#' fars_summarize_years(years)
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot State Level Accidents (Geo-Plotting)
#'
#' This function takes a State Number (\code{state.num}) and a Year (\code{year}) to import a file (based on the year parsed) as a R tibble dataframe.
#' After filtering the dataframe based on State Number (\code{state.num}), the function outputs a geo-plot of the filtered State Number (\code{state.num}) with the volume of accidents.
#'
#' The function uses fars_read function (\code{\link{fars_read}}) and make_filename (\code{\link{make_filename}}) to execute the described process.
#'
#' If State Number (\code{state.num}) doesn't exist on the dataframe an error is returned (i.e. invalid STATE number: 324).
#' If the filtered dataframe has zero records - the function returns a message 'no accidents to plot'.
#'
#' Longitudes higher than 900 and Latitudes > than 90 are treated as NAs for plotting purposes.
#'
#' @param state.num A character variable representing the State Number reference to use to plot the volume of accidents.
#' @param year A character variable representing the year of the file to import.
#' @return A geo-plot of the State Number (\code{state.num}) with the volume of accidents.
#'
#' @examples
#' fars_map_state(state.num = "6", year = "2015")
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



