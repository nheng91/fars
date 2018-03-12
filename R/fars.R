#'Read fars data in to R.
#'
#'This function imports a specified csv file and returns a dataframe.  It also checks to see if the file exists.
#'
#'@param filename The csv file, or file path, that is imported and manipulated into a dataframe.
#'@return This function returns a dataframe containing the information from the imported csv file.
#'@importFrom dplyr tbl_df
#'@importFrom readr read_csv
#'@note if the file does not exist in the working directory an error message will display "file does not exist"
#'@examples
#'fars_read("accident_2013.csv.bz2")
#'@export
fars_read <- function(filename) {
  filename <- system.file("extdata",filename,package = "fars")
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'Create filename in R.
#'
#'This function creates a character string that can be used as the name of a file for other functions in this package.
#'
#'@param year the year that will be included in the string output
#'@return The function will return the following string: "accident_year.csv.bz2".  'year' is the parameter value entered.
#'@source US National Highway Traffic Safety Administration's Fatality Analysis Reporting System \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'@examples
#'make_filename(2013)
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'Create fars dataframes.
#'
#'This function creates n data frames of months and years.  The months will vary within each data frame, but the year will remain constant.
#'n will vary with the number of years input.
#'
#'@param years a vector of years in which a separate data frame will be created for each year.
#'@references This function uses both the fars_read and make_filename function from this package as steps to create the data frames output.
#'@return The function will return a list of data frames for each year input as an argument.
#'@source US National Highway Traffic Safety Administration's Fatality Analysis Reporting System \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'@note If a year is input in which a corresponding file does not exist in the working directory, and error will occur.
#'@note Please make sure the csv files and R script are in the same directory, and that directory is the current working directory.
#'@importFrom dplyr mutate select
#'@importFrom magrittr %>%
#'@examples
#'fars_read_years(2013)
#'fars_read_years(c(2013,2014))
#'@export
fars_read_years <- function(years) {

  year <- MONTH <- NULL

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

#'Summarize fars data.
#'
#'This function will create a dataframe with n columns and 12 rows.  The columns represent the years input
#'as a parameter and the rows represent the 12 months of the year.  The values within the data frame
#'represent the total number of observations found for each month/year combination.
#'
#'@inheritParams fars_read_years
#'@references This function directly calls the fars_read_years().
#'@return The function will return a dataframe summarizing the number of observations for a month and year.
#'@source US National Highway Traffic Safety Administration's Fatality Analysis Reporting System \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'@note The function directly calls the fars_read_years() function, and indirectly calls the make_filename() and fars_read() functions
#'so the same notes listed for those functions apply here.
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom magrittr %>%
#'@importFrom tidyr spread
#'@examples
#'fars_summarize_years(2013)
#'fars_summarize_years(c(2013,2014))
#'@export
fars_summarize_years <- function(years) {

  year <- MONTH <- n <- NULL

  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#'Create fatality map.
#'
#'This function creates a map of observations (fatal accidents) that have occurred in a given state during a specified year.
#'
#'@return The function returns a map of the specified state and the location of each observation (fatality) using longitute and latitude.
#'@references The function calls make_filename() and fars_read() from this package
#'@inheritParams make_filename
#'@param state.num a number describing the state that is to be mapped
#'@source US National Highway Traffic Safety Administration's Fatality Analysis Reporting System \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#'@note This function directly calls make_filename() and fars_read(), so any notes and errors that apply to those functions
#'listed above also apply here.
#'@note The state.num must exist in the dataset.  If it does not an error will appear reading "invalid STATE number: "
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'@examples
#'fars_map_state(28,2013)
#'@export
fars_map_state <- function(state.num, year) {

  STATE <- NULL

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
