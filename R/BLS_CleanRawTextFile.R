#' Cleans a Raw Bureau of Labor Statistics (BLS) Employment File.
#'
#' The BLS Multi-screen data tool \url{http://www.bls.gov/data/#employment} can
#' return a tab-seperated file to the web page which you then cut and paste
#' as a .tsv using some text editor. The file needs some cleaning up. While
#' this is pretty simple in the text editor, this function does all that is
#' needed and writes out a new .tsv which imports nicely with \code{read_tsv()}.
#'
#' These cleanups are done: 1) If first line is "Original Data Value", it is
#' removed; 2) First line should now be field names starting with "Series ID".
#' Error is thrown if it isn't; 3) Spaces in field names are replaced with
#' underscores; 4) Numeric values may have "(C)" or "(P)" suffix to flag
#' value as corrected or preliminary respectively; 5) When either "(C)" or "(P)"
#' are present, a legend is appended to the data file. These lines are removed.
#' 6) Any empty lines at end of file are removed.
#'
#' The cleaned .tsv file is written with \code{append} appended to file name.
#'
#' @param fin The path/name if not in home directory
#' @param fout If present, a file path/name for cleaned file. If omitted, fin
#'   is used with \code{append} appended to file name.
#' @param append A String to append to the string \code{fin}. Ignored when fout
#'   is not null string.
#'
#' @return The path/name of the cleaned file if succussful else an empty string.
#' @export
#'
#' @examples
#' \dontrun{
#' BLS_CleanRawTextFile("data/BLS_NonFarmEmploymentInAreasAL_2015.tsv")
#' }
BLS_CleanRawTextFile <- function(fin, fout = NA, append = "_Clean") {
  if(file.access(fin) == -1) stop(paste(fin, "is not valid path/file"))
  if(str_sub(fin, -4, -1) != ".tsv") stop(paste(fin, "is not a '.tsv'"))
  if(is.na(fout)) {
    file_out <- str_replace(fin, "\\.tsv$", paste0(append, ".tsv"))
  } else {
    file_out <- fout
  }
  f <- read_lines(fin)

  # drop lines before column name header
  while(!str_detect(f[1], "^Series ID")){
    f <- f[-1]
  }
  if(length(f) == 0) stop(paste(fin, "does not have column name header starting with 'Series ID'"))

  # convert spaces in column names to underscore
  f[1] <- str_replace_all(f[1], fixed(" "), fixed("_"))

  # remove any "(C)" or "(P)" flags in data cols
  f <- str_replace(f, fixed("(C)"), "")
  f <- str_replace(f, fixed("(P)"), "")

  # remove trailing lines that are not data (assuming only data rows start with series prefix)
  series <- str_sub(f[2], 1, 3)
  i <- length(f)
  while(!str_detect(f[i], paste0("^", series)) & i > 0){
    f <- f[-i]
    i <- i - 1
  }
  if(length(f) < 2) stop(paste(fin, "has no data rows"))

  writeLines(f, file_out)

}