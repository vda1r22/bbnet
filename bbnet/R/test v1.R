#' @title bbnet
#'
#' @description
#' Turn NA's into zeros - there shouldn't be an apostrophe here :-)
#'
#'@param dataframe description
#'
#' @return dataframe
#'
#' @examples
#' df <- read.csv(here('Data_raw', 'Bali_BBN.csv'))
#' bbnet::na2zero(df)
#'
#' @export

na2zero <- function(dataframe)
{
  dataframe[is.na(dataframe)] <- 0
  return(dataframe)
}
