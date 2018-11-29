#' A Test Function
#'
#' This function allows to test my package.
#' @param try Ciao Zoe, sta bene? Defaults to TRUE.
#' @return string of characters
#' @keywords test
#' @export
#' @examples c <- test_function(TRUE), print(c) / c <-test_function(FALSE), print(c)
#' cat_function()
test_function <- function(try=TRUE){
  if(try==TRUE){
    print("Ciao Zoe, sta bene?")
  }
  else {
    print("WHY THE HELL YOU TYPE FALSE?!?")
  }
}