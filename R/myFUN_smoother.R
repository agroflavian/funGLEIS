#' Smooting function --> GLEIS
#'
#' smoothing data by moving average 
#' @param Geno is a single Genotype
#' @param smoothFactor number; how smooth the values should get, the higher the smoother
#' @param column name of the column e.g. "ler" ATTENTION: WRITE IN QUOTATION MARKS
#' @return returns a dataframe with 3 columns, input (smoothFactor, rowName, smoothed values)
#' @keywords regression
#' @export
#' @examples
#' myFUN_smoother()
#'
#'
myFUN_smoother <- function(geno, smoothFactor, column){
  rowName <- "rowName"
  smoothtest <- myGWC[[ent]]%>%
    filter(experiment == geno)%>%
    select_(column, rowName)%>%
    mutate(smoothed= rollmean(.[[1]], k= smoothFactor, fill = NA))
  return(data.frame(smoothtest))
}