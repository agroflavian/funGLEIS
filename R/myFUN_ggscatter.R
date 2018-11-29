#' ggscatter --> GLEIS
#'
#' plots a ggscatter  
#' @param df data.frame where data is stored
#' @param xaxis x column of the data.frame
#' @param yaxis y column of the data.frame
#' @param xlab name of the x-axis; default = xaxis
#' @param ylab name of the y-axis; default = yaxis
#' @param regression method how the regression is done --> reg.line or loess, default = reg.line
#' @return return ggscatterplot
#' @keywords regression
#' @export
#' @examples
#' myFUN_ggscatter()
#'
#'
myFUN_ggscatter <- function(df,xaxis, yaxis, xlab= xaxis, ylab= yaxis, regression= "reg.line"){
  
  p <-ggscatter(df, x = xaxis, y = yaxis,
                add = regression, add.params = list(color = "red", fill = "red"), color= "black", conf.int = TRUE, title = paste("Entry", ent, sep = " "),
                cor.coef = TRUE, cor.method = "pearson", xlim= c(0,5),
                xlab = xlab, ylab = ylab)
  return(p)
  
}