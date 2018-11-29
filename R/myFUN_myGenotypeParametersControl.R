#' A GLEIS function
#'
#' Calculated the slope/intercept/r2 and p value of the regression curve (here in the control group)
#' @param Geno is a single Genotype
#' @keywords regression
#' @export
#' @examples
#' myFUN_myGenotypeParametersControl()
#'
#'
myFUN_myGenotypeParametersControl <- function(Geno){
  tablePlot <- myGWC[[ent]]%>%
    filter(treatment == "control")%>%
    filter(experiment %in% myMaxLcum[[ent]])%>%
    filter(daytime== TRUE)%>%
    # filter(weight < 370)%>%
    filter(gwc_dry > 0.5)%>%
    # filter(gwc_dry < 1.5)%>%
    # filter(complete.cases(gwc_dry,ler_smooth))%>%
    filter(complete.cases(gwc_dry,ler_smooth_diff_date))%>%
    filter(ler_pred_date_smooth > 0)%>%
    filter(experiment == Geno)
  if(length(tablePlot$ler_smooth_diff_date) > 1){
    # fit1 <- lm(formula = ler_smooth ~gwc_dry, data= tablePlot)
    fit1 <- lm(formula = ler_smooth_diff_date ~ gwc_dry, data= tablePlot)
    Pop <- Geno
    Entry <- ent
    Treatment <- "control"
    R2 <- signif(summary(fit1)$adj.r.squared, 5)
    P <- signif(summary(fit1)$coef[2,4], 5)
    Intercept <- signif(fit1$coef[[1]],5 )
    Slope <- signif(fit1$coef[[2]], 5)
  } else {
    Pop <- Geno
    Entry <- ent
    Treatment <- "control"
    R2 <- "NA"
    P <- "NA"
    Intercept <- "NA"
    Slope <- "NA"
  }
  return(c(Pop=Pop, Entry= Entry, Treatment=Treatment, R2=R2, P=P, Intercept=Intercept, Slope=Slope))
}
