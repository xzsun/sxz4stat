#' @title Demo data
#' @description dataframe
#' @details the first col. must be the treatment as factor; the second col. must be the data for analysis.
#' @param df colname is respectively "tr" and "va", which is factor and numeric.
#' @return A list with the anova result value and the post hoc test result.
#' @examples Demo_df
Demo_df <- data.frame(tr=factor(c("A","A","A","A","B","B","B","B","C","C","C","C")),
                      va=c(10,11,11,12,13,14,14,15,16,17,17,18))

