#' @title long_to_adjacentMatrix
#' @description long data turn to width data; or edge data of network turn to adjacent matrix.
#' @details the 1st column of df is the rowname of new dataframe; the 2nd column of df is the colname of new dataframe; the 3rd column of df is the value of new dataframe.
#' @param df is belonging to dataframe.
#' @return A dataframe was formed to easy check.
#' @export long_to_adjacentMatrix
#' @import
#' @importFrom
#' @examples

### 长数据转化为宽数据或边数据转化为邻接矩阵
long_to_adjacentMatrix <- function(df=df)
{
  colnames(df) <- c("Source","Target","Weight")
  mt <- matrix(nrow = length(unique(df$Source)), ncol = length(unique(df$Target)))
  rownames(mt) <- unique(df$Source)
  colnames(mt) <- unique(df$Target)
  for (i in seq(nrow(df))){mt[df$Source[i], df$Target[i]] <- df$Weight[i]}
  mt
}
