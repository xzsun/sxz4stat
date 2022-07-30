#' @title df_bind
#' @description calculate mean and sum of the df according group of the gr and taxonomy of the ta.
#' @details colname of df is fit with the rowname of gr; and rowname of df is fit with the rowname of ta. col.n is one of the colnames of gr, and row.n is one of the colnames of ta.
#' @param df, gr, ta are belonging to dataframe.
#' @return A dataframe was averaged by some group of gr and sumed by some taxonomy of ta.
#' @export df_bind
#' @import pacman tidyverse dplyr
#' @importFrom dplyr %>%
#' @examples

df_bind <- function(df=df,gr=gr,ta=ta,col.n=col.n,row.n=row.n){

  pacman::p_load(tidyverse)

  group <- gr
  colnames(group) <- gsub(col.n, "col.n", colnames(group))

  ## OTU丰度表中的样品全部都在group中
  ot1 <- colnames(df) %in% rownames(group)
  ot2 <- df[,ot1]


  ### 选择gr分组合并求平均值 ###########################################################
  ## 先转置为行名是样本名称
  ot3 <- data.frame(t(ot2))

  # 检查两个数据框的行名是否一致,0则表示完全一致
  sum(!(rownames(ot3)==rownames(group)))

  ot4 <- cbind(tr=group$col.n,ot3) #@#@#@#@#@
  ot5 <- aggregate(.~tr,ot4,mean)

  rownames(ot5) <- ot5[,1]
  ot6 <- subset(ot5,select=-c(tr))

  otu <- ot6 %>% t() %>% as.data.frame()


  ### 选择tax分类合并求和分类物种 ###########################################################
  #taxon # 2Kingdom,3Phylum,4Class,5Order,6Family,7Genus,8Species
  tax <- ta
  colnames(tax) <- gsub(row.n, "row.n", colnames(tax)) #@#@#@#@#@

  taxOTU <- cbind(taxon=tax$row.n,otu)

  ## 合并求和
  dat13 <- taxOTU %>% group_by(taxon) %>% summarise_if(is.numeric,sum) %>% as.data.frame()

  rownames(dat13) <- dat13[,1]
  dat1 <- subset(dat13,select=-c(taxon))

  return(dat1)

}
