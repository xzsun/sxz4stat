#' @title anova_post_hoc
#' @description data anova and the post hoc test
#' @details the first col. must be the treatment as factor; the second col. must be the data for analysis.
#' @param df colname is respectively "tr" and "va", which is factor and numeric.
#' @return A list with the anova result value and the post hoc test result.
#' @export anova_post_hoc
#' @import pacman openxlsx dplyr tidyverse car xtable flextable officer doBy AggregateR agricolae
#' @importFrom car leveneTest
#' @importFrom openxlsx write.xlsx
#' @importFrom doBy order_by
#' @importFrom car recode
#' @importFrom flextable align
#' @examples
#' library(sxz4stat)
#' Demo_df <- data.frame(tr=factor(c("A","A","A","A","B","B","B","B","C","C","C","C")),va=c(10,11,11,12,13,14,14,15,16,17,17,18))
#' anova_post_hoc(Demo_df)
anova_post_hoc <- function(df){
  #如果其中有包未安装，则该条命令会安装并加载它
  pacman::p_load(openxlsx,tidyverse,car,
                 xtable,flextable,officer,
                 doBy,AggregateR,agricolae)
  date <- format(Sys.time(),format="%Y%m%d_%H%M%S")
  sapply(df,class)
  colnames(df) <- c("tr","va")

  # 正态分布检验与方差齐性检验
  pvalue1 <- shapiro.test(df$va)$p.value %>% round(4)
  pvalue2 <- leveneTest(va~factor(tr),data=df)$`Pr(>F)`[1] %>% round(4)

  # output ANOVA results
  aov <- aov(va~tr,data=df)
  ANOVA <- summary(aov,type="III")
  #edit(ANOVAtotal)
  Df <- ANOVA[[1]]["Df"][[1]][1]
  Fvalue <- ANOVA[[1]]["F value"][[1]][1]
  P <- ANOVA[[1]]["Pr(>F)"][[1]][1]
  tp <- P
  sig.lable <- if(tp<0.001){
    tp <- c("***")
  }else if(tp<0.01){
    tp <- c("**")
  }else if(tp<0.05){
    tp <- c("*")
  }else if(tp<0.1){
    tp <- c(".")
  }else if(tp>=0.1){
    tp <- c("")
  }
  dfANOVA <- data.frame(shapiro_test=pvalue1,
                        levene_test=pvalue2,
                        Df=Df,
                        F.value=Fvalue,
                        P=round(P,4),
                        sig.=sig.lable)

  # 计算平均值，标准误，差异字母
  mean1   <- aggregate(df['va'],by=df[c('tr')],mean)
  sd1     <- aggregate(df['va'],by=df[c('tr')],sd)
  length1 <- aggregate(df['va'],by=df[c('tr')],length)
  se1     <- sd1$va/sqrt(length1$va)
  min1    <- aggregate(df['va'],by=df[c('tr')],min)
  max1    <- aggregate(df['va'],by=df[c('tr')],max)

  tr <- data.frame(tr=mean1$tr,mean=mean1$va,sd=sd1$va,se=se1,
                   length=length1$va,min=min1$va,max=max1$va)

  tmslef1 <- tr[order(tr$mean,decreasing = T),]

  #=================================================================#
  # 方差分析及事后检验
  #=================================================================#
  dat1 <- df
  trnum <- length(unique(dat1$tr))
  if(trnum>2){ # ANOVA
    if(pvalue1>=0.05 & pvalue2>=0.05){#Duncan检验
      model1 <- aov(va~tr,data = dat1)
      out <- duncan.test(model1, 'tr', DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
      outabc <- data.frame(out$groups)
    }else if(pvalue1<0.05 | pvalue2<0.05){
      plg1 <- shapiro.test(log1p(dat1$va))$p.value %>% round(4)
      plg2 <- leveneTest(log1p(va)~factor(tr),data=dat1)$`Pr(>F)`[1] %>% round(4)
      if(plg1>=0.05 & plg2>=0.05){#对数转化后Duncan检验
        model1 <- aov(log1p(va)~tr,data = dat1)
        out <- duncan.test(model1, 'tr', DFerror, MSerror, alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
        outabc <- data.frame(out$groups)
      }else if(plg1<0.05 | plg2<0.05){#非参数Kruskal-Waliis秩和检验，并用fdr对p值矫正
        out <- with(dat1,kruskal(va,factor(tr),alpha = 0.05, p.adj=c("fdr"), group=TRUE, main = NULL,console=FALSE))#Bonferroni，最简单严厉的方法。FDR用比较温和的方法对p值进行了校正。
        outabc <- data.frame(out$groups)
      }
    }
    outabc$tr <- rownames(outabc)
    data1 <- merge(tmslef1,outabc,by="tr")
    data1$label <- data1$groups
    data1 <- subset(data1,select = -groups)
  }else if(trnum<=2){ # t-test
    if(pvalue1>=0.05 & pvalue2>=0.05){#t_test检验
      tp <- t.test(va~tr,data = dat1,paired = FALSE,var.equal = F)$p.value
    }else if(pvalue1<0.05 | pvalue2<0.05){
      plg1 <- shapiro.test(log1p(dat1$va))$p.value %>% round(4)
      plg2 <- leveneTest(log1p(va)~factor(tr),data=dat1)$`Pr(>F)`[1] %>% round(4)
      if(plg1>=0.05 & plg2>=0.05){#对数转化后t_test检验
        tp <- t.test(log1p(va)~tr,data = dat1,paired = FALSE,var.equal = F)$p.value
      }else if(plg1<0.05 | plg2<0.05){#非参数Wilicoxon秩和检验，并用fdr对p值矫正
        tp <- wilcox.test(va~tr,data=dat1)$p.value
      }
    }
    if(tp<0.001){
      tmslef1$label <- c("***","***")
    }else if(tp<0.01){
      tmslef1$label <- c("**","**")
    }else if(tp<0.05){
      tmslef1$label <- c("*","*")
    }else if(tp<0.1){
      tmslef1$label <- c(".",".")
    }else if(tp>=0.1){
      tmslef1$label <- c("","")
    }
    data1 <- tmslef1
    data1
  }

  list_data <- list("dfANOVA" = dfANOVA, "data1" = data1)
  return(list_data)

}


