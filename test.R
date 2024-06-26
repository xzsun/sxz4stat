###激活R文件夹内的所有函数，供测试使用
devtools::load_all()

anova_post_hoc(Demo_df)

####然后使用下述函数，自动编写License部分
# 如果没有依赖到别的具有不同版权的第三方包的话，一般选择最为广泛使用的 MIT 即可
usethis::use_mit_license()

###之后再补充依赖包的信息。分为两类：Imports、Suggests、Depends。
###假设其中的一个自定义的函数必须要用到dplyr包的filter()函数（因为实际中并未没有使用，之后会删去）
usethis::use_package('pacman',
                     type = "Imports",
                     min_version = "0.5.1")
usethis::use_package('openxlsx',
                     type = "Imports",
                     min_version = "1.0")
usethis::use_package('car',
                     type = "Imports",
                     min_version = "1.0")
usethis::use_package('xtable',
                     type = "Imports",
                     min_version = "1.0")
usethis::use_package('flextable',
                     type = "Imports",
                     min_version = "0.6.10")
usethis::use_package('officer',
                     type = "Imports",
                     min_version = "0.4.1")
usethis::use_package('doBy',
                     type = "Imports",
                     min_version = "1.0")
usethis::use_package('AggregateR',
                     type = "Imports",
                     min_version = "0.1.1")
usethis::use_package('agricolae',
                     type = "Imports",
                     min_version = "1.0")


###生成示例数据
usethis::use_data(Demo_df)


library(devtools)
library(roxygen2)
###为所有函数在man文件夹(如果没有，会创建)下逐一自动建立Rd文档，以及更新NAMESPCAE文档
devtools::document()
# warnings()

###最后再check一下上述的所有文件，包括函数脚本，看看有没有问题
devtools::check()

### 将包制成压缩包(*.tar.gz)，方便传输或者分享
#方法一： 先打开“.Rproj”文件，再点击Build → Build Source Package

#方法二：
devtools::build()

install.packages("C:/Users/zhan/Desktop/sxz4stat_0.1.4.tar.gz",
                 repos=NULL, type="source")

# library(sxz4stat)
# Demo_df

# sxz4stat::long_to_adjacentMatrix(Demo_longdf)

# Demo_longdf <- data.frame(OTU=rep(c("OTU1","OTU2","OTU3","OTU4","OTU5"),time=3),
#                           Sample=rep(c("S1","S2","S3"),each=5),
#                           Value=c(4,6,9,7,5,7,9,9,6,8,9,8,9,4,7))
# save(Demo_longdf,file="Demo_longdf.rda")


