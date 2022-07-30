# sxz4stat
data tidy, data description, one-way anova, post hoc test analysis

> Content
> Installation
> News
> Basic Usage

Installation
devtools I assume that all auxiliary packages are already installed - for example openxlsx, car, agricolae etc. If you get an unexpected error, you may need to download and install a missing dependency.

### install R package of "sxz4stat":

``` devtools::install_github("xzsun/sxz4stat")

### News

The latest sxz4stat (version 1.0.0 and higher) now uses the pulsar package for stability-based model selection.

### Basic Usage

``` library(sxz4stat)

### see the structure of data "Demo_df"

``` head(Demo_df)

``` anova_post_hoc(Demo_df)

### df_bind()函数

``` df_bind()

