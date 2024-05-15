# sxz4stat
data tidy, data description, one-way anova, post hoc test analysis

## TOC ##
0. [Installation](#installation)
1. [News](#news)
2. [Basic Usage](#basic-usage)
3. [About author](#about-author)

Installation
devtools I assume that all auxiliary packages are already installed - for example openxlsx, car, agricolae etc. If you get an unexpected error, you may need to download and install a missing dependency.

### Installation

``` devtools::install_github("xzsun/sxz4stat") ```

### News

The latest sxz4stat (version 0.1.4 ) of anova_post_hoc(Demo_df) was revised by corrected Dunncan test to LSD test.

The latest sxz4stat (version 0.1.3 ) now was added the new function,  long_to_adjacentMatrix().

### Basic Usage

``` library(sxz4stat) ```

#### see the structure of data "Demo_df"

``` head(Demo_df) ```

``` anova_post_hoc(Demo_df) ```

#### df_bind()函数

``` df_bind() ```

#### long_to_adjacentMatrix()函数

``` long_to_adjacentMatrix() ```

### About author

Contact me(https://xzsun.github.io/)

