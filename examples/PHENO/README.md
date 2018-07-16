
# PHENO

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.0.0     √ purrr   0.2.5
    ## √ tibble  1.4.2     √ dplyr   0.7.6
    ## √ tidyr   0.8.1     √ stringr 1.3.1
    ## √ readr   1.1.1     √ forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
df <- read_table('PHENO', col_names = FALSE) %>% 
  set_names(c('ID','TIME','AMT','BWT','APGA','DV','MDV','EVID')) %>% 
  filter(DV != '.') %>% 
  mutate(DV = as.numeric(DV))
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_integer(),
    ##   X2 = col_double(),
    ##   X3 = col_character(),
    ##   X4 = col_double(),
    ##   X5 = col_integer(),
    ##   X6 = col_character(),
    ##   X7 = col_integer(),
    ##   X8 = col_integer()
    ## )

``` r
head(df)
```

<div class="kable-table">

| ID |  TIME | AMT | BWT | APGA |   DV | MDV | EVID |
| -: | ----: | :-- | --: | ---: | ---: | --: | ---: |
|  1 |   2.0 | .   | 1.4 |    7 | 17.3 |   0 |    0 |
|  1 | 112.5 | .   | 1.4 |    7 | 31.0 |   0 |    0 |
|  2 |   2.0 | .   | 1.5 |    9 |  9.7 |   0 |    0 |
|  2 |  63.5 | .   | 1.5 |    9 | 24.6 |   0 |    0 |
|  2 | 135.5 | .   | 1.5 |    9 | 33.0 |   0 |    0 |
|  3 |   1.5 | .   | 1.5 |    6 | 18.0 |   0 |    0 |

</div>

``` r
ggplot(df, aes(x=TIME, y=DV, group=ID, color=ID)) +
  geom_point()+
  geom_line()
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

    ## Warning: Removed 1 rows containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
