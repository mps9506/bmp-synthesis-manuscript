
# A meta-analysis of the impacts of best management practices on nonpoint source pollutant concentration

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10795231.svg)](https://doi.org/10.5281/zenodo.10795231)

This repository contains the data and analysis used in the draft
manuscript *“A meta-analysis of the impacts of best management practices
on nonpoint source pollutant concentration,”* by Schramm et al.

This project uses `{targets}` as the main organizing workflow. The
`_targets.R` file defines the steps of the project workflow amd
functions for those setps are stored in the `R/` directory. `renv` is
used to manage package versions. To reproduce locally, clone or download
this project to your computer. Open the project with RStudio or other
IDE.

In the console:

``` r
renv::restore()
```

then use `targets::tar_make()` to execute the project pipeline. Each
step in the pipeline can be accessed with the `targets::tar_read()`
function. For example, `targets::tar_read(tn_model)` will access the
model object we fit to the total nitrogen data.

## Funding Statement

This project was funded by a nonpoint source grant from the Texas State
Soil and Water Conservation Board.

## Working Environment Info

``` r
renv::diagnostics()
```

    ## Diagnostics Report [renv 1.0.3]
    ## ===============================
    ## 
    ## # Session Info ---------------------------------------------------------------
    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/New_York
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices datasets  utils     methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.3.1      BiocManager_1.30.22 fastmap_1.1.1      
    ##  [4] cli_3.6.1           htmltools_0.5.7     tools_4.3.1        
    ##  [7] rstudioapi_0.15.0   yaml_2.3.7          rmarkdown_2.25     
    ## [10] knitr_1.45          xfun_0.41           digest_0.6.33      
    ## [13] rlang_1.1.2         renv_1.0.3          evaluate_0.23      
    ## 
    ## # Project --------------------------------------------------------------------
    ## Project path: "C:/Data-Analysis-Projects/bmp-synthesis-manuscript"
    ## 
    ## # Status ---------------------------------------------------------------------
    ## No issues found -- the project is in a consistent state.
    ## 
    ## # Packages -------------------------------------------------------------------
    ##                      Library                         Source   Lockfile                         Source Path Dependency
    ## BH                  1.81.0-1                           CRAN   1.81.0-1                           CRAN  [1]   indirect
    ## BiocManager          1.30.22                           CRAN    1.30.22                           CRAN  [1]   indirect
    ## BiocVersion           3.18.1                   Bioconductor     3.18.1                   Bioconductor  [1]   indirect
    ## DBI                    1.1.3                           CRAN      1.1.3                           CRAN  [1]   indirect
    ## DEoptimR               1.1-3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## DT                      0.30                           RSPM       <NA>                           <NA>  [1]       <NA>
    ## DiagrammeR            1.0.10                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## DiagrammeRsvg            0.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## Formula                1.2-5                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## Hmisc                  5.1-1                           RSPM       <NA>                           <NA>  [1]       <NA>
    ## KernSmooth           2.23-22                           CRAN    2.23-22                           CRAN  [2]   indirect
    ## MASS                  7.3-60                           CRAN     7.3-60                           CRAN  [2]   indirect
    ## Matrix               1.6-1.1                           CRAN    1.6-1.1                           CRAN  [2]   indirect
    ## MatrixModels           0.5-3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## MuMIn                 1.47.5                           CRAN     1.47.5                           CRAN  [1]     direct
    ## PRISMA2020             1.1.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## R.methodsS3            1.8.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## R.oo                  1.25.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## R.utils               2.12.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## R6                     2.5.1                           CRAN      2.5.1                           CRAN  [1]   indirect
    ## RColorBrewer           1.1-3                           CRAN      1.1-3                           CRAN  [1]     direct
    ## Rcpp                  1.0.11                           CRAN     1.0.11                           CRAN  [1]   indirect
    ## RcppArmadillo     0.12.6.6.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## RcppEigen          0.3.3.9.4                           CRAN  0.3.3.9.4                           CRAN  [1]   indirect
    ## SparseM                 1.81                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## TTR                   0.24.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## V8                     4.4.1                           CRAN      4.4.1                           CRAN  [1]   indirect
    ## abind                  1.4-5                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## anytime                0.3.9                           CRAN      0.3.9                           CRAN  [1]   indirect
    ## askpass                1.2.0                           RSPM      1.2.0                           RSPM  [1]   indirect
    ## assertthat             0.2.1                           CRAN      0.2.1                           CRAN  [1]   indirect
    ## backports              1.4.1                           CRAN      1.4.1                           CRAN  [1]   indirect
    ## base                    <NA>                           <NA>       <NA>                           <NA>  [2]     direct
    ## base64enc              0.1-3                           CRAN      0.1-3                           CRAN  [1]   indirect
    ## base64url                1.4                           CRAN        1.4                           CRAN  [1]   indirect
    ## bayestestR            0.13.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## beeswarm               0.4.0                           CRAN      0.4.0                           CRAN  [1]   indirect
    ## bigD                   0.2.0                           CRAN      0.2.0                           CRAN  [1]   indirect
    ## bit                    4.0.5                           CRAN      4.0.5                           CRAN  [1]   indirect
    ## bit64                  4.0.5                           CRAN      4.0.5                           CRAN  [1]   indirect
    ## bitops                 1.0-7                           CRAN      1.0-7                           CRAN  [1]   indirect
    ## blob                   1.2.4                           CRAN      1.2.4                           CRAN  [1]   indirect
    ## bookdown                0.37                           CRAN       0.37                           CRAN  [1]     direct
    ## boot                1.3-28.1                           CRAN       <NA>                           <NA>  [2]       <NA>
    ## brio                   1.1.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## broom                  1.0.5                           CRAN      1.0.5                           CRAN  [1]     direct
    ## broom.helpers         1.14.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## bs4Dash                2.3.3                           RSPM      2.3.3                           RSPM  [1]     direct
    ## bslib                  0.6.1                           RSPM      0.6.1                           RSPM  [1]   indirect
    ## cachem                 1.0.8                           CRAN      1.0.8                           CRAN  [1]   indirect
    ## callr                  3.7.3                           CRAN      3.7.3                           CRAN  [1]   indirect
    ## car                    3.1-2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## carData                3.0-5                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## cellranger             1.1.0                           CRAN      1.1.0                           CRAN  [1]   indirect
    ## checkmate              2.3.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## class                 7.3-22                           CRAN     7.3-22                           CRAN  [2]   indirect
    ## classInt              0.4-10                           RSPM     0.4-10                           RSPM  [1]   indirect
    ## cli                    3.6.1                           CRAN      3.6.1                           CRAN  [1]   indirect
    ## clipr                  0.8.0                           CRAN      0.8.0                           CRAN  [1]   indirect
    ## clubSandwich          0.5.10                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## cluster                2.1.4                           CRAN       <NA>                           <NA>  [2]       <NA>
    ## clustermq              0.9.4                           CRAN      0.9.4                           CRAN  [1]     direct
    ## codetools             0.2-19                           CRAN     0.2-19                           CRAN  [2]   indirect
    ## colorspace             2.1-0                           CRAN      2.1-0                           CRAN  [1]   indirect
    ## commonmark             1.9.0                           CRAN      1.9.0                           CRAN  [1]   indirect
    ## compiler                <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## conflicted             1.2.0                           CRAN      1.2.0                           CRAN  [1]   indirect
    ## correlation            0.8.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## corrplot                0.92                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## cowplot                1.1.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## cpp11                  0.4.6                           CRAN      0.4.6                           CRAN  [1]   indirect
    ## crayon                 1.5.2                           CRAN      1.5.2                           CRAN  [1]   indirect
    ## crosstalk              1.2.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## crul                   1.4.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## curl                   5.1.0                           RSPM      5.1.0                           RSPM  [1]   indirect
    ## data.table            1.14.8                           CRAN     1.14.8                           CRAN  [1]   indirect
    ## datawizard             0.9.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## dbplyr                 2.4.0                           RSPM      2.4.0                           RSPM  [1]   indirect
    ## desc                   1.4.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## diffobj                0.3.5                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## digest                0.6.33                           CRAN     0.6.33                           CRAN  [1]   indirect
    ## diptest               0.77-0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## downloader               0.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## dplyr                  1.1.4                           RSPM      1.1.4                           RSPM  [1]     direct
    ## dtplyr                 1.3.1                           CRAN      1.3.1                           CRAN  [1]   indirect
    ## e1071                 1.7-14                           CRAN     1.7-14                           CRAN  [1]   indirect
    ## easystats              0.7.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## effectsize             0.8.6                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## ellipsis               0.3.2                           CRAN      0.3.2                           CRAN  [1]   indirect
    ## emmeans                1.8.9                           CRAN      1.8.9                           CRAN  [1]   indirect
    ## estimability           1.4.1                           CRAN      1.4.1                           CRAN  [1]   indirect
    ## evaluate                0.23                           RSPM       0.23                           RSPM  [1]   indirect
    ## fansi                  1.0.5                           RSPM      1.0.5                           RSPM  [1]   indirect
    ## farver                 2.1.1                           CRAN      2.1.1                           CRAN  [1]   indirect
    ## fastmap                1.1.1                           CRAN      1.1.1                           CRAN  [1]   indirect
    ## flexmix               2.3-19                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## flextable              0.9.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## fontBitstreamVera      0.1.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## fontLiberation         0.1.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## fontawesome            0.5.2                           CRAN      0.5.2                           CRAN  [1]   indirect
    ## fontquiver             0.2.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## forcats                1.0.0                           CRAN      1.0.0                           CRAN  [1]     direct
    ## foreach                1.5.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## forecast              8.21.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## foreign               0.8-85                           CRAN       <NA>                           <NA>  [2]       <NA>
    ## fpc                   2.2-11                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## fracdiff               1.5-2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## fresh                  0.2.0                           CRAN      0.2.0                           CRAN  [1]   indirect
    ## fs                     1.6.3                           CRAN      1.6.3                           CRAN  [1]     direct
    ## future                1.33.2                           CRAN     1.33.2                           CRAN  [1]     direct
    ## gargle                 1.5.2                           CRAN      1.5.2                           CRAN  [1]   indirect
    ## gdtools                0.3.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## generics               0.1.3                           CRAN      0.1.3                           CRAN  [1]   indirect
    ## gfonts                 0.2.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## ggbeeswarm             0.7.2                           CRAN      0.7.2                           CRAN  [1]   indirect
    ## ggforce                0.4.1                           CRAN      0.4.1                           CRAN  [1]     direct
    ## ggplot2                3.4.4                           RSPM      3.4.4                           RSPM  [1]     direct
    ## ggpubr                 0.6.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## ggrepel                0.9.5                           CRAN      0.9.5                           CRAN  [1]     direct
    ## ggridges               0.5.6                           CRAN      0.5.6                           CRAN  [1]     direct
    ## ggsci                  3.0.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## ggsignif               0.6.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## ggtext                 0.1.2                           CRAN      0.1.2                           CRAN  [1]     direct
    ## glmnet                 4.1-8                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## globals               0.16.3                           CRAN     0.16.3                           CRAN  [1]   indirect
    ## glue                   1.6.2                           CRAN      1.6.2                           CRAN  [1]   indirect
    ## googledrive            2.1.1                           CRAN      2.1.1                           CRAN  [1]   indirect
    ## googlesheets4          1.1.1                           CRAN      1.1.1                           CRAN  [1]   indirect
    ## grDevices               <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## graphics                <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## grid                    <NA>                           <NA>       <NA>                           <NA>  [2]     direct
    ## gridExtra                2.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## gridtext               0.1.5                         GitHub      0.1.5                         GitHub  [1]     direct
    ## gt                    0.10.0                           CRAN     0.10.0                           CRAN  [1]     direct
    ## gtable                 0.3.4                           CRAN      0.3.4                           CRAN  [1]   indirect
    ## gtsummary              1.7.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## haven                  2.5.4                           RSPM      2.5.4                           RSPM  [1]   indirect
    ## highr                   0.10                           CRAN       0.10                           CRAN  [1]   indirect
    ## hms                    1.1.3                           CRAN      1.1.3                           CRAN  [1]   indirect
    ## htmlTable              2.4.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## htmltools              0.5.7                           RSPM      0.5.7                           RSPM  [1]   indirect
    ## htmlwidgets            1.6.3                           CRAN      1.6.3                           CRAN  [1]   indirect
    ## httpcode               0.3.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## httpuv                1.6.12                           CRAN     1.6.12                           CRAN  [1]   indirect
    ## httr                   1.4.7                           CRAN      1.4.7                           CRAN  [1]     direct
    ## ids                    1.0.1                           CRAN      1.0.1                           CRAN  [1]   indirect
    ## igraph                 1.5.1                           RSPM      1.5.1                           RSPM  [1]   indirect
    ## insight               0.19.7                           CRAN     0.19.7                           CRAN  [1]   indirect
    ## isoband                0.2.7                           CRAN      0.2.7                           CRAN  [1]   indirect
    ## iterators             1.0.14                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## jomo                   2.7-6                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## jpeg                  0.1-10                           CRAN     0.1-10                           CRAN  [1]   indirect
    ## jquerylib              0.1.4                           CRAN      0.1.4                           CRAN  [1]   indirect
    ## jsonlite               1.8.7                           CRAN      1.8.7                           CRAN  [1]   indirect
    ## juicyjuice             0.1.0                           CRAN      0.1.0                           CRAN  [1]   indirect
    ## kableExtra             1.3.4                           CRAN      1.3.4                           CRAN  [1]     direct
    ## kernlab               0.9-32                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## knitr                   1.45                           RSPM       1.45                           RSPM  [1]     direct
    ## labeling               0.4.3                           CRAN      0.4.3                           CRAN  [1]   indirect
    ## labelled              2.12.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## later                  1.3.1                           CRAN      1.3.1                           CRAN  [1]   indirect
    ## latex2exp              0.9.6                           CRAN      0.9.6                           CRAN  [1]     direct
    ## latexdiffr        0.2.0.9000                         GitHub 0.2.0.9000                         GitHub  [1]     direct
    ## lattice               0.21-8                           CRAN     0.21-8                           CRAN  [2]   indirect
    ## lazyeval               0.2.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## lifecycle              1.0.4                           RSPM      1.0.4                           RSPM  [1]   indirect
    ## listenv                0.9.1                           CRAN      0.9.1                           CRAN  [1]   indirect
    ## lme4                1.1-35.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## lmtest                0.9-40                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## lubridate              1.9.3                           RSPM      1.9.3                           RSPM  [1]   indirect
    ## magrittr               2.0.3                           CRAN      2.0.3                           CRAN  [1]   indirect
    ## markdown                1.11                           RSPM       1.11                           RSPM  [1]     direct
    ## mathjaxr               1.6-0                           CRAN      1.6-0                           CRAN  [1]   indirect
    ## mclust                 6.0.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## memoise                2.0.1                           CRAN      2.0.1                           CRAN  [1]   indirect
    ## metadat                1.2-0                           CRAN      1.2-0                           CRAN  [1]   indirect
    ## metafor                4.4-0                           CRAN      4.4-0                           CRAN  [1]     direct
    ## metagear                 0.7                           CRAN        0.7                           CRAN  [1]     direct
    ## metaviz                0.3.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## methods                 <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## mgcv                   1.9-0                           CRAN      1.9-0                           CRAN  [1]   indirect
    ## mice                  3.16.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## mime                    0.12                           CRAN       0.12                           CRAN  [1]   indirect
    ## minqa                  1.2.6                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## mitml                  0.4-5                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## modelbased             0.8.6                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## modelr                0.1.11                           CRAN     0.1.11                           CRAN  [1]     direct
    ## modelsummary           1.4.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## modeltools            0.2-23                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## moments               0.14.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## mpsTemplates           0.2.0 https://mps9506.r-universe.dev      0.2.0 https://mps9506.r-universe.dev  [1]     direct
    ## munsell                0.5.0                           CRAN      0.5.0                           CRAN  [1]   indirect
    ## mvtnorm                1.2-4                           CRAN      1.2-4                           CRAN  [1]   indirect
    ## narray                 0.5.1                           CRAN      0.5.1                           CRAN  [1]   indirect
    ## nlme                 3.1-163                           CRAN    3.1-163                           CRAN  [2]   indirect
    ## nloptr                 2.0.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## nnet                  7.3-19                           CRAN       <NA>                           <NA>  [2]       <NA>
    ## nullabor               0.3.9                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## numDeriv          2016.8-1.1                           CRAN 2016.8-1.1                           CRAN  [1]   indirect
    ## officedown             0.3.1                           RSPM       <NA>                           <NA>  [1]       <NA>
    ## officer                0.6.3                           CRAN      0.6.3                           CRAN  [1]     direct
    ## openssl                2.1.1                           RSPM      2.1.1                           RSPM  [1]   indirect
    ## orchaRd                  2.0                         GitHub        2.0                         GitHub  [1]     direct
    ## ordinal            2023.12-4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## pan                      1.9                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## parallel                <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## parallelly            1.37.1                           CRAN     1.37.1                           CRAN  [1]   indirect
    ## parameters            0.21.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## patchwork              1.1.3                           CRAN      1.1.3                           CRAN  [1]     direct
    ## pbapply                1.7-2                           CRAN      1.7-2                           CRAN  [1]   indirect
    ## pbkrtest               0.5.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## performance           0.10.8                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## pillar                 1.9.0                           CRAN      1.9.0                           CRAN  [1]   indirect
    ## pingr                  2.0.3                           RSPM      2.0.3                           RSPM  [1]     direct
    ## pkgbuild               1.4.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## pkgconfig              2.0.3                           CRAN      2.0.3                           CRAN  [1]   indirect
    ## pkgload                1.3.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## png                    0.1-8                           CRAN      0.1-8                           CRAN  [1]   indirect
    ## polyclip              1.10-6                           CRAN     1.10-6                           CRAN  [1]   indirect
    ## polynom                1.4-1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## prabclus               2.3-3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## praise                 1.0.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## prettyunits            1.2.0                           RSPM      1.2.0                           RSPM  [1]   indirect
    ## processx               3.8.2                           CRAN      3.8.2                           CRAN  [1]   indirect
    ## progress               1.2.2                           CRAN      1.2.2                           CRAN  [1]   indirect
    ## promises               1.2.1                           CRAN      1.2.1                           CRAN  [1]   indirect
    ## proxy                 0.4-27                           CRAN     0.4-27                           CRAN  [1]   indirect
    ## ps                     1.7.5                           CRAN      1.7.5                           CRAN  [1]   indirect
    ## purrr                  1.0.2                           CRAN      1.0.2                           CRAN  [1]     direct
    ## quadprog               1.5-8                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## quantmod              0.4.25                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## quantreg                5.97                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## ragg                   1.2.6                           RSPM      1.2.6                           RSPM  [1]     direct
    ## ranger                0.16.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## rappdirs               0.3.3                           CRAN      0.3.3                           CRAN  [1]   indirect
    ## reactR                 0.5.0                           CRAN      0.5.0                           CRAN  [1]   indirect
    ## reactable              0.4.4                           CRAN      0.4.4                           CRAN  [1]   indirect
    ## readr                  2.1.4                           CRAN      2.1.4                           CRAN  [1]   indirect
    ## readxl                 1.4.3                           CRAN      1.4.3                           CRAN  [1]     direct
    ## rematch                2.0.0                           CRAN      2.0.0                           CRAN  [1]   indirect
    ## rematch2               2.1.2                           CRAN      2.1.2                           CRAN  [1]   indirect
    ## renv                   1.0.3                           CRAN      1.0.3                           CRAN  [1]     direct
    ## report                 0.5.8                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## reprex                 2.0.2                           CRAN      2.0.2                           CRAN  [1]   indirect
    ## rio                    1.0.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## rlang                  1.1.2                           RSPM      1.1.2                           RSPM  [1]   indirect
    ## rmarkdown               2.25                           RSPM       2.25                           RSPM  [1]     direct
    ## rmdfiltr               0.1.3                         GitHub       <NA>                           <NA>  [1]       <NA>
    ## robustbase            0.99-1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## rpart                 4.1.19                           CRAN       <NA>                           <NA>  [2]       <NA>
    ## rprojroot              2.0.4                           CRAN      2.0.4                           CRAN  [1]   indirect
    ## rstatix                0.7.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## rstudioapi            0.15.0                           CRAN     0.15.0                           CRAN  [1]     direct
    ## rsvg                   2.6.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## rticles               0.26.1                         GitHub     0.26.1                         GitHub  [1]     direct
    ## rvest                  1.0.3                           CRAN      1.0.3                           CRAN  [1]   indirect
    ## rvg                    0.3.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## s2                     1.1.6                           CRAN      1.1.6                           CRAN  [1]   indirect
    ## sandwich               3.1-0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## sass                   0.4.7                           CRAN      0.4.7                           CRAN  [1]   indirect
    ## scales                 1.3.0                           RSPM      1.3.0                           RSPM  [1]     direct
    ## see                    0.8.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## selectr                0.4-2                           CRAN      0.4-2                           CRAN  [1]   indirect
    ## sessioninfo            1.2.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## sf                    1.0-15                           CRAN     1.0-15                           CRAN  [1]     direct
    ## shape                  1.4.6                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## shiny                  1.8.0                           CRAN      1.8.0                           CRAN  [1]     direct
    ## shinyWidgets           0.8.6                           RSPM      0.8.6                           RSPM  [1]     direct
    ## shinybusy              0.3.3                           RSPM      0.3.3                           RSPM  [1]     direct
    ## shinyjs                2.1.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## sourcetools          0.1.7-1                           CRAN    0.1.7-1                           CRAN  [1]   indirect
    ## spatial               7.3-17                           CRAN       <NA>                           <NA>  [2]       <NA>
    ## splines                 <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## stats                   <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## stats4                  <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## stringi                1.8.2                           RSPM      1.8.2                           RSPM  [1]   indirect
    ## stringr                1.5.1                           RSPM      1.5.1                           RSPM  [1]     direct
    ## survival               3.5-7                           CRAN       <NA>                           <NA>  [2]       <NA>
    ## svglite                2.1.3                           CRAN      2.1.3                           CRAN  [1]   indirect
    ## sys                    3.4.2                           CRAN      3.4.2                           CRAN  [1]   indirect
    ## systemfonts            1.0.5                           RSPM      1.0.5                           RSPM  [1]     direct
    ## tables                0.9.17                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## targets                1.3.2                           RSPM      1.3.2                           RSPM  [1]     direct
    ## terra                 1.7-65                           CRAN     1.7-65                           CRAN  [1]     direct
    ## testthat               3.2.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## texreg                1.39.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## textshaping            0.3.7                           RSPM      0.3.7                           RSPM  [1]   indirect
    ## tibble                 3.2.1                           CRAN      3.2.1                           CRAN  [1]     direct
    ## tidyr                  1.3.0                           CRAN      1.3.0                           CRAN  [1]     direct
    ## tidyselect             1.2.0                           CRAN      1.2.0                           CRAN  [1]   indirect
    ## tidyterra              0.5.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## tidyverse              2.0.0                           CRAN      2.0.0                           CRAN  [1]     direct
    ## tigris                 2.0.4                           CRAN      2.0.4                           CRAN  [1]     direct
    ## timeDate            4032.109                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## timechange             0.2.0                           CRAN      0.2.0                           CRAN  [1]   indirect
    ## tinytex                 0.49                           RSPM       0.49                           RSPM  [1]     direct
    ## tools                   <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## triebeard              0.4.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## tseries              0.10-55                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## tsibble                1.1.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## tweenr                 2.0.2                           CRAN      2.0.2                           CRAN  [1]   indirect
    ## twriTemplates          0.3.0   https://txwri.r-universe.dev       <NA>                           <NA>  [1]       <NA>
    ## tzdb                   0.4.0                           CRAN      0.4.0                           CRAN  [1]   indirect
    ## ucminf                 1.2.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## units                  0.8-5                           CRAN      0.8-5                           CRAN  [1]     direct
    ## urca                   1.3-3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## urltools               1.7.3                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## utf8                   1.2.4                           RSPM      1.2.4                           RSPM  [1]   indirect
    ## utils                   <NA>                           <NA>       <NA>                           <NA>  [2]   indirect
    ## uuid                   1.1-1                           CRAN      1.1-1                           CRAN  [1]   indirect
    ## vctrs                  0.6.4                           RSPM      0.6.4                           RSPM  [1]   indirect
    ## vipor                  0.4.7                           CRAN      0.4.7                           CRAN  [1]   indirect
    ## viridis                0.6.4                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## viridisLite            0.4.2                           CRAN      0.4.2                           CRAN  [1]   indirect
    ## visNetwork             2.1.2                           CRAN      2.1.2                           CRAN  [1]     direct
    ## vroom                  1.6.4                           RSPM      1.6.4                           RSPM  [1]   indirect
    ## waiter                 0.2.5                           CRAN      0.2.5                           CRAN  [1]   indirect
    ## waldo                  0.5.2                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## webp                   1.2.0                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## webshot                0.5.5                           CRAN      0.5.5                           CRAN  [1]   indirect
    ## withr                  2.5.2                           RSPM      2.5.2                           RSPM  [1]   indirect
    ## wk                     0.9.1                           CRAN      0.9.1                           CRAN  [1]   indirect
    ## writexl                1.4.2                           RSPM       <NA>                           <NA>  [1]       <NA>
    ## xfun                    0.41                           RSPM       0.41                           RSPM  [1]     direct
    ## xml2                   1.3.5                           CRAN      1.3.5                           CRAN  [1]   indirect
    ## xtable                 1.8-4                           CRAN      1.8-4                           CRAN  [1]   indirect
    ## xts                   0.13.1                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## yaml                   2.3.7                           CRAN      2.3.7                           CRAN  [1]   indirect
    ## zip                    2.3.0                           CRAN      2.3.0                           CRAN  [1]   indirect
    ## zoo                   1.8-12                           CRAN       <NA>                           <NA>  [1]       <NA>
    ## 
    ## [1]: C:/Data-Analysis-Projects/bmp-synthesis-manuscript/renv/library/R-4.3/x86_64-w64-mingw32       
    ## [2]: C:/Users/michael.schramm/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/f8897b8d
    ## 
    ## # ABI ------------------------------------------------------------------------
    ## - ABI conflict checks are not yet implemented on Windows.
    ## 
    ## # User Profile ---------------------------------------------------------------
    ## [no user profile detected]
    ## 
    ## # Settings -------------------------------------------------------------------
    ## List of 13
    ##  $ bioconductor.version     : NULL
    ##  $ external.libraries       : chr(0) 
    ##  $ ignored.packages         : chr(0) 
    ##  $ package.dependency.fields: chr [1:3] "Imports" "Depends" "LinkingTo"
    ##  $ ppm.enabled              : NULL
    ##  $ ppm.ignored.urls         : chr(0) 
    ##  $ r.version                : NULL
    ##  $ snapshot.type            : chr "implicit"
    ##  $ use.cache                : logi TRUE
    ##  $ vcs.ignore.cellar        : logi TRUE
    ##  $ vcs.ignore.library       : logi TRUE
    ##  $ vcs.ignore.local         : logi TRUE
    ##  $ vcs.manage.ignores       : logi TRUE
    ## 
    ## # Options --------------------------------------------------------------------
    ## List of 8
    ##  $ defaultPackages                     : chr [1:6] "datasets" "utils" "grDevices" "graphics" ...
    ##  $ download.file.method                : NULL
    ##  $ download.file.extra                 : NULL
    ##  $ install.packages.compile.from.source: chr "interactive"
    ##  $ pkgType                             : chr "both"
    ##  $ repos                               : Named chr [1:7] "https://bioconductor.org/packages/3.18/bioc" "https://bioconductor.org/packages/3.18/data/annotation" "https://bioconductor.org/packages/3.18/data/experiment" "https://bioconductor.org/packages/3.18/workflows" ...
    ##   ..- attr(*, "names")= chr [1:7] "BioCsoft" "BioCann" "BioCexp" "BioCworkflows" ...
    ##  $ renv.consent                        : logi TRUE
    ##  $ renv.verbose                        : logi TRUE
    ## 
    ## # Environment Variables ------------------------------------------------------
    ## HOME                        = C:\Users\michael.schramm\Documents
    ## LANG                        = <NA>
    ## MAKE                        = <NA>
    ## R_LIBS                      = C:/Data-Analysis-Projects/bmp-synthesis-manuscript/renv/library/R-4.3/x86_64-w64-mingw32;C:/Users/michael.schramm/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/f8897b8d
    ## R_LIBS_SITE                 = C:/Users/michael.schramm/AppData/Local/Programs/R/R-4.3.1/site-library
    ## R_LIBS_USER                 = C:/Data-Analysis-Projects/bmp-synthesis-manuscript/renv/library/R-4.3/x86_64-w64-mingw32
    ## RENV_DEFAULT_R_ENVIRON      = <NA>
    ## RENV_DEFAULT_R_ENVIRON_USER = <NA>
    ## RENV_DEFAULT_R_LIBS         = <NA>
    ## RENV_DEFAULT_R_LIBS_SITE    = C:/Users/michael.schramm/AppData/Local/Programs/R/R-4.3.1/site-library
    ## RENV_DEFAULT_R_LIBS_USER    = C:\Users\michael.schramm\AppData\Local/R/win-library/4.3
    ## RENV_DEFAULT_R_PROFILE      = <NA>
    ## RENV_DEFAULT_R_PROFILE_USER = <NA>
    ## RENV_PROJECT                = C:/Data-Analysis-Projects/bmp-synthesis-manuscript
    ## 
    ## # PATH -----------------------------------------------------------------------
    ## - C:\rtools43/x86_64-w64-mingw32.static.posix/bin
    ## - C:\rtools43/usr/bin
    ## - C:\rtools43\x86_64-w64-mingw32.static.posix\bin
    ## - C:\rtools43\usr\bin
    ## - C:\Users\michael.schramm\AppData\Local\Programs\R\R-4.3.1\bin\x64
    ## - C:\Windows\system32
    ## - C:\Windows
    ## - C:\Windows\System32\Wbem
    ## - C:\Windows\System32\WindowsPowerShell\v1.0\
    ## - C:\Windows\System32\OpenSSH\
    ## - C:\Program Files\dotnet\
    ## - C:\Users\michael.schramm\AppData\Local\Programs\Quarto\bin
    ## - C:\Users\michael.schramm\AppData\Local\Microsoft\WindowsApps
    ## - C:\Users\michael.schramm\AppData\Local\Programs\Git\cmd
    ## - C:\MyPerl\c\bin
    ## - C:\MyPerl\perl\site\bin
    ## - C:\MyPerl\perl\bin
    ## - C:\Program Files\RStudio\resources\app\bin\quarto\bin\tools
    ## - C:\Users\michael.schramm\AppData\Local\Programs\Microsoft VS Code\bin
    ## - C:\Users\michael.schramm\AppData\Local\Programs\R\R-4.3.1
    ## - C:\Users\michael.schramm\AppData\Roaming\TinyTeX\bin\windows
    ## - C:\Users\michael.schramm\AppData\Local\Programs\Quarto\bin
    ## - C:\Program Files\RStudio\resources\app\bin\postback
    ## 
    ## # Cache ----------------------------------------------------------------------
    ## There are a total of 1001 packages installed in the renv cache.
    ## Cache path: "C:/Users/michael.schramm/AppData/Local/R/cache/R/renv/cache/v5/R-4.3/x86_64-w64-mingw32"
