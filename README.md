
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
sessioninfo::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.3.1 (2023-06-16 ucrt)
    ##  os       Windows 11 x64 (build 22631)
    ##  system   x86_64, mingw32
    ##  ui       RTerm
    ##  language (EN)
    ##  collate  English_United States.utf8
    ##  ctype    English_United States.utf8
    ##  tz       America/New_York
    ##  date     2024-03-07
    ##  pandoc   3.1.1 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  ! package     * version date (UTC) lib source
    ##  P BiocManager   1.30.22 2023-08-08 [?] CRAN (R 4.3.1)
    ##  P cli           3.6.1   2023-03-23 [?] CRAN (R 4.3.1)
    ##  P digest        0.6.33  2023-07-07 [?] CRAN (R 4.3.1)
    ##  P evaluate      0.23    2023-11-01 [?] RSPM (R 4.3.0)
    ##  P fastmap       1.1.1   2023-02-24 [?] CRAN (R 4.3.1)
    ##  P htmltools     0.5.7   2023-11-03 [?] RSPM (R 4.3.0)
    ##  P knitr         1.45    2023-10-30 [?] RSPM (R 4.3.0)
    ##    renv          1.0.3   2023-09-19 [1] CRAN (R 4.3.1)
    ##  P rlang         1.1.2   2023-11-04 [?] RSPM (R 4.3.0)
    ##  P rmarkdown     2.25    2023-09-18 [?] RSPM (R 4.3.0)
    ##  P rstudioapi    0.15.0  2023-07-07 [?] CRAN (R 4.3.1)
    ##  P sessioninfo   1.2.2   2021-12-06 [?] CRAN (R 4.3.1)
    ##  P xfun          0.41    2023-11-01 [?] RSPM (R 4.3.0)
    ##  P yaml          2.3.7   2023-01-23 [?] CRAN (R 4.3.0)
    ## 
    ##  [1] C:/Data-Analysis-Projects/bmp-synthesis-manuscript/renv/library/R-4.3/x86_64-w64-mingw32
    ##  [2] C:/Users/michael.schramm/AppData/Local/R/cache/R/renv/sandbox/R-4.3/x86_64-w64-mingw32/f8897b8d
    ## 
    ##  P ── Loaded and on-disk path mismatch.
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────
