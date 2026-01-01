# ggpubrplus 0.7.0

## Package Rename

This package is a modernized fork of `ggpubr` renamed to `ggpubrplus` to avoid
conflicts while the upstream pull request (kassambara/ggpubr#657) is pending.
Once the PR is merged and the original `ggpubr` is updated on CRAN, users can
switch back to the original package.

## Issues Addressed

This version addresses the following **9 issues** from the original ggpubr repository:

| Issue | Description |
|-------|-------------|
| #656 | border() size deprecation warning |
| #654 | border() size deprecation warning |
| #645 | Deprecation in ggplot2 3.4.0 |
| #644 | border() size deprecation warning |
| #572 | compare_means() error with ref.group and anova |
| #552 | ggviolin missing adjust parameter for bandwidth |
| #536 | tidyr 1.3.0 release notice |
| #512 | stat_cor() OutDec locale issue |
| #490 | ggdensity missing bw parameter for bandwidth |

## Bug fixes

### Package Dependency Updates

- **Raised minimum R version** to R >= 4.1.0 (from R >= 3.1.0) to match ggplot2
  >= 3.5.2 requirement.

- **Raised minimum dplyr version** to dplyr >= 1.1.0 (from dplyr >= 0.7.1) to
  use modern `reframe()`, `slice_head()`, `slice_tail()`, `across()`, and
  `where()` functions.

- **Raised minimum rlang version** to rlang >= 1.0.0 (from rlang >= 0.4.6) to
  align with dplyr/tidyr requirements.

### dplyr Compatibility

- **`ggsummarytable()` mutate_if() deprecation**: Replaced deprecated
  `dplyr::mutate_if()` with `dplyr::mutate(across(where(...)))` syntax.

- **`compare_means()` do() deprecation**: Replaced deprecated `dplyr::do()`
  with modern `dplyr::reframe()` for p-value adjustment calculations.

- **`desc_statby()` do() deprecation**: Replaced deprecated `dplyr::do()`
  with `dplyr::reframe()` for computing summary statistics by groups.

- **`.top_up()` and `.top_down()` do() deprecation**: Replaced deprecated
  `dplyr::do(utils::tail())` and `dplyr::do(utils::head())` with modern
  `dplyr::slice_tail()` and `dplyr::slice_head()` functions.

- **Added explicit `.groups = "drop"`** to `summarise()` calls to prevent
  grouping warnings.

### ggplot2 3.4.0+ Compatibility (Issues #644, #645, #654, #656)

- **`border()` size deprecation** (Issues #644, #654, #656): Fixed deprecation
  warning in `border()` function by replacing deprecated `size` parameter with
  `linewidth` in `element_rect()` as required by ggplot2 3.4.0+.

- **`stat_cor()` deprecated ..var.. notation**: Updated `default_aes` to use
  modern `after_stat()` syntax instead of deprecated `..hjust..` and `..vjust..`
  notation.

- **`stat_compare_means()` deprecated ..var.. notation**: Updated `default_aes`
  to use `after_stat(hjust)` and `after_stat(vjust)` instead of deprecated
  `..hjust..` and `..vjust..` notation.

- **`stat_regline_equation()` deprecated ..var.. notation**: Updated
  `default_aes` to use `after_stat(eq.label)`, `after_stat(hjust)`, and
  `after_stat(vjust)` instead of deprecated notation.

- **`ggscatter()` geom_rug size deprecation**: Fixed deprecation warning in
  `ggscatter()` when `rug = TRUE` by using `linewidth` instead of `size` for
  `geom_rug()`.

- **`ggpaired()` geom_line size deprecation**: Fixed deprecation warning in
  `ggpaired()` by using `linewidth` instead of `size` for connecting lines.

- **`ggecdf()` stat_ecdf size deprecation**: Fixed deprecation warning in
  `ggecdf()` by using `linewidth` instead of `size` for the ECDF line.

- **`ggdensity()` geom_density size deprecation**: Fixed deprecation warning
  in `ggdensity()` by using `linewidth` instead of `size` for density lines.

- **`ggballoonplot()` guides() FALSE deprecation**: Updated to use
  `guides(size = "none")` instead of deprecated `guides(size = FALSE)`.

### tidyr Compatibility (Issue #536)

- **`ggballoonplot()` tidyr::gather() deprecation**: Replaced deprecated
  `tidyr::gather()` with `tidyr::pivot_longer()`.

- **`compare_means()` tidyr::gather() deprecation**: Replaced deprecated
  `tidyr::gather()` with `tidyr::pivot_longer()`.

### Locale Compatibility

- **`stat_cor()` OutDec locale issue** (Issue #512): Fixed parsing error when
  `options(OutDec = ",")` is set by explicitly using `decimal.mark = "."`.

### Kernel Density Parameter Fixes

- **`ggviolin()` missing adjust parameter** (Issue #552): Added `adjust` parameter.

- **`ggdensity()` missing bw parameter** (Issue #490): Added `bw` and `adjust`
  parameters to the allowed options.

### compare_means() Fixes

- **`compare_means()` error with ref.group and anova** (Issue #572): Fixed error
  when using `compare_means()` with `ref.group` and `method = "anova"`.
