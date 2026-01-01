# ggpubrplus

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/erdeyl/ggpubr-modern)
[![License: GPL v2+](https://img.shields.io/badge/License-GPL%20v2+-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)

**A modernized fork of ggpubr with full compatibility for ggplot2 3.5.2+, dplyr 1.1.0+, tidyr 1.3.0+, and R 4.1.0+**

## About This Package

This package is a **fork of ggpubr** renamed to `ggpubrplus` to avoid package name conflicts while the upstream pull request ([kassambara/ggpubr#657](https://github.com/kassambara/ggpubr/pull/657)) is pending review.

Once the PR is merged and the original `ggpubr` is updated on CRAN, users can switch back to the original package.

### Why This Fork Exists

The original ggpubr package generates deprecation warnings with:
- ggplot2 3.4.0+ (released 2022)
- dplyr 1.1.0+ (deprecated `do()`, `mutate_if()`)
- tidyr 1.3.0+ (deprecated `gather()`)

This fork resolves all deprecation warnings while maintaining full backward compatibility.

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("erdeyl/ggpubrplus")
```

## Key Fixes

### ggplot2 3.4.0+ `size` → `linewidth` Fixes
- `border()` - element_rect() size deprecation
- `ggscatter()` - geom_rug() and stat_stars() size deprecation
- `ggpaired()` - geom_line() size deprecation
- `ggecdf()` - stat_ecdf() size deprecation
- `ggdensity()` - geom_density() size deprecation

### Deprecated `..var..` Notation Fixes
- `stat_cor()` - Updated to use `after_stat()`
- `stat_compare_means()` - Updated to use `after_stat()`
- `stat_regline_equation()` - Updated to use `after_stat()`

### dplyr Compatibility Fixes
- `compare_means()` - Replaced `do()` with `reframe()`
- `desc_statby()` - Replaced `do()` with `reframe()`
- `ggsummarytable()` - Replaced `mutate_if()` with `across(where())`

### tidyr Compatibility Fixes
- `ggballoonplot()` - Replaced `gather()` with `pivot_longer()`
- `compare_means()` - Replaced `gather()` with `pivot_longer()`

### Bug Fixes
- `compare_means()` - Fixed error with ref.group and anova (Issue #572)
- `ggviolin()` - Added missing adjust parameter (Issue #552)
- `ggdensity()` - Added missing bw parameter (Issue #490)
- `stat_cor()` - Fixed locale issue with `options(OutDec = ",")` (Issue #512)

## Original Package

This is based on the [ggpubr](https://github.com/kassambara/ggpubr) package by Dr. Alboukadel Kassambara.

For documentation of the original package, see: https://rpkgs.datanovia.com/ggpubr/

## License

GPL (>= 2)

## Credits

- **Original Author**: Dr. Alboukadel Kassambara
- **Maintainer**: Laszlo Erdey (Faculty of Economics and Business, University of Debrecen, Hungary)
