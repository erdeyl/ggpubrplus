<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggpubrplus

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/erdeyl/ggpubrplus)
[![License: GPL
v2+](https://img.shields.io/badge/License-GPL%20v2+-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18131490.svg)](https://doi.org/10.5281/zenodo.18131490)

**A modernized fork of ggpubr with full compatibility for ggplot2
4.0.2+, dplyr 1.2.0+, tidyr 1.3.2+, and R 4.1.0+**

## About This Package

This package is a **fork of ggpubr** renamed to `ggpubrplus` to avoid
package name conflicts while the upstream pull request
([kassambara/ggpubr#657](https://github.com/kassambara/ggpubr/pull/657))
is pending review.

Once the PR is merged and the original `ggpubr` is updated on CRAN,
users can switch back to the original package.

### Why This Fork Exists

The original ggpubr package generates deprecation warnings with: -
ggplot2 4.0.2+ (deprecated `size` vs `linewidth`) - dplyr 1.2.0+
(deprecated `do()`, `mutate_if()`) - tidyr 1.3.2+ (deprecated
`gather()`)

This fork resolves deprecation warnings and targets the modern R
ecosystem (including ggplot2 4.0.2+), which is a breaking change for
users on older ggplot2 versions.

## Installation

``` r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("erdeyl/ggpubrplus")
```

## Key Fixes

### ggplot2 3.4.0+ `size` -\> `linewidth` Fixes

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

- `compare_means()` - Fixed error with ref.group and anova (Issue \#572)
- `ggviolin()` - Added missing adjust parameter (Issue \#552)
- `ggdensity()` - Added missing bw parameter (Issue \#490)
- `stat_cor()` - Fixed locale issue with `options(OutDec = ",")` (Issue
  \#512)

### New Features

#### Customizable P-Value Formatting (Issue \#334)

Added comprehensive p-value formatting system with predefined style
presets to match different journal and publication style requirements.

**New Functions:** - `format_p_value()` - Format p-values with
customizable options - `get_p_format_style()` - Retrieve a predefined
formatting style - `list_p_format_styles()` - List all available styles

**New Parameters in Statistical Functions:** All statistical functions
(`stat_compare_means()`, `compare_means()`, `geom_pwc()`,
`stat_anova_test()`, `stat_kruskal_test()`, `stat_friedman_test()`,
`stat_welch_anova_test()`, `stat_cor()`, `ggadjust_pvalue()`) now
support: - `p.format.style` - Use a predefined style (apa, nejm, lancet,
ama, graphpad, scientific) - `p.digits` - Number of decimal places -
`p.leading.zero` - Whether to include leading zero - `p.min.threshold` -
Minimum threshold for “\< threshold” notation

**Example:**

``` r
# Use APA style formatting
bxp + stat_compare_means(p.format.style = "apa")

# Use NEJM style
bxp + geom_pwc(method = "t_test", p.format.style = "nejm")
```

## Citation

Please cite both the original package and this fork:

**Original package:** \> Kassambara A (2023). ggpubr: ‘ggplot2’ Based
Publication Ready Plots. R package version 0.6.0.
<https://CRAN.R-project.org/package=ggpubr>

**This fork:** \> Erdey L (2026). Supplementary compatibility updates
for ggpubr (Kassambara, 2023): Modern R ecosystem support (ggplot2
≥4.0.2, dplyr ≥1.2.0, tidyr ≥1.3.2, R ≥4.1.0) — to be cited with the
original package. R package version 0.9.0.6.
<https://doi.org/10.5281/zenodo.18131490>

## Original Package

This is based on the [ggpubr](https://github.com/kassambara/ggpubr)
package by Dr. Alboukadel Kassambara.

For documentation of the original package, see:
<https://rpkgs.datanovia.com/ggpubr/>

## License

GPL (\>= 2)

## Credits

- **Author/Maintainer**: Laszlo Erdey (Faculty of Economics and
  Business, University of Debrecen, Hungary)
- **Original ggpubr package**: Dr. Alboukadel Kassambara
