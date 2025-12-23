# ggpubr-modern

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/erdeyl/ggpubr-modern)
[![License: GPL v2+](https://img.shields.io/badge/License-GPL%20v2+-blue.svg)](https://www.gnu.org/licenses/gpl-2.0)

**A modernized fork of ggpubr with full compatibility for ggplot2 3.4.0+ and tidyr 1.3.0+**

## About This Fork

This repository is a **temporary distribution** of the ggpubr package with critical compatibility fixes for modern R environments. It is intended to be used until the upstream pull request ([kassambara/ggpubr#657](https://github.com/kassambara/ggpubr/pull/657)) is merged.

### Why This Fork Exists

The original ggpubr package generates deprecation warnings with:
- ggplot2 3.4.0+ (released 2022)
- tidyr 1.3.0+

This fork resolves all deprecation warnings while maintaining full backward compatibility.

## Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("erdeyl/ggpubr-modern")
```

## Key Fixes in This Fork

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

### tidyr Compatibility Fixes
- `ggballoonplot()` - Replaced `tidyr::gather()` with `tidyr::pivot_longer()`
- `compare_means()` - Replaced `tidyr::gather()` with `tidyr::pivot_longer()`

### Other Fixes
- `ggballoonplot()` - Updated example to use `guides(size = "none")`
- `stat_cor()` - Fixed locale issue with `options(OutDec = ",")`

## Original Package

This package is a derivative work of **ggpubr** by Alboukadel Kassambara.

- **Original Repository:** https://github.com/kassambara/ggpubr
- **CRAN:** https://CRAN.R-project.org/package=ggpubr
- **Documentation:** https://rpkgs.datanovia.com/ggpubr/

## License

This package is distributed under the **GNU General Public License v2 or later (GPL-2+)**, the same license as the original ggpubr package.

## Citation

When using this package, please cite **both** the original author and this fork:

### Primary Citation (Original Author)

```
Kassambara A (2023). ggpubr: 'ggplot2' Based Publication Ready Plots.
R package version 0.6.0. https://CRAN.R-project.org/package=ggpubr
```

### BibTeX (Original)

```bibtex
@Manual{ggpubr,
  title = {ggpubr: 'ggplot2' Based Publication Ready Plots},
  author = {Alboukadel Kassambara},
  year = {2023},
  note = {R package version 0.6.0},
  url = {https://CRAN.R-project.org/package=ggpubr},
}
```

### Secondary Citation (This Fork)

If you specifically used this modernized fork, please also acknowledge:

```
Erdey L (2025). ggpubr-modern: Modernized fork of ggpubr with
ggplot2 3.4.0+ and tidyr compatibility fixes.
R package version 0.6.3. https://github.com/erdeyl/ggpubr-modern
```

### BibTeX (This Fork)

```bibtex
@Manual{ggpubr-modern,
  title = {ggpubr-modern: Modernized Fork of ggpubr},
  author = {Laszlo Erdey},
  year = {2025},
  note = {R package version 0.6.3. Based on ggpubr by Kassambara.},
  url = {https://github.com/erdeyl/ggpubr-modern},
}
```

## Acknowledgments

- **Alboukadel Kassambara** - Original author and maintainer of ggpubr
- **Claude Code** - AI assistance in developing compatibility fixes

## Upstream Pull Request

The fixes in this fork have been submitted as a pull request to the original repository:
- **PR #657:** https://github.com/kassambara/ggpubr/pull/657

Once the PR is merged and released on CRAN, users should switch back to the official package:

```r
install.packages("ggpubr")
```

## Contact

- **Fork Maintainer:** László Erdey (erdey.laszlo@econ.unideb.hu)
- **Affiliation:** Faculty of Economics and Business, University of Debrecen, Hungary
- **ORCID:** [0000-0002-6781-4303](https://orcid.org/0000-0002-6781-4303)
