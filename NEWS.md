# ggpubrplus 0.9.0.4

## Bug fixes

- Fix `stat_compare_means()` grouped edge cases where pairwise comparisons are not computable (e.g., subsets with fewer than two levels), preventing `tidyr::pivot_longer()` empty-selection failures.
- Return clean empty-layer outputs from `stat_compare_means()` when no subset is comparable.
- Restore `compare_means(..., method = "t.test")` pairwise defaults to match `pairwise.t.test(pool.sd = !paired)`.
- Add the correctly spelled `"wilcoxon"` method alias while keeping `"wiloxon"` for backward compatibility.
- Remove duplicated `keep_only_tbl_df_classes()` helper definition.

## Tests

- Add regression coverage for grouped `<2` level subsets in both `compare_means()` and `stat_compare_means()`.

## Maintenance

- Re-run examples and regenerate documentation/manual artifacts for this patch.

# ggpubrplus 0.9.0.3

## Maintenance

- Merge PR #14 style normalization and sequence-idiom safety cleanup.
- Regenerate and synchronize Rd manuals with current roxygen sources.
- Exclude generated `Rplots.pdf` from source tarballs via `.Rbuildignore` to keep CRAN checks clean.

# ggpubrplus 0.9.0.2

## Maintenance

- Refactor duplicated `size`/`linewidth` compatibility handling into a shared internal helper.
- Replace remaining `1:nrow()` / `1:length()` patterns touched by recent compatibility work with safer `seq_len()` / `seq_along()` usage.
- Regenerate README/pkgdown artifacts so rendered docs stay synchronized with current package metadata.

# ggpubrplus 0.9.0.1

## Maintenance

- Raise minimum ggplot2 requirement to >= 4.0.2.
- Synchronize DESCRIPTION, README, CITATION, and package docs for this patch release.
- No functional API changes.

# ggpubrplus 0.9.0

## Compatibility

- Require ggplot2 >= 4.0.0 (breaking change).
- ggviolin(): add ggplot2 4.x quantile parameters; map deprecated
  `draw_quantiles` to `quantiles` with verbose migration warnings.

## Tests

- Add ggviolin deprecation-path tests for quantile parameters.

# ggpubrplus 0.8.4.1

## Compatibility

- Replace deprecated tidyselect helper with `any_of()` for p-value columns.
- Tests: adjust `geom_exec()` expectations for ggplot2 4.0 layer parameter storage.
- Bump minimum versions for ggrepel, tidyr, dplyr, cowplot, rlang, and rstatix.

# ggpubrplus 0.8.4

## Compatibility

- Convert `size` to `linewidth` for `geom_bar()`/`geom_col()` in `geom_exec()` to
  preserve bar outline widths with ggplot2 4.0.0+.
- Tests: replace deprecated `aes_()` usage with tidy-eval equivalents.

# ggpubrplus 0.8.3

## Bug fixes

- **`.parse_font()` decimal font size parsing** (ggpubr#659): Fixed bug where
  decimal font sizes (e.g., `lab.font = c(2.4, "italic", "black")`) were not
  recognized, causing the decimal value to be interpreted as a color index
  instead. The regex pattern now correctly matches both integer and decimal
  (dot or comma) font sizes. This affects all functions using `lab.font`
  parameters including `ggpie()`, `ggdonutchart()`, and font parameters in
  `ggpar()`.

# ggpubrplus 0.8.2

## Documentation and release

- Refresh documentation for the 0.8.2 release.
- Clarify public `create_p_label()` usage and tidy formatting.

# ggpubrplus 0.8.1

## Bug fixes and consistency

- Route `p.format.signif` through `add_stat_label()` for plotmath-safe asterisks.
- Use adjusted p-values when raw p-values are missing for `p.format.signif`.
- Add `p.decimal.mark` support across p-value formatting helpers.
- Documentation cleanup: clarify `create_p_label()` public status and normalize roxygen spacing.

# ggpubrplus 0.8.0

## New Features

### Customizable P-Value Formatting (Addresses kassambara/ggpubr#334)

Added comprehensive p-value formatting system with predefined style presets to match
different journal and publication style requirements.

**New Functions:**
- `format_p_value()` - Format p-values with customizable decimal places, leading zero,
  and threshold notation
- `get_p_format_style()` - Retrieve a predefined formatting style by name
- `list_p_format_styles()` - List all available predefined styles

**New Parameters in Statistical Functions:**
The following functions now support p-value formatting parameters (`p.format.style`,
`p.digits`, `p.leading.zero`, `p.min.threshold`):
- `stat_compare_means()`
- `compare_means()`
- `geom_pwc()` / `stat_pwc()`
- `stat_anova_test()`
- `stat_kruskal_test()`
- `stat_friedman_test()`
- `stat_welch_anova_test()`
- `stat_cor()`
- `ggadjust_pvalue()`

**Available Styles:**

| Style | Digits | Leading Zero | Threshold | Description |
|-------|--------|--------------|-----------|-------------|
| default | 2 | Yes | None | Current behavior (backward compatible) |
| apa | 3 | No | < .001 | APA Style |
| nejm | 3 | Yes | < 0.001 | New England Journal of Medicine |
| lancet | 4 | Yes | < 0.0001 | The Lancet |
| ama | 3 | No | < .001 | American Medical Association |
| graphpad | 4 | Yes | < 0.0001 | GraphPad Prism |
| scientific | 2 | Yes | None | Scientific notation for GWAS |

**Example Usage:**
```r
# Use APA style formatting
bxp + stat_compare_means(p.format.style = "apa")

# Use NEJM style
bxp + geom_pwc(method = "t_test", p.format.style = "nejm")

# Custom formatting: 4 digits, no leading zero
compare_means(len ~ dose, ToothGrowth, p.digits = 4, p.leading.zero = FALSE)

# List all available styles
list_p_format_styles()
```

## Package Rename

This package is a modernized fork of `ggpubr` renamed to `ggpubrplus` to avoid
conflicts while the upstream pull request (kassambara/ggpubr#657) is pending.
Once the PR is merged and the original `ggpubr` is updated on CRAN, users can
switch back to the original package.

## Issues Addressed

This version addresses the following **10 issues** from the original ggpubr repository:

| Issue | Description |
|-------|-------------|
| #656 | border() size deprecation warning |
| #654 | border() size deprecation warning |
| #645 | Deprecation in ggplot2 3.4.0 |
| #644 | border() size deprecation warning |
| #572 | compare_means() error with ref.group and anova |
| #552 | ggviolin missing adjust parameter for bandwidth |
| #536 | tidyr 1.3.0 release notice |
| #533 | Installation failure due to tidyr version conflict |
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
