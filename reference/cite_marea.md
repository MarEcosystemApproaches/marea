# Get citation information for marea

Returns the recommended citation for the marea package, either as plain
text or in BibTeX format.

## Usage

``` r
cite_marea(bibtex = FALSE)
```

## Arguments

- bibtex:

  Logical. If TRUE, returns BibTeX format. If FALSE (default), returns
  plain text.

## Value

Citation information for the marea package.

## Examples

``` r
# Get text citation
cite_marea()
#> To cite marea in publications use:
#> 
#>   Jamie C. Tam, Benoit Casault, Stephanie Clay et al. (). marea:
#>   Maritime Ecosystem Approach R Package. R package version 1.0.0.9000.
#>   https://doi.org/10.5281/zenodo.15706086
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {marea: Maritime Ecosystem Approach R Package},
#>     author = {Jamie C. Tam and Benoit Casault and Stephanie Clay and Adam Cook and Remi Daigle and Andrew M. Edwards and Jaimie Harbin and Brad Hubley and Keith David and Mike McMahon and Emily A. O'Grady},
#>     note = {R package version 1.0.0.9000},
#>     url = {https://github.com/MarEcosystemApproaches/marea},
#>     doi = {10.5281/zenodo.15706086},
#>   }
#> 
#> We appreciate citations as they help us secure funding for continued
#> development.

# Get BibTeX citation
cite_marea(bibtex = TRUE)
#> @Manual{,
#>   title = {marea: Maritime Ecosystem Approach R Package},
#>   author = {Jamie C. Tam and Benoit Casault and Stephanie Clay and Adam Cook and Remi Daigle and Andrew M. Edwards and Jaimie Harbin and Brad Hubley and Keith David and Mike McMahon and Emily A. O'Grady},
#>   note = {R package version 1.0.0.9000},
#>   url = {https://github.com/MarEcosystemApproaches/marea},
#>   doi = {10.5281/zenodo.15706086},
#> }
```
