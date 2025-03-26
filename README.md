
<!--
README.md is generated from README.Rmd. Please edit that file. Build with
-->
<!--
which builds the .html that can be viewed locally (but isn't pushed to GitHub;
GitHub uses README.md to make the page you see on GitHub).

Figures - to update a figure, manually run the piece of code that makes
the .png file. See notes below (and pacea Issue #44). Make the png resolutions the same in
each direction, at least for multi-panel plots, as seemed a bit blurry when I
hadn't (primary production and hotssea at least).
-->
<!-- # pacea <img src="man/figures/logo.png" align="right" height="138" /> -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/MarEcosystemApproaches/marea/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MarEcosystemApproaches/marea/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/MarEcosystemApproaches/marea/graph/badge.svg?token=93afkFJUVL)](https://codecov.io/gh/MarEcosystemApproaches/marea)
<!-- [![DOI](https://zenodo.org/badge/417554147.svg)](https://zenodo.org/doi/10.5281/zenodo.13840804) -->
<!-- badges: end -->

An R package of Maritime ecosystem information to help facilitate an
ecosystem approach to fisheries management.

*We wrangle the data so you don’t have to*

marea stands for MARitimes Ecosystem Approach.

marea is an R package containing a variety of data sets and model
output. We wrangle the data sets behind the scenes to get them into
usable formats in R, and provide helpful plotting functions. All data
sets include documentation regarding the original sources and authors,
who should be cited and consulted as appropriate.

Please continue reading through this README file, and especially see the
vignettes section get started.

## What is in marea?

Currently, marea contains:

**see pacea for examples**

Plotting and helper functions are provided, and demonstrated in the
detailed vignettes. All data and model outputs are fully documented and
referenced.

## Brief examples of some questions that can be quickly visualised (see vignettes for full details)

<!--
**How does this year's sea surface temperature (red curve) in West Dixon Entrance compare to previous
years, based on the buoy there?**
<!-- Run this png code manually (as not evaluated here) to update the figure -->

``` r
plot(buoy_sst)    # Buoy C46205 is plotted by default
```

<!-- ![](man/figures/README-north-hecate-temp.png){width=80%} -->

**Are we experiencing a phase of El Niño, based on the Oceanic Niño
Index? (If the last bars are red and above 0.5, then ‘yes’)**

``` r
plot(oni)
```

<img src="man/figures/README-oni.png" style="width:80.0%" />

**see pacea for more examples**

**What climatic and oceanographic indices are currently available, and
over what years?**

``` r
# knitr::kable(marea_indices)
```

## Vignettes

For more detailed explanations of the data sets and functionality see
the vignettes, which are already rendered here (note that we do not
automatically update them when the data in marea are updated, so if you
run them locally you may get more up-to-date values):

TODO these are from pacea for now, and link to that GitHub site:

-   [indices.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/indices.html)
    Climatic and oceanographic indices and associated plotting
    functions.
-   [buoys.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/buoys.html)
    Calculated daily mean sea surface temperatures and associated
    plotting functions.
-   [populations.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/populations.html)
    Estimates of animal populations and associated plotting functions.
-   [oisst.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/oisst.html)
    OISST data and calculations and associated plotting functions.
-   [bccm.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/bccm.html)
    BCCM model results restricted to Canada’s Exclusive Economic Zone,
    calculations, and associated plotting functions.
-   [bccm_full.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/bccm_full.html)
    BCCM model results over the full model domain, calculations, and
    associated plotting functions.
-   [hotssea.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/hotssea.html)
    HOTSSea model results and associated plotting functions.
-   [zooplankton.html](http://htmlpreview.github.io/?https://github.com/pbs-assess/pacea/blob/main/vignettes/zooplankton.html)
    Zooplankton anomalies for the Strait of Georgia, calculations, and
    associated plotting functions.

The presentations where we introduced `pacea` are available as .pdf.
[Here is the DFO version (10th November
2023)](talks/pacea-release-talk-dfo.pdf) and [here is the UVic Ecostats
version (21st November 2023)](talks/pacea-release-talk-uvic.pdf). The
code to build them is [here as .Rmd](talks/pacea-release-talk.Rmd).

## Installation

Okay, if you’re convinced this package might be useful for you, then to
install the latest version just:

    install.packages("remotes")    # If you do not already have the "remotes" package

    remotes::install_github("MarEcosystemApproaches/marea")

If you get an error like

    Error in utils::download.file(....)

then the connection may be timing out (happens to us on the DFO
network). Try

    options(timeout = 1200)

and then try and install again. If you get a different error then post
an Issue or contact ???TODO
<a href="mailto:andrew.edwards@dfo-mpo.gc.ca">Andy</a>.

## Updates

We plan to continually enhance `marea` by updating current data sets and
adding new ones, as well as adding functionality. To explain these, we
will document them by date in the [NEWS](NEWS.md).

TODO We plan to update the indices, buoy temperatures, and OISST values
on the 20th of every month (or soon after), which should capture updates
to those data.

To check what date you installed `marea` locally on your machine, we
have the helper command:

``` r
marea_installed()

TODO You last locally installed marea on 2023-11-01 which is 8 days ago.

Compare that with the updates in the NEWS file (and you can also check dates of the latest commits at
https://github.com/MarEcosystemApproaches/marea/commits/main)
```

The commits tell you when we last changed something, while the
[NEWS](NEWS.md) will give a more concise summary of changes by date.
Then you may wish to update `marea` by running
`remotes::install_github("MarEcosystemApproaches/marea")` again.

## Audience and motivation

The primary audience is stock assessment scientists who want to analyse
environmental variables in the context of their stock assessment (but we
anticipate other interested users). The Fisheries Act requires
management of fisheries to take into account “[the biology of the fish
and the environmental conditions affecting the
stock](https://laws-lois.justice.gc.ca/eng/acts/f-14/page-3.html#h-1175547)”.
Such an Ecosystem Approach to Fisheries Management requires data.

A comprehensive [2022 analysis of Canadian stock
assessments](https://publications.gc.ca/collections/collection_2022/mpo-dfo/Fs97-6-3473-eng.pdf)
found that availability of data on environmental variables was the
leading cause of not integrating such information into assessments.
marea aims to help make data availability more streamlined, and avoid
each individual assessment scientist having to figure out themselves
where to get appropriate data.

This work is strongly motivated by, and based on, the
[pacea](https://github.com/pbs-assess/pacea) R package by Andrew Edwards
and colleagues for Pacific ecosystem information, which in turn was
inspired by the [GSLea](https://github.com/duplisea/gslea) R package by
Dan Duplisea and colleagues for the Gulf of St Lawrence.

## Citation

If you use `marea` in your work then please cite it as (NOTE this has
been updated since the original release):

TODO

<!-- Edwards A.M., Tai T.C., Watson J., Peña M.A., Hilborn A., Hannah C.G., Rooper
  C.N., Flynn K.L., and Oldford, G.L. (2024). pacea: An R package of Pacific ecosystem information to
  help facilitate an ecosystem approach to fisheries management.
  <https://github.com/pbs-assess/pacea>, <https://zenodo.org/doi/10.5281/zenodo.13840804>
-->

TODO: You may wish to add the date you installed it (using
`marea_installed()`). Use `citation("marea")` to get a version for LaTeX
and R Markdown bibliographies. Although the DOI badge at the top of this
page ends in …805, the …804 DOI is preferable because that will not
change with any future updates to the GitHub tag. So …804 seems simpler
and more consistent to use (thanks!).

Also let us know of any applications. This will help us devote effort
into maintaining `marea`.
<!-- For pacea this hasn't really happened unless we happen
to talk to people! -->

## Acknowledgments

TODO Funding?

We thank the following for contributing ideas, example code for ROMS
output, model output, and/or helping with ideas and coding issues:

TODO:

-   Jessica Nephin, Lindsay Davidson, Strahan Tucker, Brianna Wright,
    Patrick Thompson, Matt Grinnell, Sean Anderson, Philina English,
    Chris Grandin, Jennifer Boldt, Kelly Young, and others.

## Notes for developers

1.  Please work on your own development branch to ensure that the main
    branch is never broken (so users can always download the package).
    See below for details on branching.

2.  Edit and render the README.Rmd file, not the README.Md file. If you
    add any new figures then commit and push them (they will be in
    man/figures/README-<chunk-name>) so they show up on the GitHub
    README. Always render the .Rmd so that it and the rendered .Md stay
    in sync (see
    <https://github.com/pbs-assess/pacea/issues/44#issuecomment-1793235838>
    for how to have Git check that README.Md is newer than README.Rmd).

3.  Every data set is built using code in the `raw-data/` directory,
    with documentation in `R/data.R`. Document everything!

4.  If you add new functions please add tests as well so we retain good
    code coverage (as indicated in the badge at the top of the README on
    GitHub).

5.  Use the standard `lower_case_and_underscores` for naming variables
    and functions.

6.  Citation information is in inst/CITATION, so if you update
    DESCRIPTION file (e.g. add an author) then update that also and
    update the citation info above.

7.  TODO for marea: Logo included by running
    `use_logo("inst/pacea-logo-from-carley.png")`. The .png was expertly
    designed by Carley Colclough. There is also
    `inst/pacea-logo-for-printing.jpg` for printing purposes.

### Branching in Git

To ensure the main branch is never broken (and so users can always
download the package), when editing package code please work on your own
branch and then merge it in. If just updating the README then we can
stick with the main branch (because messing something up should not void
the package being installable).

Your own branch should be called `dev-yourname`, e.g. `dev-jamie`. If
you don’t yet have a branch you need to:

-   create it locally (and ‘check it out’, i.e. move into it):
    `git checkout -b dev-yourname`
-   tell Git where to push to:
    `git push --set-upstream origin dev-yourname` (Git will tell you
    this command if you do a `git push` without having run it)
-   update GitHub Actions so that pushes of your branch get properly
    tested on GitHub. Just add your branch to **both** lists of branches
    at the top of:
    -   `.github/workflows/R-CMD-check.yaml`
    -   `.github/workflows/test-coverage.yaml`
-   Do that as your first commit and push to check everything’s working.

Once you have your branch set up then the workflow is:

1.  Make sure you are all caught up on the main branch:

-   `git checkout main`
-   `git fetch`
-   `git rebase`

2.  Switch to your development branch and merge the main branch into it

-   `git checkout dev-yourname`
-   `git merge main`
-   for magit users (probably just Andy), this is just `b b` and `m m`
    (it recommends arguments)

3.  Do some new commits in your branch and push them in the usual way.
    GitHub Actions will work on your branch.

So from a quick test, the `R-CMD-check` badge only refers to the main
branch, which is great as it should never show as failing because we
should only be breaking things on own branch. Always having the green
passing badge should inspire consumer confidence. On GitHub you can
select your branch and see the tickmark, which will tell you if the
build has failed (or if your changes have caused the code coverage to
decline), and you’ll probably get an automatic email also, just to drive
home the message. So the badges on the README on your branch still
relate to the main branch.

You can click on the codecov badge on GitHub, and then on the codecov
page you can select your branch. There are ways to check code coverage
locally, but Andy found these all take a long time for pacea (and it’s
easier to push and let GitHub check the coverage). See pacea commit
6c872da for the commands Andy tried (now deleting to keep this README
file cleaner).

4.  When you are happy with your changes and ready to merge them into
    the main branch, you need to

-   add any changes that users should be aware of to the [NEWS](NEWS.Md)
    file.

No need to mention things that are only of interest to developers (like
the commiting of these instructions). We want the NEWS to be succinct
and quickly readable so that people look at it.

Then you need to double check that the main branch is up-to-date with
the version on GitHub (someone may have pushed some changes since you
last fetched), then merge any changes back into your branch. You could
just check on GitHub (if you’re the last person to have pushed to main
then you are probably fine), or do these:

-   `git checkout main`

-   `git fetch`

-   `git rebase`

-   `git checkout dev-yourname`

-   `git merge main`

If that does merge in some new commits then you should

-   `git push` to run the GitHub Actions checks again on GitHub. If the
    merge says something like you are all caught up, no merging is
    needed (i.e. no-one committed anything to the main branch
    recenetly), then no need for the `git push`, since this entire step
    will not have added any new code.

5.  Now you are ready to merge your changes into the main branch, so
    that people will get them when they install/update `pacea`:

-   `git checkout main`
-   `git merge dev-yourname`
-   `git push`

And you are done! If you want, we have some aliases (such as `git co`
for `git checkout` and `git p` for `git push` listed in this example
.gitconfig file:
<https://raw.githubusercontent.com/quantitative-biology/module-1-git/main/misc/.gitconfig>
from our e-book
<https://www.quantitative-biology.ca/git-and-github.html#save-our-template-.gitconfig-file>
You can put those into your `.gitconfig` file if you like.

This can help understanding on branches:
<https://www.atlassian.com/git/tutorials/using-branches/git-merge>
