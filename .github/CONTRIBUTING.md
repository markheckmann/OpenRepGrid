# Contributing to OpenRepGrid

> Everybody can contribute! No need to be an R programmer to do so.

### Make suggestions

To make `OpenRepGrid` suit the communitie's needs, we need to know what features you need, what you miss and what you feel needs improvement. You can suggest improvements and new features [here](https://github.com/markheckmann/OpenRepGrid/issues) or send us an <a href="mailto:heckmann.mark@gmail.com">email</a>.

### Report bugs

If you discover bugs in the software (incorrect results, crashes etc.), please let us know. You may file bug reports as a  [github issue](https://github.com/markheckmann/OpenRepGrid/issues) or send us an <a href="mailto:heckmann.mark@gmail.com">email</a>.

### Write documentation

If you feel the documentation needs improvement or you have a use case or tutorial you would like to see in the documentation, just get in touch via <a href="mailto:heckmann.mark@gmail.com">email</a>. Also, if you already have a draft you would like to add, send it to us.

### Supply code snippets

If have some basic R knowledge and come up with some code snippets just send them to us and we will take care to integrate your ideas into the package.

### Join the dev team

If you have solid R knowledge, just hammer out some code and send it to us. Or, if you are familiar with `git`, fork the `OpenRepGrid` repo, make some changes and submit a pull request.

#### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("markheckmann/OpenRepGrid", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

#### Code style

*   You should run [styler](https://CRAN.R-project.org/package=styler) to make sure your R code is formatted correctly. But please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  
