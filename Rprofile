# some options
options(max.print=100)
options(sci.pen=10)
options(editor="vim")
options(repos=c(CRAN = "https://cran.rstudio.com/"))
options(download.file.method="libcurl")
options(menu.graphics=FALSE)
#options(prompt='R > ') # does changing the prompt mess up ess??


# package specific options
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# tab completion for library
utils::rc.settings(ipck=TRUE)

# auto set R to max columns based on terminal size
# make sure that $COLUMNS is available (exported in bash_profile)
.set_width <- function(col_width = Sys.getenv("COLUMNS")) options(width=as.integer(col_width))

# make sure that these packages are available

suppressPackageStartupMessages({
    library(stats)
    library(graphics)
    library(grDevices)
    library(datasets)
    library(utils)
    library(methods)
    library(base)
})

# pkgs = c('colorout', 'lookup', 'lintr', 'tidyverse', 'CB')
# jalvesaq/colorout # but this messes up cool progress messages in packages like `pak` (and ess doesn't use it)
# jimhester/lookup
pkgs = c('lookup', 'lintr')
for (pkg in pkgs) {
    if (suppressPackageStartupMessages(suppressWarnings(!require(pkg, character.only = TRUE, quietly = TRUE))))
        message(sprintf("Package '%s' not installed", pkg))
}
rm(pkgs, pkg)

# use colorout, if available
if ('colorout' %in% loadedNamespaces()){
# customize colors
# use colorout::show256Colors() to show all colors in browser window
        setOutputColors256(normal = 241, negnum = 247, zero = 226,
                number = 247, date = 179, string = 33,
                const = 252, false = 203, true = 78,
                infinite = 39, stderror = 88,
                warn = c(1, 0, 1), error = c(1, 15),
                verbose = FALSE, # set to TRUE to show results of setup
                zero.limit = NA)
}

if ('rlang' %in% loadedNamespaces()) options(error = rlang::entrace)

.First <- function(){
    if (Sys.getenv("RSTUDIO") == "") .set_width()
}
