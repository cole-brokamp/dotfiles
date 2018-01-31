# some options
options(max.print=100)
options(sci.pen=10)
options(editor="vim")
options(repos=c(CRAN = "https://cran.rstudio.com/"))
options(download.file.method="libcurl")
options(menu.graphics=FALSE)
options(prompt='R > ')

# tab completion for library
utils::rc.settings(ipck=TRUE)

# if on CCHMC HPC, use custom tempdir for installing packages
if (Sys.info()['user'] == 'broeg1') { # better way to determine this?
    # install unixtools with `mkdir rtmp; TMPDIR=$PWD/rtmp R -e "install.packages('unixtools',repos='http://www.rforge.net/')"; rm -rf rtmp` from the shell
    unixtools::set.tempdir('~/myRinstalls/')
    Sys.setenv(TMP='~/myRinstalls/')
}

# auto set R to max columns based on terminal size
# make sure that $COLUMNS is available (exported in bash_profile)
# if (Sys.getenv("RSTUDIO") == "") {
#     .wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
#       options(width=as.integer(howWide))
#     }
# }

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

pkgs = c('remotes', 'colorout', 'tidyverse', 'lookup', 'CB', 'lintr')
for (pkg in pkgs) {
    if (suppressPackageStartupMessages(suppressWarnings(!require(pkg, character.only = TRUE, quietly = TRUE))))
        message(sprintf("Package '%s' not installed", pkg))
}
rm(pkgs, pkg)


# install these packages from github:
# jalvesaq/colorout
# jimhester/lookup
# cole-brokamp/CB

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

## rice options
options(
    # color scheme see [here](https://help.farbox.com/pygments.html) for a list of supported color schemes, default is `"native"`
    rice.color_scheme = "native",
    # either  `"emacs"` (default) or `"vi"`.
    rice.editing_mode = "vi",
    # auto match brackets and quotes
    rice.auto_match = FALSE,
    # auto indentation for new line and curly braces
    rice.auto_indentation = TRUE,
    rice.tab_size = 4,
    # pop up completion while typing
    rice.complete_while_typing = TRUE,
    # automatically adjust R buffer size based on terminal width
    rice.auto_width = TRUE,
    # insert new line between prompts
    rice.insert_new_line = FALSE,
    # when using history search (ctrl-r/ctrl-s in emacs mode), do not show duplicate results
    rice.history_search_no_duplicates = FALSE,
    # custom prompt for different modes
    # rice.prompt = "\033[0;34mr$>\033[0m ",
    rice.shell_prompt = "\033[0;31m#!>\033[0m ",
    rice.browse_prompt = "\033[0;33mBrowse[{}]>\033[0m ",
    # supress the loading message for reticulate
    rice.suppress_reticulate_message = TRUE
)

# print time and wd on startup
.First <- function(){
    library(utils)
    timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))
    # if (Sys.getenv("RSTUDIO") == "") .wideScreen()
}
