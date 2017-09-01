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
    unixtools::set.tempdir('~/myRinstalls/')
    Sys.setenv(TMP='~/myRinstalls/')
}

# auto set R to max columns based on terminal size
# make sure that $COLUMNS is available (exported in bash_profile)
if (Sys.getenv("RSTUDIO") == "") {
    .wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
      options(width=as.integer(howWide))
    }
    # .wideScreen()
}

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

pkgs = c('remotes', 'colorout', 'tidyverse', 'lookup', 'CB')
for (pkg in pkgs) {
    if (suppressPackageStartupMessages(!require(pkg, character.only = TRUE, quietly = TRUE)))
        message(sprintf("Package '%s' not installed", pkg))
}

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

# print time and wd on startup if interactive
.First <- function(){
  if(interactive()){
    library(utils)
    timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))
    .wideScreen()
  }
}
