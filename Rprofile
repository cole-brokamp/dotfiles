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

# set system variables
# if on CCHMC HPC, set proxy variables
if (Sys.info()['user'] == 'broeg1') { # better way to determine this?
    Sys.setenv(http_proxy='http://srv-sysproxy:ieQu3nei@bmiproxyp.chmcres.cchmc.org:80')
    Sys.setenv(https_proxy='https://srv-sysproxy:ieQu3nei@bmiproxyp.chmcres.cchmc.org:80')
    # Sys.setenv(http_proxy='http://bmiproxyp.chmcres.cchmc.org:80')
    # Sys.setenv(https_proxy='https://bmiproxyp.chmcres.cchmc.org:80')
    # install unixtools with `mkdir rtmp; TMPDIR=$PWD/rtmp R -e "install.packages('unixtools',repos='http://www.rforge.net/')"; rm -rf rtmp` from the shell
    unixtools::set.tempdir('~/myRinstalls/')
    Sys.setenv(TMP='~/myRinstalls/')
}

# auto set R to max columns based on terminal size
# make sure that $COLUMNS is available (exported in bash_profile)
if (Sys.getenv("RSTUDIO") == "") {
    .wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
      options(width=as.integer(howWide))
    }
    .wideScreen()
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

    if (suppressWarnings(!require(remotes, quietly=TRUE))) install.packages('remotes')
    if (suppressWarnings(!require(colorout, quietly=TRUE))) remotes::install_github('jalvesaq/colorout')
    if (suppressWarnings(!require(tidyverse, quietly=TRUE))) install.packages('tidyverse')
    if (suppressWarnings(!require(lookup, quietly=TRUE))) remotes::install_github('jimhester/lookup')
    if (suppressWarnings(!require(CB, quietly=TRUE))) remotes::install_github('cole-brokamp/CB')

})

# use colorout
library(colorout)
# customize colors
# use colorout::show256Colors() to show all colors in browser window
setOutputColors256(normal = 241, negnum = 247, zero = 226,
        number = 247, date = 179, string = 33,
        const = 252, false = 203, true = 78,
        infinite = 39, stderror = 88,
        warn = c(1, 0, 1), error = c(1, 15),
        verbose = FALSE, # set to TRUE to show results of setup
        zero.limit = NA)


# use lookup during interactive sessions
if(interactive()) suppressPackageStartupMessages(library(lookup))

# print time and wd on startup
.First <- function(){
  if(interactive()){
    library(utils)
    timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))

  }
}
