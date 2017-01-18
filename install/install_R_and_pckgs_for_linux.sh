## update system
sudo apt-get update && sudo apt-get upgrade

## install necessary packages
apt-get update && apt-get install -y \
    gdebi-core \
    pandoc pandoc-citeproc \
    libproj-dev libgdal-dev \
    libxml2-dev libxt-dev libcairo2-dev \
    libssh2-1-dev libcurl4-openssl-dev \
    curl less git make wget

# install R
echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list \
    && apt-get update \
    && apt-get install r-base-core -y --force-yes \

# set default CRAN repo and DL method
echo 'options(repos=c(CRAN = "https://cran.rstudio.com/"), download.file.method="wget")' >> /etc/R/Rprofile.site

# run the following in R to install packages as sudo to make avail for all users
sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"devtools::install_github('cole-brokamp/CB')\""
sudo su - -c "R -e \"devtools::install_github('cole-brokamp/automagic')\""
