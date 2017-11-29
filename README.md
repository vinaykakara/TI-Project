# TI-Project

This tutorial helps to learn about deploying shiny apps in shiny server from scratch.

In this tutorial we will discuss about how to install **R,Rstudio,Shiny server**

Ihis tutorial is based upon 64 bit ubuntu system. 

# Install R
Open termial in ubuntu and type the following:
To ensure we get the most recent version of R, we need to first add xenial (our Ubuntu release name) to our sources.list:
```
sudo sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list'
```
Now add public keys
```
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
```
Now install R
```
sudo apt-get update
sudo apt-get -y install r-base
```
Now you should be able to run R by typing
```
R
```
Now quit R by typing quit() or open new terminal
If you chose the weakest machine type, many packages won’t be able to install because of not enough memory. We need to add 1G of swap space:
```
sudo /bin/dd if=/dev/zero of=/var/swap.1 bs=1M count=1024
sudo /sbin/mkswap /var/swap.1
sudo /sbin/swapon /var/swap.1
sudo sh -c 'echo "/var/swap.1 swap swap defaults 0 0 " >> /etc/fstab'
```
Lets install some dependencies to run R
```
sudo apt-get -y install libcurl4-gnutls-dev
sudo apt-get -y install libxml2-dev
sudo apt-get -y install libssl-dev
```
# Install Rstudio
Go back to terminal and type the following:
```
sudo apt-get install gdebi-core
wget https://download1.rstudio.org/rstudio-1.0.44-amd64.deb
sudo gdebi rstudio-1.0.44-amd64.deb
rm rstudio-1.0.44-amd64.deb
```
# Install R packages
Open terminal and type the following commands
```
sudo apt-get install openssl
```
Some packages are available in github so install devtools to install packages from Github
```
sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"devtools::install_github('daattali/shinyjs')\""
```

Open Rstudio and type the following commands in terminal
```
install.packages(c("shiny","shinydashboard","ggplot2","plotly","lubridate","shinythemes","reshape","dplyr","tidyr","xts","pool","dplyr","shinyjs","xlsx","readxl","DT","pool","bubbles","shinySignals","plyr"))
```
Install some packages from github
```
devtools::install_github("jcheng5/bubbles")
```
If there are any problems install packages see https://www.digitalocean.com/community/tutorials/how-to-install-r-packages-using-devtools-on-ubuntu-16-04

To install rJava package:
First install Java if it is not present
```
sudo apt-get install default-jre
```
Then install JDK
```
sudo apt-get install default-jdk
```
Then assotiate the JDK installed with R
```
sudo R CMD javareconf
```
Install RJava and Rgdal
```
sudo apt-get install r-cran-rjava
sudo apt-get install libgdal1-dev libproj-dev
```
Install package in RStudio: Open Rstudio and type
```
install.packages("rJava")
```
