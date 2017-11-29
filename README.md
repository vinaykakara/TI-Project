# TI-Project

This tutorial helps to learn about deploying shiny apps in shiny server from scratch.

In this tutorial we will discuss about how to install **R,Rstudio,Shiny server**

Ihis tutorial is based upon 64 bit ubuntu system. 

# Install R
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
# Install Rstudio

