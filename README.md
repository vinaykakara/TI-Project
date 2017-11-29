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
If you also chose the weakest machine type like I did, many packages won’t be able to install because of not enough memory. We need to add 1G of swap space:
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

Open Rstudio and type the following commands in terminal
