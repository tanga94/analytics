# Please just run these lines like once month or so. It will check to see whether there is a new version of R and will give 
# you the option to update. This is just to make sure that you have the latest version of R and all the packages so that
# everything will work


# This function installs & loads the package you need to download R
if (!require(installr)) {
  install.packages("installr")
  require(installr)
}


check.for.updates.R()  # This tells you if there is a new version of R or not. 

install.R()  # If there was a pop up that said there is a new version, then run this line.
             # Ignore this line if there isn't a newer version

copy.packages.between.libraries() # copy your packages to the newest R installation from the
# one version before it.  (if ask=T, it will ask you between which two versions to perform the copying)

update.packages()  # This will update your packages.
