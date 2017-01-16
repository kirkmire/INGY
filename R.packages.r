# store_packages.R
#
# stores a list of your currently installed packages

tmpp = installed.packages()

installedpackages = as.vector(tmpp[is.na(tmpp[,"Priority"]), 1])
save(installedpackages, file=(paste(getwd(),'/installed_packages.rda',sep = "")))

# restore_packages.R
#
# installs each package from the stored list of packages

load((paste(getwd(),'/installed_packages.rda',sep = "")))

for (count in 1:length(installedpackages)) install.packages(installedpackages[count])
