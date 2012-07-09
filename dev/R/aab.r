

# check and install

#system("cd /Users/markheckmann/Documents/Magic\\ Briefcase/DA\\ openRepgrid/openrepgrid/basic")
cd /Users/markheckmann/Documents/Magic\ Briefcase/DA\ openRepgrid/openrepgrid/basic/
#cd /Users/unimitarbeiter/Documents/"Magic Briefcase"/"DA openRepgrid"/openrepgrid/basic/

R CMD build OpenRepGrid
R CMD check OpenRepGrid
R CMD INSTALL -l /Library/Frameworks/R.framework/Resources/library/ OpenRepGrid_0.1.6.tar.gz

library(OpenRepGrid) # load lib
?OpenRepGrid


