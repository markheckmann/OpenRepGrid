### This file is part of the OpenRepGrid package for R.  It is made
### available under the terms of the GNU General Public License,
### version 2, or at your option, any later version, incorporated
### herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE. See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA


# environment in package namespace used to save package
# settings
.OpenRepGridEnv <- new.env()
assign("settings", list(), envir = .OpenRepGridEnv)


.onAttach <- function(lib, pkg) {
  packageStartupMessage(
    "------------------------------------------------",
    "\n OpenRepGrid v", utils::packageDescription("OpenRepGrid", fields = "Version"),
    "\n Tools for the analysis of repertory grid data",
    "\n See: https://docs.openrepgrid.org/",
    "\n------------------------------------------------",
    appendLF = TRUE
  )

  # invisible object saved in environment in namespace
  setDefaultSettings()
}


# clean up workspace
# .onUnload <- function(lib){
#
# }
