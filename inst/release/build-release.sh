# exporting so it's visible to R CMD build
export R_LIBS=$HOME/.m2/R_repository
DIR=$(pwd)
BASHDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Make directory if it doesn't exist
mkdir -p $R_LIBS

# download deps, if required. then actually build
Rscript "$BASHDIR/build-release.R" $R_LIBS $DIR && R CMD build .

if [ $? -gt 0 ]
then 
 	exit 1;
fi




