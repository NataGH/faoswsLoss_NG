##' Build Modified Cells
##' @rdname buildModifiedCells
##' @param dimensions
##' @param coordinates
##' 
##' @import data.table 


.buildModifiedCells <- function(dimensions, coordinates) {

	# Check passed arguments.
	#
	if(missing(dimensions)) {
		stop("The dimensions parameter cannot be omitted.")
	}
	if(missing(coordinates)) {
		stop("The coordinates parameter cannot be omitted.")
	}
	if(length(dim(coordinates)) != 2) {
		stop("The coordinates must be represented as a bidimensional matrix.")
	}
	if(dim(coordinates)[2] != length(dimensions)) {
		stop("The length of the dimensions array mismatch the number of columns of the passed coordinates matrix.")
	}

	# Assign the column names to the coordinates matrix, using the specified dimensions.
	#
	colnames(coordinates) <- dimensions

	# Return the matrix as a data.table object.
	#
	as.data.table(coordinates)
}
