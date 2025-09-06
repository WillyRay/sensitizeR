
#' @title Sensitize Functions
#' @description Functions for the sensitizeR package.
#' @author WillyRay
#' @keywords internal
#' @seealso \code{?sensitizeR}
#' @details This file contains core functions for the sensitizeR package.

# sensitizer

#' Join Parameter and output CSV Files by a Common Column
#'
#' 
#'

#' Join Model Input and Output CSV Files by a Shared Key
#'
#' This function takes a CSV (.csv or .txt) file containing model input parameters and a CSV file containing model outputs.
#' The third argument is the shared key column name present in both files. The function returns a dataframe containing all columns
#' from the first file and all columns from the second file, joined on the shared key. If duplicate columns exist (other than the key),
#' they are deduplicated if their data matches; otherwise, the function aborts and notifies the user.
#'
#' @param file1 Path to the first CSV file (model input parameters).
#' @param file2 Path to the second CSV file (model outputs).
#' @param by The name of the shared key column to join on (character).
#' @return A data.frame containing all columns from both files, joined on the shared key.
#' @details
#' - Both files must contain the shared key column specified by `by`.
#' - If duplicate columns (other than the key) have different data, the function aborts and returns NULL.
#' - Accepts both .csv and .txt files in standard CSV format.
#' @examples
#' join_csv_files("data1.csv", "data2.csv", "id")
#' join_csv_files("inputs.txt", "outputs.txt", "run")
#' @export


join_csv_files <- function(file1, file2, by) {
	df1 <- read.csv(file1, stringsAsFactors = FALSE)
	df2 <- read.csv(file2, stringsAsFactors = FALSE)
	df <- merge(df1, df2, by = by)

	# Find duplicate column names (excluding the join column)
	col_names <- colnames(df)
	dups <- col_names[duplicated(col_names) & col_names != by]
	if (length(dups) > 0) {
		for (dup in unique(dups)) {
			# Find all columns with this name
			cols <- which(col_names == dup)
			# Compare data for all rows
			same <- TRUE
			for (i in 2:length(cols)) {
				if (!all(df[[cols[1]]] == df[[cols[i]]], na.rm = TRUE)) {
					same <- FALSE
					break
				}
			}
			if (!same) {
				message(sprintf("Duplicate column '%s' has different data. Aborting join.", dup))
				return(invisible(NULL))
			} else {
				# Remove all but the first duplicate column
				df <- df[ , -cols[-1]]
				col_names <- colnames(df)
			}
		}
	}
	return(df)
}






