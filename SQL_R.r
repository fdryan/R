
# ---------------------------------------------------------------------------------------------
# Functions for working with SQL in R
# ---------------------------------------------------------------------------------------------

# Read in a SQL file from the wroking directory
# e.g. read.sql("sql/test.sql")

	read.sql <- function(filename, silent = TRUE) {
		q <- readLines(filename, warn = !silent)
		q <- q[!grepl(pattern = "^\\s*--", x = q)] # remove full-line comments
		q <- sub(pattern = "--.*", replacement="", x = q) # remove midline comments
		q <- paste(q, collapse = " ")
		return(q)
	}
	
# ---------------------------------------------------------------------------------------------
#'
#' sqlQry(sqlString, execute = F, connection = conn,  view = F, csv = NULL, output.dir = "output/)
#'
#' A versatile function for working with SQL in R.
#'
#' You can pass the function single or multiple SQL strings (seperated by ";") or a reference to
#' a SQL file containing one or more statements seperated by ";"
#'
#' Default behaviour is to not actually run but return a vector containing SQL statements.
#'
#' If you execute SQL against the database, there are options to
#'  • View() the results of the queries immediatly
#'  • Save the results of the queries (to .csv)
#'
#' If a single statement is run a dataframe will be returned
#' If multiple statements are run a list will be returned
#' @sqlString A sql string or a reference to a .SQL file
#' @execute Boolean run SQL or not
#' @connection a database connection object
#' @view Boolean Optional choice to immediatly View() the results of the staments
#' @csv Optional string or vector containing file names to use to save results of queries to csv
#' @output.dir Location to save CSV results. Default is Output directory within wd()
# ---------------------------------------------------------------------------------------------
    sqlQry <- function(sqlString,
                       execute = FALSE,
                       connection = conn,
                       view = FALSE,
                       csv = NULL,
                       output.dir = "output/"){
        sqlString <- str_trim(sqlString)

        if (str_sub(sqlString, -4) == ".sql"){ # if input string is a file then get it
            sqlString <- read.sql(sqlString)
        }

        sqlString <- strsplit(sqlString, ";")[[1]]

        if (execute == TRUE){
            data.out <- list()
            for ( i in 1:length(sqlString)){
                qry <- sqlString[i]
                data.out[[i]] <- sqlQuery( connection, qry)
            }

            if (length(data.out) == 1){
                data.out <- data.out[[1]]
            }

            if(length(csv) !=0  & length(data.out) == 1 ){
                write.csv(data.out, csv)
            } else if  ( length(csv) != 0 & length(csv) != length(data.out)){
                warning("Data will not be saved to CSV as number of outputs differs from the number of file names provided", call. = F)
            } else if ( length(csv) == length(data.out)){
                for(i in 1:length(csv)){
                    if(csv[i] != ""){write.csv(data.out[[i]], paste(output.dir, csv[i], str_replace_all(Sys.time(), ":", "-"), ".csv", sep = ""))}
                }
            }
            if(view){
                for(i in 1:length(data.out)) View(data.out[[i]], paste("Query", i))
                }
            return (data.out)
        } else {
            return (sqlString)
        }
	}
