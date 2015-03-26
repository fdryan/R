
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
	

# Takes SQL statement(s) (seperated by ";") or a reference to a sql file 
# By default it will return SQL character vectors and will not execute
# the statements
# If you want to run the SQL the change execute to TRUE
# If statements are executed they will run against a default connection
# object called "conn". You can alter the connection by giving a different
# object to the connection argument.
# Results from multiple statements will be returned from a list.
# Results of a single statement will be returned as a dataframe.

    sqlQry <- function(sqlString, execute = FALSE, connection = conn){
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
            if (length(data.out) == 1){data.out <- data.out[[1]]}
            return (data.out)    
        } else {
                
            return (sqlString)
        }    
	}
