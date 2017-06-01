#
# Easy script to write a SQL statement or PL/SQL procedure for updating
# geometries in Oracle Locator, from a geometry WKT string. The PL/SQL 
# procedure will be handled in case the geometry WKT length exceeds the
# maximum limit (4K characters).
#
# @author eblondel
# @date 2017/05/30
#

#split a string x into chunks of size n
 getChunks <- function(x,n){
    sst <- strsplit(x, '')[[1]]
    m <- matrix('', nrow=n, ncol=(length(sst)+n-1)%/%n)
    m[seq_along(sst)] <- sst
    as.list(apply(m, 2, paste, collapse=''))
 }
 
#prepare a SQL statement (SQL or PL/SQL depending on the length of geometry string)
 prepareSQLStatement <- function(wkt, sqlType, trgTable, trgValues = NULL, trgFilters = NULL){
    sql <- ""
    chunkSize <- 4000 #limit of number of characters.
    chunks <- list()
    byChunks <- FALSE
    sqlNote <- ""
    if(nchar(wkt) > chunkSize){
        byChunks <- TRUE
        chunks <- getChunks(wkt, chunkSize)
        sqlNote = paste0("-- Note: Given the size of the geometry WKT (", nchar(wkt), " characters), the SQL generated is a PL/SQL ", sqlType," procedure based on WKT chunks.\n")
    }else{
        sqlNote = paste0("-- Note: Given the size of the geometry WKT (< 4000 characters), the SQL generated is a simple SQL ", sqlType)
    }
    
    #sql comment/statement
    sql = sprintf("-- %s SQL %s statement \n", trgTable, sqlType)
    thegeomField <- paste0("'", wkt, "'")
    if(byChunks){
        #begin procedure
        thegeomField = "the_clob"
        sql <- paste0(sql, sqlNote)
        sql <- paste0(sql, "DECLARE\n")
        sql <- paste0(sql, "   the_clob CLOB;\n")
        sql <- paste0(sql, "BEGIN\n")
        sql <- paste0(sql, "   the_clob := empty_clob();\n")
        for(chunk in chunks){   
            sql <- paste0(sql, "   the_clob := CONCAT(the_clob,'", chunk, "');\n")
        }
    }
        
    #sql statement UPDATE
    if(sqlType == "UPDATE"){
        sql <- paste0(sql, "UPDATE ", trgTable, "\n")
        sql <- paste0(sql, "SET\n\t")
        sql <- paste0(sql, "THE_GEOM = SDO_GEOMETRY(", thegeomField, ",4326)")
        
        #eventual extra fields to update
        if(is.list(trgValues)){
           if(is.list(trgValues)){
             values <- names(trgValues)
             valueList <- lapply(values, function(x){
               sqlValue <- trgValues[[x]]
               if(class(sqlValue) == "character"){
                 sqlValue <- paste0("'", sqlValue, "'")
               }
               sqlSetValue <- paste0(x, " = ", sqlValue)
               return(sqlSetValue)
             })
             sql <- paste0(sql, ",\n\t")
             sql <- paste0(sql, paste(valueList, collapse = ",\n\t"), "\n")
           }
        }
        
        #eventual filters to apply
        if(!is.null(trgFilters)){
            if(is.list(trgFilters)){
                filters <- names(trgFilters)
                filterList <- lapply(filters, function(x){
                    sqlFilterValue <- trgFilters[[x]]
                    if(class(sqlFilterValue)=="character"){
                        sqlFilterValue <- paste0("'", sqlFilterValue, "'")
                    }
                    sqlFilter <- paste0(x, " = ", sqlFilterValue)
                    return(sqlFilter)
                })
                sql <- paste0(sql, "WHERE ")
                sql <- paste0(sql, paste(filterList, collapse = " AND "), ";\n")
            }
        }
    }else if(sqlType == "INSERT"){
        trgFields <- c("THE_GEOM", names(trgValues))
        trgValues <- paste(as.vector( sapply(trgValues, function(x){
           newVal <- x;
           if(class(x)=="character"){
               newVal <- paste0("'",newVal,"'")
           };
           return(newVal)
        })),collapse=", ")
        sql <- paste0(sql, "INSERT INTO ", trgTable, " (", paste(trgFields, collapse=", "), ") ")
        sql <- paste0(sql, "VALUES(SDO_GEOMETRY(", thegeomField, ", 4326), ", trgValues, ");\n\n")
    }else{
        stop(sprintf("SQL type %s not supported by this routine!", sqlType))
    }
    
    #commit
    sql <- paste0(sql, "COMMIT;\n")
        
    if(byChunks){
        #end procedure
        sql <- paste0(sql, "END;")
    }
    
    return(sql)
 }
 
 #write the SQL file calling prepareSQLStatement
 writeSQLFile <- function(filename, wkt, sqlType, trgTable, trgValues = NULL, trgFilters = NULL){
        #write header for sql file    
        headerline = "-- R routine - https://github.com/openfigis/sdiR/tree/master/R/update_oracle_geometries.R \n"
        headerline <- paste0(headerline, "-- SQL automatically generated on ", date(), "\n")
        headerline <- paste0(headerline, "-- User parameters:\n")
        headerline <- paste0(headerline, "--   * Target table: ", trgTable, "\n")
        headerline <- paste0(headerline, "--   * Target table values: ", ifelse(is.null(trgValues), "-", paste(sapply(names(trgValues), function(x){paste(x, "=", trgValues[[x]])}), collapse="; ")), "\n")  
        headerline <- paste0(headerline, "--   * Target table filters: ", ifelse(is.null(trgFilters), "-", paste(sapply(names(trgFilters), function(x){paste(x, "=", trgFilters[[x]])}), collapse="; ")), "\n")     
        headerline <- paste0(headerline, "--   * SQL Statement type: ", sqlType, "\n")
        headerline <- paste0(headerline, "--   * Output file name: ", filename, "\n")
        headerline <- paste0(headerline, "\n\n")
       
        #call prepareSQLStatement
        sql <- prepareSQLStatement(wkt, sqlType, trgTable, trgValues, trgFilters)
        
        #merge & write
        sql <- paste0(headerline, sql)
        writeLines(sql, filename)
}

#try it
if(F){
  geom <- "your WKT geometry too large to be managed with simple SQL"
  writeSQLFile("myfile.sql", geom, "UPDATE", "RFB_COMP", trgFilters = list(RFB = "COMHAFAT-ATLAFCO"))
}
