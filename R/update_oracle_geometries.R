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
 prepareSQLStatement <- function(wkt, sqlType, trgTable, trgFilter = NULL){
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
        sql <- paste0(sql, "UPDATE ", trgTable, " ")
        sql <- paste0(sql, "SET")
        sql <- paste0(sql, " THE_GEOM = SDO_GEOMETRY(", thegeomField, ",4326)")
        if(!is.null(trgFilter)){
            if(is.list(trgFilter)){
                filters <- names(trgFilter)
                filterList <- lapply(filters, function(x){
                    sqlFilterValue <- trgFilter[[x]]
                    if(class(sqlFilterValue)=="character"){
                        sqlFilterValue <- paste0("'", sqlFilterValue, "'")
                    }
                    sqlFilter <- paste0(x, " = ", sqlFilterValue)
                    return(sqlFilter)
                })
                
                sql <- paste0(sql, " WHERE ")
                sql <- paste0(sql, paste(filterList, collapse = " AND "), ";\n\n")
            }
        }
    }else{
        stop(sprintf("SQL type %s not supported by this routine!", sqlType))
    }
        
    if(byChunks){
        #end procedure
        sql <- paste0(sql, "END;")
    }
    
    return(sql)
 }
 
 #write the SQL file calling prepareSQLStatement
 writeSQLFile <- function(filename, wkt, sqlType, trgTable, trgFilter = NULL){
        #write header for sql file    
        headerline = "-- R routine - https://github.com/openfigis/sdiR/tree/master/R/update_oracle_geometries.R \n"
        headerline <- paste0(headerline, "-- SQL automatically generated on ", date(), "\n")
        headerline <- paste0(headerline, "-- User parameters:\n")
        headerline <- paste0(headerline, "--   * Target table: ", trgTable, "\n")
        headerline <- paste0(headerline, "--   * Target table filters: ", paste(sapply(names(trgFilter), function(x){paste(x, "=", trgFilter[[x]])}), collapse="; "), "\n")        
        headerline <- paste0(headerline, "--   * SQL Statement type: ", sqlType, "\n")
        headerline <- paste0(headerline, "--   * Output file name: ", filename, "\n")
        headerline <- paste0(headerline, "\n\n")
       
        #call prepareSQLStatement
        sql <- prepareSQLStatement(wkt, sqlType, trgTable, trgFilter)
        
        #merge & write
        sql <- paste0(headerline, sql)
        writeLines(sql, filename)
}

#try it
if(TRUE){
  geom <- "your WKT geometry too large to be managed with simple SQL"
  writeSQLFile("myfile.sql", geom, "UPDATE", "RFB_COMP", trgFilter = list(RFB = "COMHAFAT-ATLAFCO"))
}
