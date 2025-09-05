# 3-EXPORT --------------------------------------------------------------------------------
  
  # DEFINE & CREATE OUTPUT DIRECTORY
    
    output.base.name <- 
      Sys.time() %>% 
      gsub(":",".",.) 
      
    output.dir <-
      paste(
        wd,
        "/2-outputs/",
        output.base.name,
        "/",
        sep = ""
      )
    
    if(output.dir %>% dir.exists %>% not){
      dir.create(output.dir, recursive = TRUE)
    }
    
  # WRITE CSV FILES INTO OUTPUT DIRECTORY
    
    ExportCsvs <- 
      function(table, table_name){
        file.name <- paste(table_name,"_",output.base.name,".csv",sep="")
        write.csv(table, file.name, row.names = FALSE, na = "")
      }
    
    setwd(output.dir)

    Map(
      ExportCsvs,
      clean.tables.ls,
      names(clean.tables.ls)
    )
    
  # WRITE TABLES INTO SINGLE EXCEL FILE IN OUTPUT DIRECTORY
    
    output.file.path <- 
      paste(output.dir, "Reformatted Data_Analysis_",Sys.Date(),".xlsx", sep = "") %>%
      file.path(.)
      
    wb <- createWorkbook()
    
    for (i in seq_along(clean.tables.ls)) {
      sheet_name <- names(clean.tables.ls)[i] %>% gsub("\\.tb","",.)  # Get the name of the list element (if named)
      if (is.null(sheet_name) || sheet_name == "") {
        sheet_name <- paste0("Sheet", i)  # Assign default sheet names if missing
      }
      
      addWorksheet(wb, sheet_name)  # Create a new sheet
      writeData(wb, sheet = sheet_name, clean.tables.ls[[i]])  # Write data
    }
    
    saveWorkbook(wb, output.file.path, overwrite = TRUE)
    
    cat("Excel file successfully saved at:", output.file.path, "\n") # Print confirmation

  # CODE CLOCKING
    code.duration <- Sys.time() - sections.all.starttime
    code.duration
