#00000000000000000000000000000000000000000000000000000000000#
#0000       2024-11 NW Data Commons                     0000#
#00000000000000000000000000000000000000000000000000000000000#

# 0-SETUP --------------------------------------------------------------------------------
	
  # INITIAL SETUP
    rm(list=ls()) #Remove lists
    gc()
    options(java.parameters = "- Xmx8g") #helps r not to fail when importing large xlsx files with xlsx package

    wd <- "/home/wnf/code/nw-data-commons"
    
  # SECTION & CODE CLOCKING
    
    sections.all.starttime <- Sys.time()
    section0.starttime <- sections.all.starttime
  
  # LOAD LIBRARIES/PACKAGES
    #library(wnf.utils)
    #LoadCommonPackages()
    
    library(googledrive)
    drive_auth(email = "william@fluxrme.com") 
    
    library(googlesheets4) 
    library(tidyverse) 
    library(reshape2) 
    library(data.table)
    library(lubridate)
    library(magrittr)
    library(readxl)
    library(stringr)
    library(tictoc)
    library(dplyr)
    library(ggplot2)
    library(janitor)
    library(lubridate)
    library(ncdf4)
    library(openxlsx)  
    library(purrr)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(sf)
    library(stringr)
    library(countrycode)
    library(patchwork)
    library(viridis)
    library(grid)
    library(graticule)
    library(units)
    
  # DEFINE USEFUL FUNCTIONS
   
    ListToTibbleObjects <- function(list){
      for(i in 1:length(list)){
        
        object.name.i <- paste(names(list)[i], ".tb", sep = "")
        
        assign(
          object.name.i,
          list[[i]],
          pos = 1
        )
        
        print(paste(i, ": ", object.name.i, sep = ""))
      }
    }
    
    ReplaceNames <- function(tb,current.names, new.names) {
      
      #Data Checks
      if(!is.data.frame(tb)){
        stop("Input not a data frame. Input must be of class 'data.frame'.")
      }
      
      #New Names Checks
      if(!exists("new.names")){
        new.names <- readline(prompt = "No new names defined. Enter a vector of new names to replace current names: ")
      }
      
      if(!is.character(new.names)){
        new.names <- as.character(new.names)
        warning("'new.names' input not of class 'character.' Coercing to character vector.")
      }
      
      if(!is.character(new.names)){
        new.names <- as.character(new.names)
        warning("'new.names' input not of class 'character.' Coercing to character vector.")
      }
      
      #Current Names Checks
      if(!exists("current.names")){
        
        if(length(names(tb)) == length(new.names)){
          print("No current names to replace specified. All current names will be replaced.")
          current.names <- names(tb)
        }
        
        if(length(names(tb)) != length(new.names)){
          stop(
            paste(
              "No current names to replace specified. Current tb has ",
              length(names(tb)),
              " columns. New names is of length ",
              length(new.names),
              ".",
              sep = ""
            )
          )
        }
        
      } #End of if statement for when current.names not defined by user
      
      if(any(!current.names %in% names(tb))){
        warning(
          paste(
            "One or more current.names were not found in input data frame: '",
            current.names[!current.names %in% names(tb)],
            "'. ",
            sep = ""
          )
        )
      }
      
      #Actual Function: name replacement
      names(tb)[names(tb) %in% current.names] <- new.names
      return(tb)
    }

    IndexMatchToVectorFromTibble <- function(
      vector,
      lookup.tb,
      match.varname,
      replacement.vals.varname,
      mult.replacements.per.cell = c(FALSE,TRUE),
      mult.replacements.separator.char = NULL,
      print.matches = c(TRUE,FALSE)
    ){
      if(mult.replacements.per.cell){
        lookup.tb <-
          SplitColReshape.ToLong(
            df = lookup.tb,
            id.varname = replacement.vals.varname,
            split.varname = match.varname,
            split.char = ","
          ) #strsplit(match.col, mult.replacements.separator.char) %>% unlist %>% as.vector
      }
      
      match.col <- lookup.tb %>% dplyr::select(all_of(match.varname)) %>% dplyr::pull()
      replacement.col <- lookup.tb %>% dplyr::select(all_of(replacement.vals.varname)) %>% dplyr::pull()
      matched.vals.ls <- list()
      unmatched.vals.ls <- list()
      
      for(i in 1:length(vector)){
        if(is.na(vector[i])){next()} #Skips NAs
        if(!any(match.col == vector[i])){
          unmatched.vals.ls[[i]] <- vector[i]
          warning(
            paste("No match for '", vector[i], "' found in column '", match.varname, "'.", sep = "")
          )
        }else{
          matched.vals.ls <- vector[i]
          vector[i] <- replacement.col %>% unlist %>% .[match.col == vector[i]]
        }
      }
      
      if(!missing(print.matches) && print.matches){
        matched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
          paste0("Values replaced: ",.) %>% print
        unmatched.vals.ls %>% unlist %>% as.vector %>% RemoveNA %>% paste(., collapse = ", ") %>%
          paste0("Values not replaced: ",.) %>% print
      }
      return(vector)
    }

  # SECTION CLOCKING
    section0.duration <- Sys.time() - section0.starttime
    section0.duration
    
# 1-IMPORT --------------------------------------------------------------------------------
  
  # IMPORT CONFIG TABLES ---- 
    gs4_auth(email = "william@fluxrme.com")
    
    sheet.id = "https://docs.google.com/spreadsheets/d/1M9o6hIX9R8f44-UGea09Z27yhNhK340efd6Udgwrnl8/"
        
    configs.ss <-
      as_sheets_id(sheet.id)
      
    sheet.names.v <- sheet_names(configs.ss)
    
    all.configs.ls <-
      lapply(
        sheet.names.v, 
        function(x){
          read_sheet(configs.ss, sheet = x)
        }
      )
    
    names(all.configs.ls) <- sheet.names.v
    
    #Assign each table to its own tibble object
      ListToTibbleObjects(all.configs.ls) #Converts list elements to separate tibble objects names with their respective sheet names with ".tb" appended
    
    
  # IMPORT SOURCE DATA (defining function) ----
  
    source.data.folder.id <- "1JS013_BF_b_cwfC-kirYzF-Y0yYxf31d"

    import.files.tb <- 
      drive_ls(path = as_id(source.data.folder.id)) %>%
      mutate(mimeType = map_chr(drive_resource, "mimeType")) %>%
      filter(mimeType == "application/vnd.google-apps.spreadsheet") 
      
    ImportSourceData_GoogleSheets <- function(name_of_file_to_be_imported){
        
      file.id <- 
        import.files.tb %>% 
        filter(name == name_of_file_to_be_imported) %>% 
        dplyr::select(id) %>%
        unlist %>% as.vector() %>%
        as_sheets_id(.)
      
      sheet.names <- sheet_names(file.id)
      
      configs <- source.table.configs.tb %>% filter(file.name == name_of_file_to_be_imported)
      
      import.tables.ls <- 
        lapply(
          sheet.names, 
          function(x){
            read_sheet(file.id, sheet = x)
          }
        )
      
      names(import.tables.ls) <- sheet.names #assign sheet names as list element names
      
      list.name <- configs$object.name %>% paste0(., ".ls", collapse = "")
      assign(list.name, import.tables.ls, envir = .GlobalEnv) #create a list of the imported tables in the global environment
      print(list.name)
      
    }
    
# 2-CLEANING & RESHAPING --------------------------------------------------------------------------------
  #2.0 GENERAL FUNCTIONS FOR ALL TABLES & ONE-OFF PROCEDURES ----

    #General function for flagging outliers using IQR method
    FlagOutliers_IQR <- function(
      tb, 
      config.tb = source.table.configs.tb, 
      default.multiplier = 10, 
      source.table.list.name = NULL
    ) {
      print("Looking for matching indicators in the following columns:")
      print(colnames(tb))
      print("Against config table rows:")
      print(config.tb$indicators.of.concern)

      # If source.table.list.name is provided, extract the object name
      object.name <- NULL
      if (!is.null(source.table.list.name)) {
        object.name <- deparse(substitute(source.table.list.name)) %>%
          gsub("\\.ls$", "", .) # remove .ls suffix if present
        message(paste("Detected object name:", object.name))
      }

      # Try to match config row using object.name
      matched.table <- config.tb
      if (!is.null(object.name)) {
        matched.table <- matched.table %>%
          filter(object.name == !!object.name)
      }

      # Now further filter using indicators match (if needed)
      matched.table <- matched.table %>%
        filter(sapply(indicators.of.concern, function(indicators) {
          vars <- strsplit(indicators, ",\\s*")[[1]]
          all(vars %in% colnames(tb))
        }))

      if (nrow(matched.table) == 0) {
        warning("No matching table found in config for the current tibble.")
        return(tb)
      }

      # Pull IQR multiplier
      iqr.multiplier <- matched.table$outlier.iqr.multiplier %>% first()
      if (is.null(iqr.multiplier) || is.na(iqr.multiplier)) {
        iqr.multiplier <- default.multiplier
        warning("IQR multiplier not found in config; using default.")
      }

      # Apply outlier detection
      indicators <- matched.table$indicators.of.concern %>% 
        strsplit(",\\s*") %>% unlist()

      for (colname in indicators) {
        if (!colname %in% colnames(tb)) next

        q1 <- quantile(tb[[colname]], 0.25, na.rm = TRUE)
        q3 <- quantile(tb[[colname]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower <- q1 - iqr.multiplier * iqr
        upper <- q3 + iqr.multiplier * iqr
        message(paste("Outlier bounds for", colname, ": [", round(lower, 2), ",", round(upper, 2), "]"))

        flag.col <- paste0(colname, ".outlier.flag")
        tb[[flag.col]] <- ifelse(
          tb[[colname]] < lower | tb[[colname]] > upper,
          "outlier", ""
        )
      }

      return(tb)
    }


    # #One-Off Code for fao.crop.indicators table to add iso3 codes & reshape to wider
    # fao.crop.indicators.long.tb <- fao.crop.indicators.long.tb
    # fao.crop.indicators.long.tb$`Area Code (M49)` %<>% as.numeric
    # fao.crop.indicators.long.tb %<>%
    #   mutate(country.iso3 = countrycode(`Area Code (M49)`, origin = "un", destination = "iso3c"))

    # #Filter only the crops you care about (clean names for output)
    # crop_map <- c(
    #   "Maize (corn)" = "corn",
    #   "Rice" = "rice",
    #   "Wheat" = "wheat",
    #   "Sugar cane" = "sugar.cane",
    #   "Soya beans" = "soy"
    # )

    # #Reshape from wide (years as columns) to long format
    # fao.crop.indicators.wide.tb <- fao.crop.indicators.long.tb %>%
    #   filter(Item %in% names(crop_map)) %>%
    #   mutate(
    #     crop = crop_map[Item],                       # rename crop
    #     element = tolower(gsub(" ", ".", Element))   # standardize element names
    #   ) %>%
    #   pivot_longer(cols = c(`2015`), names_to = "year", values_to = "value") %>%
    #   mutate(
    #     variable = paste(crop, element, year, sep = ".")
    #   ) %>%
    #   select(country.iso3, variable, value) %>%
    #   filter(!is.na(country.iso3)) %>%
    #   pivot_wider(names_from = variable, values_from = value)

    # setwd("/home/wnf/code/nw-data-commons/2-outputs")
    # fao.output.filename <- paste0(
    #   "fao.crop.indicators.wide.",
    #   Sys.time() %>% gsub(":",".",.) %>% substr(., 1, nchar(.)-7),
    #   ".csv",
    #   sep=""
    # )
    # write.csv(fao.crop.indicators.wide.tb, fao.output.filename)


  #2.1 TEMPERATURE ----
    
    ImportSourceData_GoogleSheets("1.temperature")

    #source_table_list <- temperature.ls[[1]]
    #source_table_names <- temperature.ls %>% names %>% .[1] #%>% list(.)
    CleanReshape_Temp <- function(source_table_list, source_table_names) {

      scenario <- 
        source_table_names %>%
        strsplit("_") %>% 
        lapply(`[`, 1) %>%
        unlist() %>%
        as.numeric()

      indicator <- 
        source_table_names %>%
        strsplit("_") %>% 
        lapply(`[`, 2) %>%
        unlist()

      result <- 
        source_table_list %>%
        ReplaceNames(., names(.), tolower(names(.))) %>%
        ReplaceNames(., c("id", "nation"), c("country.id", "country.name")) %>%
        mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>%
        select(-country.name) %>%
        reshape2::melt(id = "country.id") %>%
        mutate(
          soot.injection.scenario = scenario,
          variable = as.character(variable),
          indicator = indicator,
          years.elapsed.raw = str_extract(variable, "^[^ ]+") %>% as.numeric(),
          month = str_extract(variable, "(?<= - )\\d+") %>% as.numeric()
        ) %>%
        group_by(soot.injection.scenario) %>%
        mutate(
          years.elapsed = years.elapsed.raw - min(years.elapsed.raw, na.rm = TRUE),
          months.elapsed = years.elapsed * 12 + month
        ) %>%
        ungroup() %>%
        mutate(
          start.date = case_when(
            soot.injection.scenario == 0 ~ as.Date("01/31/2018", format = "%m/%d/%Y"),
            soot.injection.scenario %in% c(5, 16, 150) ~ as.Date("01/31/2020", format = "%m/%d/%Y"),
            TRUE ~ NA_Date_
          ),
          date = start.date %m+% months(months.elapsed - 1)  # months.elapsed starts at 1
        ) %>%
        as_tibble()

      print(source_table_names)

      return(result)
    }
    
    temperature.clean.tb <-
      Map(
          CleanReshape_Temp,
          temperature.ls,
          names(temperature.ls)
      ) %>%
      do.call(rbind, .) %>%
      pivot_wider(
        names_from = indicator,
        values_from = value
      ) %>%
      mutate( #converting units from kelvin to celsius
        surface.temp = surface.temp - 273.15,
      ) %>%
      left_join( #add months metadata (seasons in n & s hemisphere)
        ., 
        months.tb,
        by = "month"
      ) %>%
      left_join( #add country metadata from configs table
        ., 
        countries.tb,
        by = "country.id"
      ) %>%
      # mutate(
      #   surface.temp.weighted.by.land.area = surface.temp * country.land.area.sq.km,
      #   surface.temp.weighted.by.population = surface.temp * country.population.2018
      # ) %>%
      FlagOutliers_IQR() %>%
      dplyr::select( #select & order final variables
        country.name, country.iso3,	country.hemisphere,	
        country.region,	country.sub.region,	country.intermediate.region, 
        country.nuclear.weapons, country.nato.member.2024, 
        country.population.2018, country.land.area.sq.km,
        soot.injection.scenario, 
        years.elapsed, months.elapsed, date, month, season.n.hemisphere, season.s.hemisphere,
        surface.temp, surface.temp.outlier.flag, #surface.temp.weighted.by.land.area, surface.temp.weighted.by.population
        surface.temp.stdev, surface.temp.stdev.outlier.flag
      ) %>%
      as_tibble()

    temperature.clean.tb %>% as.data.frame %>% .[sample(1:nrow(.), 10),]

  #2.2 PRECIPITATION ----
    ImportSourceData_GoogleSheets("2.precipitation")

    #source_table_list <- precipitation.ls[[1]]
    #source_table_names <- precipitation.ls %>% names %>% .[1] %>% list(.)
    CleanReshape_Precip <- function(source_table_list, source_table_names) {
      
      scenario <- 
        source_table_names %>%
        strsplit("_") %>%
        lapply(`[`, 1) %>%
        unlist() %>%
        as.numeric()
      
      indicator <- 
        source_table_names %>%
        strsplit("_") %>%
        lapply(`[`, 2) %>%
        unlist()
      
      result <- 
        source_table_list %>%
        ReplaceNames(., names(.), tolower(names(.))) %>%
        ReplaceNames(., c("id", "nation"), c("country.id", "country.name")) %>%
        mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>%
        select(-country.name) %>%
        reshape2::melt(id = "country.id") %>%
        mutate(
          soot.injection.scenario = scenario,
          variable = as.character(variable),
          indicator = indicator,
          years.elapsed.raw = str_extract(variable, "^[^ ]+") %>% as.numeric(),
          month = str_extract(variable, "(?<= - )\\d+") %>% as.numeric()
        ) %>%
        group_by(soot.injection.scenario) %>%
        mutate(
          years.elapsed = years.elapsed.raw - min(years.elapsed.raw, na.rm = TRUE),
          months.elapsed = years.elapsed * 12 + month
        ) %>%
        ungroup() %>%
        mutate(
          start.date = case_when(
            soot.injection.scenario == 0 ~ as.Date("01/31/2018", format = "%m/%d/%Y"),
            soot.injection.scenario %in% c(5, 16, 150) ~ as.Date("01/31/2020", format = "%m/%d/%Y"),
            TRUE ~ NA_Date_
          ),
          date = start.date %m+% months(months.elapsed - 1)
        ) %>%
        as_tibble()
      
      print(source_table_names)
      
      return(result)
    }

    precipitation.clean.tb <-
      Map(
          CleanReshape_Precip,
          precipitation.ls,
          names(precipitation.ls)
      ) %>%
      do.call(rbind, .) %>%
      pivot_wider(
        names_from = indicator,
        values_from = value
      ) %>%
      mutate( #converting unit from m/s to mm/month
        precip.rate = precip.rate * 1000 * 86400 * 30.4375,
        precip.stdev = precip.stdev * 1000 * 86400 * 30.4375
      ) %>%
      left_join( #add months metadata (seasons in n & s hemisphere)
        ., 
        months.tb,
        by = "month"
      ) %>%
      left_join( #add country metadata from configs table
        ., 
        countries.tb,
        by = "country.id"
      ) %>%
      # mutate(
      #   precip.rate.convective.weighted.by.land.area = precip.rate.convective * country.land.area.sq.km,
      #   precip.rate.convective.weighted.by.population = precip.rate.convective * country.population.2018
      # ) %>%
      FlagOutliers_IQR() %>%
      dplyr::select( #select & order final variables
        country.name, country.iso3,	country.hemisphere,	
        country.region,	country.sub.region,	country.intermediate.region, 
        country.nuclear.weapons, country.nato.member.2024, 
        country.population.2018, country.land.area.sq.km,
        soot.injection.scenario, 
        years.elapsed, months.elapsed, date, month, season.n.hemisphere, season.s.hemisphere,
        precip.rate, precip.rate.outlier.flag,
        precip.stdev, precip.stdev.outlier.flag
      ) %>%
      as_tibble()

    precipitation.clean.tb %>% as.data.frame %>% .[sample(1:nrow(.), 10),]

    # precipitation.clean.tb %>%
    #   filter(precip.rate == precip.stdev) %>%
    #   group_by(years.elapsed, months.elapsed, soot.injection.scenario) %>%
    #   summarise(num_countries = n_distinct(country.name), .groups = "drop") %>%
    #   as.data.frame %>%
    #   arrange(soot.injection.scenario)


  #2.3 UV ----
    
    ImportSourceData_GoogleSheets("3.uv")
    
    CleanReshape_UV <- function(source_table_list, source_table_names) {
      
      scenario <- 
        source_table_names %>%
        strsplit(., "_") %>% 
        unlist %>%
        .[1] %>%
        ifelse(. != "control", paste0(., "Tg"), .)
      
      indicator <- 
        source_table_names %>%
        strsplit(., "_") %>% 
        unlist %>%
        .[2]
      
      result <- 
        source_table_list %>% 
        ReplaceNames(., names(.), tolower(names(.))) %>%
        ReplaceNames(., c("id", "nation"), c("country.id", "country.name")) %>%
        mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>%
        select(-country.name) %>%
        reshape2::melt(id = "country.id") %>%
        mutate(
          soot.injection.scenario = recode(
            scenario,
            "control" = 0,
            "150Tg" = 150
          ),
          variable = as.character(variable),
          year.raw = str_extract(variable, "^[^ ]+") %>% as.numeric(),
          month = str_extract(variable, "(?<= - )\\d+") %>% as.numeric(),
          indicator = indicator
        ) %>%
        group_by(soot.injection.scenario) %>%
        mutate(
          years.elapsed = year.raw - min(year.raw, na.rm = TRUE),
          months.elapsed = years.elapsed * 12 + month
        ) %>%
        ungroup() %>%
        mutate(value = as.numeric(value)) %>%
        left_join(countries.tb, by = "country.id") %>%
        left_join(months.tb, by = "month") %>%
        select(
          country.name, country.iso3, country.hemisphere,	
          country.region, country.sub.region, country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          soot.injection.scenario, years.elapsed, months.elapsed, month, 
          season.n.hemisphere, season.s.hemisphere,
          indicator, value
        ) %>%
        as_tibble()
      
      print(source_table_names)
      
      return(result)
    }
    
    uv.clean.tb <-
      Map(
        CleanReshape_UV,
        uv.ls,
        names(uv.ls)
      ) %>%
      bind_rows() %>%
      pivot_wider(
        names_from = indicator,
        values_from = value
      ) %>%
      ReplaceNames(., names(.), tolower(names(.))) %>%
      FlagOutliers_IQR() %>%
      as_tibble()

    #uv.clean.tb %>%
    #  select(-value) %>%
    #  apply(., 2, TableWithNA)
    
  #2.4a AGRICULTURE AGMIP (Multi-Model Aggregates, Jonas) ----
    
  #Clean & Reshape FAOSTAT crop indicators 
    fao.crop.indicators.clean.tb <- 
      fao.crop.indicators.tb %>%
      ReplaceNames(., names(.), tolower(names(.))) %>%
      ReplaceNames(., "item", "crop") %>%
      select(country.iso3, crop, year, value) %>%
      mutate(crop = tolower(crop)) %>%
      mutate(
        crop = case_when(
          crop == "maize (corn)" ~ "corn",
          crop == "soya beans" ~ "soya.beans",
          TRUE ~ crop
        )
      ) %>%
      group_by(country.iso3, crop) %>%
      summarise(mean.yield = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = crop,
        values_from = mean.yield,
        names_glue = "mean.yield.{crop}"
      )

    # fao.crop.indicators.clean.tb %>% #one-off code to produce pairwise correlation coefficients for yields of crops
    #   select(
    #     mean.yield.corn,
    #     mean.yield.rice,
    #     mean.yield.wheat,
    #     mean.yield.soya.beans
    #   ) %>%
    #   cor(use = "pairwise.complete.obs")


  #Import Agriculture.AGMIP Data
    ImportSourceData_GoogleSheets("4a.agriculture.agmip")

  #Clean & Reshape Agriculture.AGMIP Data
    CleanReshape_AgricultureAGMIP <- function(source_table_list, source_table_names) {
      
      print("Working on cleaning & reshaping:")
      print(source_table_names)

      # Extract components
      split_parts <- strsplit(source_table_names, "_")[[1]]
      cesm.model.configuration <- tolower(split_parts[1])      # "mills" or "bardeen"
      scenario <- split_parts[2]
      crop <- tolower(split_parts[3])              # normalize crop name

      result <- 
        source_table_list %>% 
        ReplaceNames(., names(.), tolower(names(.))) %>%
        select(-country_name, -`...1`) %>%
        ReplaceNames(., "country_iso3", "country.iso3") %>%
        mutate(across(where(is.list), ~ suppressWarnings(as.character(unlist(.))))) %>%
        reshape2::melt(., id = "country.iso3") %>%
        mutate(
          crop = crop,
          cesm.model.configuration = cesm.model.configuration,
          soot.injection.scenario = 5,
          years.elapsed = str_extract(variable, "(?<=_)[^_]*$") %>% as.numeric(),
          pct.change.harvest.yield = value %>% as.numeric() %>% suppressWarnings()
        ) %>%
        left_join(countries.tb, by = "country.iso3") %>%
        left_join(fao.crop.indicators.tb, by = "country.iso3") %>%
        select(
          country.name, country.iso3, country.hemisphere, country.region, country.sub.region, country.intermediate.region,
          country.nuclear.weapons, country.nato.member.2024, country.population.2018, country.land.area.sq.km,
          corn.area.harvested.2015, corn.yield.2015, corn.production.2015, 
          rice.area.harvested.2015, rice.yield.2015, rice.production.2015, 
          soy.area.harvested.2015, soy.yield.2015, soy.production.2015, 
          wheat.area.harvested.2015, wheat.yield.2015, wheat.production.2015,
          soot.injection.scenario, years.elapsed,
          cesm.model.configuration, crop, pct.change.harvest.yield
        ) %>% 
        filter(!is.na(pct.change.harvest.yield)) %>%
        as_tibble()

      return(result)
    }

    # Assemble full clean table
    agriculture.agmip.clean.tb <- 
      Map(
        CleanReshape_AgricultureAGMIP,
        agriculture.agmip.ls,
        names(agriculture.agmip.ls)
      ) %>%
      do.call(rbind, .) %>%
      mutate(
        crop = case_when(
          crop == "maize" ~ "corn",
          crop == "soy" ~ "soya.beans"
          TRUE ~ crop
        ),
        cesm.model.configuration = case_when(
          cesm.model.configuration == "bardeen" ~ "toon"
        )
      ) %>%
      pivot_wider(
        names_from = crop,
        values_from = pct.change.harvest.yield,
        names_glue = "pct.change.harvest.yield.{crop}",
        #values_fn = dplyr::first  # avoid list-columns; keeps values as-is
      )

    # agriculture.agmip.clean.tb %>% 
    #  select(-names(.)[length(names(.))]) %>% 
    #  apply(., 2, TableWithNA) #display unique values for each variable except the indicator (for checking)
    
  #2.4b AGRICULTURE CLM (Community Land Model) ----
    
    ImportSourceData_GoogleSheets("4b.agriculture.clm")
    
    CleanReshape_AgricultureCLM <- function(source_table_list, source_table_names){
      
      crop <- 
        source_table_names %>%
        strsplit(., "-") %>% 
        unlist %>%
        .[1]
      
      years.elapsed <- 
        source_table_names %>%
        strsplit(., "-") %>% 
        unlist %>%
        .[2] %>% 
        as.numeric
      
      result <- 
        source_table_list %>% 
        ReplaceNames(., names(.),tolower(names(.))) %>% #lower-case all table names
        ReplaceNames(., c("nation-id", "nation-name"), c("country.id","country.name")) %>%  #standardize geographic variable names
        select(-id, -country.name) %>%
        reshape2::melt(., id = "country.id") %>% #reshape to long
        mutate( #add/rename variables
          soot.injection.scenario = recode(
            variable, 
            "5tg" = 5,
            "16tg" = 16,
            "27tg" = 27,
            "37tg" = 37,
            "47tg" = 47,
            "150tg" = 150
          ),
          crop = crop,
          years.elapsed = years.elapsed,
          pct.change.harvest.yield = na_if(value, 9.96920996838686e+36)
        ) %>%
        left_join( #add country metadata from configs table
          ., 
          countries.tb,
          by = "country.id"
        ) %>%
        left_join(
          fao.crop.indicators.tb,
          by = "country.iso3"
        ) %>%
        select( #select & order final variables
          country.name, country.iso3,	country.hemisphere,	
          country.region,	country.sub.region,	country.intermediate.region, 
          country.nuclear.weapons, country.nato.member.2024, 
          country.population.2018, country.land.area.sq.km,
          corn.area.harvested.2015, corn.yield.2015, corn.production.2015, 
          rice.area.harvested.2015, rice.yield.2015, rice.production.2015, 
          soy.area.harvested.2015, soy.yield.2015, soy.production.2015,
          sugar.cane.area.harvested.2015, sugar.cane.yield.2015, sugar.cane.production.2015, 
          wheat.area.harvested.2015, wheat.yield.2015, wheat.production.2015,
          soot.injection.scenario, 
          years.elapsed,
          crop, 
          pct.change.harvest.yield
        ) %>% 
        as_tibble #ensure final result is a tibble
      
      print(source_table_names)
      
      return(result)

    }
    
    agriculture.clm.clean.tb <- 
      Map(
        CleanReshape_AgricultureCLM,
        agriculture.clm.ls,
        names(agriculture.clm.ls)
      ) %>%
      do.call(rbind, .) %>%
      mutate(
        crop = case_when(
          crop == "grass" ~ "livestock.pasture.grass", 
          crop == "swheat" ~ "spring.wheat",
          TRUE ~ crop
        )
      ) %>%
      pivot_wider(
        names_from = crop,
        values_from = pct.change.harvest.yield,
        names_glue = "pct.change.harvest.yield.{crop}"
      ) %>%
      FlagOutliers_IQR(source.table.list.name = agriculture.clm.ls) %>% 
      as_tibble()
    
    # agriculture.clm.clean.tb %>% 
    #   select(names(.)[!grepl("pct.change.harvest.yield|2015", names(.))]) %>% 
    #   apply(., 2, TableWithNA) #display unique values for each variable except the indicator (for checking)
    
  
  #2.5 FISH CATCH ----
    
    ImportSourceData_GoogleSheets("5.fish.catch")
    
    CleanReshape_FishCatch <- function(source_table_list, source_table_names){

      scenario <- 
        source_table_names
      
      result <- 
        source_table_list %>% 
        select(names(.)[!str_detect(names(.), "ctrl")]) %>%
        ReplaceNames(., names(.),tolower(names(.))) %>% #lower-case all table names
        ReplaceNames(., c("eez_no"), c("eez.num")) %>%
        mutate(across(where(is.list), ~ suppressWarnings(as.numeric(unlist(.))))) %>% #convert all list variables into character
        reshape2::melt(., id = "eez.num") %>% #reshape to long
        mutate( #add/rename variables
          soot.injection.scenario = recode(
            scenario, 
            "cntrl" = 0,
            "control" = 0,
            "5Tg" = 5,
            "16Tg" = 16,
            "27Tg" = 27,
            "37Tg" = 37,
            "47Tg" = 47,
            "150Tg" = 150
          ),
          years.elapsed = variable %>% str_extract(., "(?<=_)[^_]+$") %>% str_remove(., "yr") %>% as.numeric,
          indicator.raw = 
            variable %>% 
            str_extract(., "(?<=_).*?(?=_[^_]*$)"),
          value = value %>% divide_by(10^9),
        ) %>%
        mutate(
          indicator = 
            IndexMatchToVectorFromTibble(
              indicator.raw, 
              fish.catch.indicators.tb,
              "extracted.indicator.name.raw",
              "indicator.name.clean",
              mult.replacements.per.cell = FALSE
            )
        ) %>%
        pivot_wider(
          id_cols = c(soot.injection.scenario, eez.num, years.elapsed),
          names_from = indicator,
          values_from = value
        )%>%
        as_tibble #ensure final result is a tibble
      
      print(source_table_names)
      
      return(result)
      
    }

    fish.catch.clean.tb <- 
      Map(
        CleanReshape_FishCatch,
        fish.catch.ls,
        names(fish.catch.ls)
      ) %>%
      bind_rows() %>%
      left_join(fish.catch.eez.tb, by = "eez.num") %>%
      mutate(
        mean.pct.catch.change = mean.pct.catch.change * 10^9,
        std.dev.pct.catch.change = std.dev.pct.catch.change * 10^9,
        eez.name = eez.name %>% gsub("Exclusive Economic Zone", "EEZ", .),
        mean.catch.per.1000.sq.km = mean.catch / (eez.area / 1000)
      ) %>%
      FlagOutliers_IQR() %>%
      select(
        eez.name, eez.num, eez.area, 
        years.elapsed, 
        soot.injection.scenario,
        mean.catch,  
        mean.catch.per.1000.sq.km,
        mean.catch.change, 
        mean.pct.catch.change, 
        std.dev.catch,
        std.dev.catch.change,
        std.dev.pct.catch.change,
        mean.catch.outlier.flag,
        mean.catch.per.1000.sq.km.outlier.flag,
        mean.catch.change.outlier.flag,
        mean.pct.catch.change.outlier.flag,
        std.dev.catch.outlier.flag,
        std.dev.catch.change.outlier.flag,
        std.dev.pct.catch.change.outlier.flag
      )

    #fish.catch.clean.tb %>%
    #  select(
    #    eez, eez.num, 
    #    soot.injection.scenario, 
    #    years.elapsed
    #  ) %>%
    #  apply(., 2, TableWithNA)
    
  #2.6 SEA ICE ----
    
    ImportSourceData_GoogleSheets("6.sea.ice")
    
    CleanReshape_SeaIce <- function(source_table){
    
      scenario <- names(source_table)[1] %>% str_extract(., "(?<=NW-).*")
      
      result <-
        source_table %>%
        .[-1,] %>%
        ReplaceNames(., names(source_table)[1], "port") %>%
        reshape2::melt(
          .,
          id = "port"
        ) %>%
        ReplaceNames(., c("variable","value"), c("month","sea.ice.thickness.meters")) %>%
        mutate(
          months.elapsed = as.character(month) %>% gsub("\\.", "", .) %>% as.numeric %>% subtract(1),  # Clean and convert month strings
          month = (months.elapsed - 1) %% 12 + 1,  # Calculate the month (1-12)
          years.elapsed = (months.elapsed - 1) %/% 12 # Calculate the year (0, 1, 2, ...)
        ) %>%
        mutate(
          soot.injection.scenario =
            recode(scenario, 
              "37Tg" = 37,
              "46.8Tg" = 47,
              "150Tg" = 150
            )
        ) %>%
        left_join( #add months metadata (seasons in n & s hemisphere)
          ., 
          months.tb,
          by = "month"
        ) %>%
        left_join( #add port metadata
          ., 
          ports.tb,
          by = "port"
        ) %>%
        select(
          port, country, container.traffic, latitude, longitude,
          soot.injection.scenario, 
          months.elapsed, years.elapsed, month, season.n.hemisphere, season.s.hemisphere, 
          sea.ice.thickness.meters
        ) %>%
        as_tibble
      
      return(result)
    
    }
    
    sea.ice.clean.tb <-
      lapply(
        sea.ice.ls,
        CleanReshape_SeaIce
      ) %>%
      do.call(rbind, .) %>%
      FlagOutliers_IQR() %>%
      as_tibble()
    
    #sea.ice.clean.tb %>%
    #  select(-sea.ice.thickness.meters) %>%
    #  apply(., 2, TableWithNA)
  
  #2.7 CONSOLIDATE TABLES INTO LIST

    clean_object_names <- 
      source.table.configs.tb$object.name %>%
      sapply(., function(x){paste(x, ".clean.tb", sep="")}) %>%
      as.vector
    
    clean_table_names <- 
      source.table.configs.tb$object.name %>%
      as.vector()
    
    clean.tables.ls <- 
      lapply(
        clean_object_names, 
        function(x) {
          if (exists(x)) get(x) else NULL
        }
      ) %>%
      purrr::compact() # Remove NULL entries for non-existent tibbles
    
    names(clean.tables.ls) <- clean_table_names[clean_object_names %in% ls()]

  #2.8 FINAL CLEANING

    # Helper: Drop rows where all indicators of concern are NA
    filter_by_indicators_of_concern <- function(tb, table.name.raw) {
      
      num.rows.initial <- nrow(tb)

      indicators.str <- source.table.configs.tb$indicators.of.concern[
        source.table.configs.tb$object.name == table.name.raw
      ]

      indicators <- indicators.str %>%
        strsplit(",\\s*") %>%
        unlist()

      indicators <- indicators[indicators %in% names(tb)]

      if (length(indicators) == 0) return(tb)

      tb %<>%
        filter(if_any(all_of(indicators), ~ !is.na(.)))

      num.rows.final <- nrow(tb)

      percent.removed <- round(100 * (num.rows.initial - num.rows.final) / num.rows.initial, 1)

      cat(
        "Filtering for rows in '", table.name.raw, "' without data for indicator(s) of concern.\n",
        "Initial number of rows: ", num.rows.initial, "\n",
        "Final number of rows: ", num.rows.final, "\n",
        "Removed ", num.rows.initial - num.rows.final, " rows (", percent.removed, "%)\n\n",
        sep = ""
      )

      return(tb)
    }

    # Apply to each table
    clean.tables.ls <- mapply(
      filter_by_indicators_of_concern,
      clean.tables.ls,
      names(clean.tables.ls),
      SIMPLIFY = FALSE
    )


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
