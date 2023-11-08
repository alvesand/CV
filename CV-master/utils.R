create_CV_object <-  function(data_location,
                              pdf_mode = FALSE,
                              sheet_is_publicly_readable = TRUE) {
    
    cv <- list(
        pdf_mode = pdf_mode,
        links = c()
    )
    
    is_google_sheets_location <- stringr::str_detect(data_location, "docs\\.google\\.com")
    
    if(is_google_sheets_location){
        if(sheet_is_publicly_readable){
            # This tells google sheets to not try and authenticate. Note that this will only
            # work if your sheet has sharing set to "anyone with link can view"
            googlesheets4::gs4_deauth()
        } else {
            # My info is in a public sheet so there's no need to do authentication but if you want
            # to use a private sheet, then this is the way you need to do it.
            # designate project-specific cache so we can render Rmd without problems
            options(gargle_oauth_cache = ".secrets")
        }
        
        read_gsheet <- function(sheet_id){
            googlesheets4::read_sheet(data_location, sheet = sheet_id, skip = 1, col_types = "c")
        }
        cv$entries_data  <- read_gsheet(sheet_id = "entries")
        cv$skills        <- read_gsheet(sheet_id = "language_skills")
        cv$text_blocks   <- read_gsheet(sheet_id = "text_blocks")
        cv$contact_info  <- read_gsheet(sheet_id = "contact_info")
        cv$post_info  <- read_gsheet(sheet_id = "post_info")
    } else {
        # Want to go old-school with csvs?
        cv$entries_data <- readr::read_csv(paste0(data_location, "entries.csv"), skip = 1)
        cv$skills       <- readr::read_csv(paste0(data_location, "language_skills.csv"), skip = 1)
        cv$text_blocks  <- readr::read_csv(paste0(data_location, "text_blocks.csv"), skip = 1)
        cv$contact_info <- readr::read_csv(paste0(data_location, "contact_info.csv"), skip = 1)
        cv$post_info <- readr::read_csv(paste0(data_location, "post_info.csv"), skip = 1)
    }
    
    
    extract_year <- function(dates){
        date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
        date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10
        
        date_year
    }
    
    parse_dates <- function(dates){
        
        date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
        date_month[is.na(date_month)] <- "1"
        
        paste("1", date_month, extract_year(dates), sep = "-") %>%
            lubridate::dmy()
    }
    
    # Clean up entries dataframe to format we need it for printing
    cv$entries_data %<>%
        tidyr::unite(
            tidyr::starts_with('description'),
            col = "description_bullets",
            sep = "\n- ",
            na.rm = TRUE
        ) %>%
        dplyr::mutate(description_bullets = as.list(strsplit(description_bullets , "\n- ")) ) %>% 
        dplyr::mutate(
            # description_bullets = ifelse(description_bullets != "", paste0("- ", description_bullets), ""),
            start = ifelse(start == "NULL", NA, start),
            end = ifelse(end == "NULL", NA, end),
            start_year = extract_year(start),
            end_year = extract_year(end),
            no_start = is.na(start),
            has_start = !no_start,
            no_end = is.na(end),
            has_end = !no_end,
            timeline = dplyr::case_when(
                no_start  & no_end  ~ "N/A",
                no_start  & has_end ~ as.character(end),
                has_start & no_end  ~ paste("Current", "-", start),
                TRUE                ~ paste(end, "-", start)
            )
        ) %>%
        dplyr::arrange(desc(parse_dates(end))) %>%
        dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .))
    
    cv
}

sanitize_links <- function(cv, text){
    if(cv$pdf_mode){
        link_titles <- stringr::str_extract_all(text, '(?<=\\[).+?(?=\\])')[[1]]
        link_destinations <- stringr::str_extract_all(text, '(?<=\\().+?(?=\\))')[[1]]
        
        n_links <- length(cv$links)
        n_new_links <- length(link_titles)
        
        if(n_new_links > 0){
            # add links to links array
            cv$links <- c(cv$links, link_destinations)
            
            # Build map of link destination to superscript
            link_superscript_mappings <- purrr::set_names(
                paste0("<sup>", (1:n_new_links) + n_links, "</sup>"),
                paste0("(", link_destinations, ")")
            )
            
            # Replace the link destination and remove square brackets for title
            text <- text %>%
                stringr::str_replace_all(stringr::fixed(link_superscript_mappings)) %>%
                stringr::str_replace_all('\\[(.+?)\\]', "\\1")
        }
    }
    
    list(cv = cv, text = text)
}

# print text block
print_text_block <- function(cv, label){
    text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
        dplyr::pull(text)
    
    strip_res <- sanitize_links(cv, text_block)
    
    cat(strip_res$text)
    
    invisible(strip_res$cv)
}

# print table of skills
print_skill <- function(cv){
    
    glue_template <- "
\\cvskill{<<group>>}{<<skills>>}"
    
    skills_formatted <- cv$skills %>%
        mutate(skill = if_else(is.na(details)|details=="", glue::glue("{skill}"), glue::glue("{skill} ({details})"))) %>% 
        group_by(group) %>% 
        summarize(skills = glue::glue_collapse(skill, sep = ", ")) 
    
    cv_skill <- skills_formatted %>% 
        glue::glue_data(glue_template, .open = "<<", .close = ">>" ) %>% 
        paste0(., collapse = '\n')
    
    cv_skill_env <- paste0('\n\\begin{cvskills}\n', cv_skill, '\n\\end{cvskills}\n' , collapse= '\n')
    
    cat(cv_skill_env)
    
    invisible(cv)
}

# print table of posts
print_post<- function(cv, glue_template = "default"){
    
    if(glue_template == "default"){
        glue_template <- "
- [{name}]({link})
\n"
    }  
    print(glue::glue_data(cv$post_info , glue_template))
    
    invisible(cv)
}

