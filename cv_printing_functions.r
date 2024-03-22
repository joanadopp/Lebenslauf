# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!

#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param source_location Where is the code to build your CV hosted?
#' @param pdf_mode Is the output being rendered into a pdf? Aka do links need
#'   to be stripped?
#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier.)
#' @return A new `CV_Printer` object.
create_CV_object <-  function(data_location,
                              pdf_mode = FALSE,
                              sheet_is_publicly_readable = TRUE) {

  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  # Assuming data_location is a Google Sheets location
  if (sheet_is_publicly_readable) {
    # This tells Google Sheets to not try and authenticate.
    # Note that this will only work if your sheet has sharing set to "anyone with link can view"
    googlesheets4::gs4_deauth()
  } else {
    # My info is in a public sheet so there's no need to do authentication,
    # but if you want to use a private sheet, then this is the way you need to do it.
    # designate project-specific cache so we can render Rmd without problems
    options(gargle_oauth_cache = ".secrets")
  }

  read_gsheet <- function(sheet_id){
    googlesheets4::read_sheet(data_location, sheet = sheet_id, skip = 1, col_types = "c")
  }

  cv$entries_data <- read_gsheet(sheet_id = "entries")
  cv$text_blocks <- read_gsheet(sheet_id = "text_blocks")
  cv$contact_info <- read_gsheet(sheet_id = "contact_info")
  cv$list <- read_gsheet(sheet_id = "list")
  cv$output <- read_gsheet(sheet_id = "output")
  cv$side <- read_gsheet(sheet_id = "side")

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
    tidyr::unite(
      tidyr::starts_with('extra'),
      col = "extras",
      sep = "\n\n ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", description_bullets), ""),
      extras = ifelse(extras != "", extras, ""),
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

  # Clean up output dataframe to format we need it for printing
  cv$output %<>%
    tidyr::unite(
      tidyr::starts_with('institution'),
      col = "institution_bullets",
      sep = "<br>",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      institution_bullets = ifelse(institution_bullets != "", institution_bullets, ""))

  cv$side %<>%
    tidyr::unite(
      tidyr::starts_with('entry'),
      col = "entry_bullets",
      sep = "<br>",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      entry_bullets = ifelse(entry_bullets != "", entry_bullets, ""))

  cv
}

#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id, glue_template = "default"){

  if(glue_template == "default"){
    glue_template <- "
### {title}

{ifelse(!is.na(loc), {loc}, {description_bullets})}

{ifelse(!is.na(institution), {institution}, '')}

{timeline}

{description_bullets}

{extras}
\n\n\n"
  }

  section_data <- dplyr::filter(cv$entries_data, section == section_id & in_resume == "TRUE")
  print(glue::glue_data(section_data, glue_template))
  invisible(cv)
}

#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `output` table

print_output_section <- function(cv, section_id){
  section_data <- cv$output %>%
    dplyr::filter(section == section_id & in_resume == "TRUE") %>%
    # extract first four characters from entries in `year` column, so that "start--end" or "start-current" entries can be sorted along with entries where only one year is listed
    dplyr::mutate(datum = stringr::str_sub(year, 1, 4)) %>%
    # sort newest to oldest
    dplyr::arrange(desc(datum))

  # for line indentation: > (4 spaces don't work)
  # for line breaks: <br> (/ doesn't work)
  # must add open line between <br> and >
  # \n (must be 3 of them) resets indentation for new entry
  section_data %>%
    glue::glue_data(
      " > {title}<br>
      
      > > {ifelse(!is.na(institution_bullets), institution_bullets, '')}
      <br>
      \n\n\n
      "
    ) %>%
    print()
  invisible(cv)
}

#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)
  cat(text_block)
  invisible(cv)
}

#' @description Contact information section with icons
print_contact_info <- function(cv){
  glue::glue_data(
    cv$contact_info,
    "- <i class='fa fa-{icon}' style='color: #53DD6C;'></i> {contact}"
  ) %>% print()

  invisible(cv)
}

print_list <- function(cv, section_id){
  cv$list %>%
    dplyr::filter(section == section_id & in_resume == TRUE) %>%
    glue::glue_data(
      "> <i class='fa fa-{ifelse(is.na(icon), ' ', icon)}' style='color: #53DD6C;'></i> {item}"
    ) %>%
    print()
  invisible(cv)
}


print_side_section <- function(cv, section_id){
  cv$side %>%
    dplyr::filter(in_resume == TRUE & section == section_id) %>%
    glue::glue_data(
      "{entry_bullets}

      \n\n\n
      "
    ) %>%
    print()
  invisible(cv)
}

#print_output_section <- function(cv, section_id){
 # section_data <- cv$output %>%
  #  dplyr::filter(section == section_id & in_resume == "TRUE") %>%
    # extract first four characters from entries in `year` column, so that "start--end" or "start-current" entries can be sorted along with entries where only one year is listed
    #dplyr::mutate(datum = stringr::str_sub(year, 1, 4)) %>%
    # sort newest to oldest
    #dplyr::arrange(desc(datum))
    
    # for line indentation: > (4 spaces don't work)
    # for line breaks: <br> (/ doesn't work)
    # must add open line between <br> and >
    # \n (must be 3 of them) resets indentation for new entry
  #  section_data %>%
   # glue::glue_data(
    #  " > {title}<br>
      
     # > > {ifelse(!is.na(year), paste0(year, ': ', institution_bullets), institution_bullets)}
      #<br>
      #\n\n\n
      #"
  #  ) %>%
   # print()
#  invisible(cv)
#}
