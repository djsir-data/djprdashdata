#' Function to create a lookup table of cross-tabs in the ABS LFS
#' @param abs_6202_raw Dataframe containing all time series tables from ABS 6202 (Labour Force)
#' @param abs_6291_raw Dataframe containing all time series tables from ABS 6291.0.55.001 (Labour Force, Detailed)
#' @param lfs_pivots Dataframe containing tidied pivot tables
#' @return Tibble with one row per unique ABS LFS series

create_lfs_lookup <- function(abs_6202_raw,
                              abs_6291_raw,
                              lfs_pivots = get_tidy_lfs_pivots()) {
  abs_6202_raw <- abs_6202_raw %>%
    dplyr::mutate(cat_no <- "6202.0")

  abs_6291_raw <- abs_6291_raw %>%
    dplyr::mutate(cat_no = "6291.0.55.001")

  abs_lfs <- abs_6291_raw %>%
    dplyr::filter(!.data$series_id %in% abs_6202_raw$series_id) %>%
    dplyr::bind_rows(abs_6202_raw) %>%
    dplyr::filter(.data$series_id != "")

  abs_lfs <- abs_lfs %>%
    dplyr::group_by(.data$series_id) %>%
    # We only need one observation per series
    dplyr::filter(
      date == max(.data$date),
      # Where a series appears multiple times with different series descriptions,
      # keep the one with the longest description
      nchar(.data$series) == max(nchar(.data$series))
    ) %>%
    # Where a Series ID appears in multiple tables, keep only the first
    dplyr::filter(.data$table_no == min(.data$table_no)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$cat_no,
      dplyr::starts_with("table"), dplyr::starts_with("series")
    )

  abs_lfs <- abs_lfs %>%
    split(abs_lfs$series_id) %>%
    purrr::map_dfr(~ suppressWarnings(readabs::separate_series(.x)))

  lfs_long <- abs_lfs %>%
    tidyr::pivot_longer(cols = c(
      dplyr::starts_with("series_"),
      -.data$series_id, -.data$series_type
    )) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::arrange(.data$table_no)

  age_values <- c(
    "20-24 years",
    "40-44 years",
    "45-54 years",
    "35-44 years",
    "55-64 years",
    "65 years and over",
    "15-24 years",
    "15-64 years",
    "25-29 years",
    "35-39 years",
    "25-34 years",
    "30-34 years",
    "50-54 years",
    "45-49 years",
    "15-19 years",
    "60-64 years",
    "20-24 years",
    "45-54 years",
    "40-44 years",
    "35-44 years",
    "55-64 years",
    "55-59 years",
    "55 years and over",
    "20-29 years",
    "20-64 years"
  )

  indicator_values <- c(
    "Employed total",
    "Unemployed total",
    "Labour force total",
    "Not in the labour force (NILF)",
    "Civilian population aged 15 years and over",
    "Civilian population aged 15-24 years",
    "Unemployment rate",
    "Participation rate",
    "Employment to population ratio",
    "Employed to population ratio",
    "Employed full-time",
    "Employed part-time",
    "Unemployed looked for full-time work",
    "Unemployed looked for only part-time work",
    "Unemployed looked for both full-time and part-time work",
    "Unemployment rate looked for full-time work",
    "Unemployed looked for only full-time work",
    "Unemployment rate looked for only part-time work",
    "Unemployment rate looked for only part time work",
    "Average duration of job search",
    "Hours actually worked in all jobs per employed person",
    "Long-term unemployed",
    "Long-term unemployment ratio",
    "Median duration of job search",
    "Number of hours actually worked in all jobs",
    "Number of hours usually worked in all jobs",
    "Number of weeks searching for job",
    "Unemployed",
    "Weekly hours usually worked in all jobs per employed person",
    "Civilian population aged 15-19 years",
    "Civilian population aged 15-64 years",
    "Monthly hours worked in all jobs",
    "Monthly hours worked in all jobs (employed full-time)",
    "Monthly hours worked in all jobs (employed part-time)",
    "Quarterly hours worked in all jobs",
    "Underemployed total",
    "Underemployment ratio (proportion of employed)",
    "Underemployment rate (proportion of labour force)",
    "Underutilisation rate",
    "Underemployed full-time (worked part-time for economic reasons)",
    "Underemployed part-time (prefer more hours)",
    "Underemployed full-time (expanded analytical series)",
    "Underemployed part-time (expanded analytical series)",
    "Underemployed full-time (prefer more hours)",
    "Underemployed part-time (worked less than usual hours for economic reasons)",
    "Underemployed total (expanded analytical series)",
    "Civilian population aged 15-29 years",
    "Civilian population aged 20-64 years",
    "Number of actual hours worked in all jobs",
    "Hours actually worked in all jobs per capita",
    "Number of hours sought by unemployed",
    "Number of additional hours preferred or hours not worked by underemployed",
    "Number of potential hours in the labour force",
    "Volume measure of unemployment rate",
    "Volume measure of underemployment rate",
    "Volume measure of underutilisation rate",
    "Engaged in employment and/or study",
    "Fully engaged in employment and/or study",
    "Partially engaged in employment and/or study",
    "Not engaged in either employment or study",
    "Engagement rate",
    "Fully engaged rate",
    "Retrenched (in previous quarter)",
    "Employed total (previous quarter)",
    "Retrenchment rate"
  )



  duration_of_unemp_values <- c(
    "104 weeks and over (2 years and over)",
    "13 weeks and under 26 weeks (3-6 months)",
    "13 weeks and under 52 weeks (3-12 months)",
    "26 weeks and under 52 weeks (6-12 months)",
    "4 weeks and under 13 weeks (1-3 months)",
    "52 weeks and over (1 year and over)",
    "52 weeks and over (Long-term unemployed)",
    "52 weeks and under 104 weeks (1-2 years)",
    "Under 13 weeks (under 3 months)",
    "Under 4 weeks (under 1 month)",
    "Under 52 weeks (under 1 year)",
    "Under 52 weeks (under 12 months)"
  )

  sa4_values <- c(
    "Adelaide - Central and Hills",
    "Adelaide - North",
    "Adelaide - South",
    "Adelaide - West",
    "Ballarat",
    "Barossa - Yorke - Mid North",
    "Bendigo",
    "Brisbane - East",
    "Brisbane - North",
    "Brisbane - South",
    "Brisbane - West",
    "Brisbane Inner City",
    "Bunbury",
    "Cairns",
    "Capital Region",
    "Central Coast",
    "Central Queensland",
    "Coffs Harbour - Grafton",
    "Darling Downs - Maranoa",
    "Darwin",
    "Far West and Orana",
    "Geelong",
    "Gold Coast",
    "Hume",
    "Hunter Valley exc Newcastle",
    "Illawarra",
    "Ipswich",
    "Latrobe - Gippsland",
    "Launceston and North East",
    "Logan - Beaudesert",
    "Mackay - Isaac - Whitsunday",
    "Mandurah",
    "Melbourne - Inner",
    "Melbourne - Inner East",
    "Melbourne - Inner South",
    "Melbourne - North East",
    "Melbourne - North West",
    "Melbourne - Outer East",
    "Melbourne - South East",
    "Melbourne - West",
    "Mid North Coast",
    "Moreton Bay - North",
    "Moreton Bay - South",
    "Mornington Peninsula",
    "Murray",
    "New England and North West",
    "New South Wales - Central West",
    "Newcastle and Lake Macquarie",
    "Northern Territory - Outback",
    "Perth - Inner",
    "Perth - North East",
    "Perth - North West",
    "Perth - South East",
    "Perth - South West",
    "Queensland - Outback",
    "Richmond - Tweed",
    "Riverina",
    "Shepparton",
    "South Australia - Outback",
    "South Australia - South East",
    "Southern Highlands and Shoalhaven",
    "Sunshine Coast",
    "Sydney - Baulkham Hills and Hawkesbury",
    "Sydney - Blacktown",
    "Sydney - City and Inner South",
    "Sydney - Eastern Suburbs",
    "Sydney - Inner South West",
    "Sydney - Inner West",
    "Sydney - North Sydney and Hornsby",
    "Sydney - Northern Beaches",
    "Sydney - Outer South West",
    "Sydney - Outer West and Blue Mountains",
    "Sydney - Parramatta",
    "Sydney - Ryde",
    "Sydney - South West",
    "Sydney - Sutherland",
    "Tasmania - South East",
    "Tasmania - West and North West",
    "Toowoomba",
    "Townsville",
    "Victoria - North West",
    "Warrnambool and South West",
    "Western Australia - Outback (North and South)",
    "Western Australia - Wheat Belt",
    "Wide Bay"
  )


  status_in_emp_values <- c(
    "Contributing family worker",
    "Owner manager of incorporated enterprise with employees",
    "Owner manager of incorporated enterprise without employees",
    "Owner manager of unincorporated enterprise with employees",
    "Owner manager of unincorporated enterprise without employees",
    "Employee",
    "Owner manager of enterprise without employees",
    "Employee with paid leave entitlements",
    "Employee without paid leave entitlements",
    "Owner manager",
    "Owner manager of enterprise with employees"
  )


  relship_in_hhold_values <- c(
    "Dependent student",
    "Family member",
    "Husband, wife or partner",
    "Lone parent",
    "Non-dependent child",
    "Not living alone",
    "Other related individual",
    "Relationship not determined",
    "Visitor",
    "Lone person",
    "Non-family member"
  )

  education_attendance_values <- c(
    "Attending full-time education",
    "Not attending full-time education",
    "Attending tertiary educational institution full-time",
    "Attending school (aged 15-19 years)",
    "Attending educational institution",
    "Attending tertiary educational institution part-time",
    "Not attending educational institution"
  )

  state_values <- c(
    "Australian Capital Territory",
    "New South Wales",
    "Northern Territory",
    "Queensland",
    "South Australia",
    "Tasmania",
    "Victoria",
    "Western Australia"
  )

  hours_values <- c(
    "Did not work (0 hours)",
    "Usually doesn't work (0 hours)",
    "Usually works 1 hour or more",
    "Usually works 35 hours or more",
    "Usually works fewer than 35 hours",
    "Worked 1 hour or more",
    "Worked 35 hours or more",
    "Worked fewer than 35 hours",
    "1-9 hours",
    "10-19 hours",
    "20-29 hours",
    "30-34 hours",
    "35-39 hours",
    "40-44 hours",
    "45-49 hours",
    "50-59 hours",
    "60-69 hours",
    "70 hours or more"
  )

  sex_values <- c(
    "Females",
    "Males",
    "Persons"
  )

  gcc_restofstate_values <- c(
    "Greater Adelaide",
    "Greater Brisbane",
    "Greater Hobart",
    "Greater Melbourne",
    "Greater Perth",
    "Greater Sydney",
    "Rest of NSW",
    "Rest of Qld",
    "Rest of SA",
    "Rest of Tas.",
    "Rest of Vic.",
    "Rest of WA"
  )

  dependents_values <- c(
    "With children under 15",
    "With dependants",
    "With no children under 15 and with dependent students",
    "With non-dependent children only",
    "Without children",
    "Without dependants"
  )

  marital_status_values <- c(
    "Married",
    "Not married"
  )

  industry_values <- c(
    "Agriculture, Forestry and Fishing",
    "Education and Training",
    "Accommodation and Food Services",
    "Administrative and Support Services",
    "Agriculture, Forestry and Fishing",
    "Arts and Recreation Services",
    "Construction",
    "Electricity, Gas, Water and Waste Services",
    "Financial and Insurance Services",
    "Health Care and Social Assistance",
    "Information Media and Telecommunications",
    "Manufacturing",
    "Mining",
    "Other Services",
    "Professional, Scientific and Technical Services",
    "Public Administration and Safety",
    "Rental, Hiring and Real Estate Services",
    "Retail Trade",
    "Transport, Postal and Warehousing",
    "Wholesale Trade"
  )


  industr_subdiv_values <- c(
    "Accommodation",
    "Accommodation and Food Services nfd",
    "Administrative and Support Services nfd",
    "Administrative Services",
    "Adult, Community and Other Education",
    "Agriculture",
    "Agriculture, Forestry and Fishing nfd",
    "Agriculture, Forestry and Fishing Support Services",
    "Air and Space Transport",
    "Aquaculture",
    "Arts and Recreation Services nfd",
    "Auxiliary Finance and Insurance Services",
    "Basic Chemical and Chemical Product Manufacturing",
    "Basic Material Wholesaling",
    "Beverage and Tobacco Product Manufacturing",
    "Broadcasting (except Internet)",
    "Building Cleaning, Pest Control and Other Support Services",
    "Building Construction",
    "Coal Mining",
    "Commission-Based Wholesaling",
    "Computer System Design and Related Services",
    "Construction",
    "Construction nfd",
    "Construction Services",
    "Creative and Performing Arts Activities",
    "Defence",
    "Education and Training nfd",
    "Electricity Supply",
    "Electricity, Gas, Water and Waste Services",
    "Electricity, Gas, Water and Waste Services nfd",
    "Exploration and Other Mining Support Services",
    "Fabricated Metal Product Manufacturing",
    "Finance",
    "Financial and Insurance Services",
    "Financial and Insurance Services nfd",
    "Fishing, Hunting and Trapping",
    "Food and Beverage Services",
    "Food Product Manufacturing",
    "Food Retailing",
    "Forestry and Logging",
    "Fuel Retailing",
    "Furniture and Other Manufacturing",
    "Gambling Activities",
    "Gas Supply",
    "Grocery, Liquor and Tobacco Product Wholesaling",
    "Health Care and Social Assistance",
    "Health Care and Social Assistance nfd",
    "Heavy and Civil Engineering Construction",
    "Heritage Activities",
    "Hospitals",
    "Information Media and Telecommunications",
    "Information Media and Telecommunications nfd",
    "Insurance and Superannuation Funds",
    "Internet Publishing and Broadcasting",
    "Internet Service Providers, Web Search Portals and Data Processing Services",
    "Library and Other Information Services",
    "Machinery and Equipment Manufacturing",
    "Machinery and Equipment Wholesaling",
    "Manufacturing",
    "Manufacturing nfd",
    "Medical and Other Health Care Services",
    "Metal Ore Mining",
    "Mining",
    "Mining nfd",
    "Motion Picture and Sound Recording Activities",
    "Motor Vehicle and Motor Vehicle Parts Retailing",
    "Motor Vehicle and Motor Vehicle Parts Wholesaling",
    "Non-Metallic Mineral Mining and Quarrying",
    "Non-Metallic Mineral Product Manufacturing",
    "Non-Store Retailing and Retail Commission-Based Buying and/or Selling",
    "Oil and Gas Extraction",
    "Other Goods Wholesaling",
    "Other Services",
    "Other Services nfd",
    "Other Store-Based Retailing",
    "Other Transport",
    "Personal and Other Services",
    "Petroleum and Coal Product Manufacturing",
    "Polymer Product and Rubber Product Manufacturing",
    "Postal and Courier Pick-up and Delivery Services",
    "Preschool and School Education",
    "Primary Metal and Metal Product Manufacturing",
    "Printing (including the Reproduction of Recorded Media)",
    "Private Households Employing Staff and Undifferentiated Goods- and Service-Producing Activities of Households for Own Use",
    "Professional, Scientific and Technical Services",
    "Professional, Scientific and Technical Services (Except Computer System Design and Related Services)",
    "Professional, Scientific and Technical Services nfd",
    "Property Operators and Real Estate Services",
    "Public Administration",
    "Public Administration and Safety",
    "Public Administration and Safety nfd",
    "Public Order, Safety and Regulatory Services",
    "Publishing (except Internet and Music Publishing)",
    "Pulp, Paper and Converted Paper Product Manufacturing",
    "Rail Transport",
    "Rental and Hiring Services (except Real Estate)",
    "Rental, Hiring and Real Estate Services",
    "Rental, Hiring and Real Estate Services nfd",
    "Repair and Maintenance",
    "Residential Care Services",
    "Retail Trade",
    "Retail Trade nfd",
    "Road Transport",
    "Social Assistance Services",
    "Sports and Recreation Activities",
    "Telecommunications Services",
    "Tertiary Education",
    "Textile, Leather, Clothing and Footwear Manufacturing",
    "Transport Equipment Manufacturing",
    "Transport Support Services",
    "Transport, Postal and Warehousing",
    "Transport, Postal and Warehousing nfd",
    "Warehousing and Storage Services",
    "Waste Collection, Treatment and Disposal Services",
    "Water Supply, Sewerage and Drainage Services",
    "Water Transport",
    "Wholesale Trade",
    "Wholesale Trade nfd",
    "Wood Product Manufacturing"
  )

  market_nonmarket_values <- c(
    "Market",
    "Rest of Non-market",
    "Rest of Market",
    "Non-market"
  )

  highest_ed <- c(
    "Advanced Diploma/Diploma",
    "Bachelor Degree",
    "Below Year 10/No educational attainment",
    "Certificate III/IV",
    "Graduate Diploma/Graduate Certificate",
    "Year 11 or equivalent",
    "Postgraduate Degree",
    "Year 10 or equivalent",
    "Year 12 or equivalent"
  )

  exp_future_emp_values <- c(
    "Changing jobs or seeking other employment",
    "Retiring",
    "Seasonal, temporary, fixed contract or casual job",
    "Expects to be with current employer or business in 12 months",
    "Employer or own business closing down or downsizing",
    "Does not expect to be with current employer or business in 12 months",
    "Expects to finish work for other reasons",
    "Returning to study, travelling, or family reasons"
  )


  occupation_values <- c(
    "Clerical and Administrative Workers",
    "Community and Personal Service Workers",
    "Labourers",
    "Machinery Operators and Drivers",
    "Managers",
    "Professionals",
    "Sales Workers",
    "Technicians and Trades Workers"
  )

  duration_with_emp_values <- c(
    "With current employer or business for 12 months or more",
    "With current employer or business for fewer than 12 months"
  )

  sector_values <- c(
    "Public sector",
    "Private sector"
  )

  current_lfs_values <- c(
    "Currently employed",
    "Currently unemployed",
    "Currently not in the labour force (NILF)"
  )

  lfs_lookup <- lfs_long %>%
    dplyr::mutate(category = dplyr::case_when(
      .data$value %in% age_values ~ "age",
      .data$value %in% indicator_values ~ "indicator",
      .data$value %in% duration_of_unemp_values ~ "duration_of_unemp",
      .data$value %in% sa4_values ~ "sa4",
      .data$value %in% status_in_emp_values ~ "status_in_emp",
      .data$value %in% marital_status_values ~ "marital_status",
      .data$value %in% relship_in_hhold_values ~ "relship_in_hhold",
      .data$value %in% education_attendance_values ~ "education_attendance",
      .data$value %in% state_values ~ "state",
      .data$value %in% hours_values ~ "hours",
      .data$value %in% sex_values ~ "sex",
      .data$value %in% gcc_restofstate_values ~ "gcc_restofstate",
      .data$value %in% dependents_values ~ "dependents",
      .data$value %in% industry_values ~ "industry",
      .data$value %in% industr_subdiv_values ~ "industry_subdiv",
      .data$value %in% market_nonmarket_values ~ "market_nonmarket",
      .data$value %in% highest_ed ~ "highest_ed",
      .data$value %in% exp_future_emp_values ~ "exp_future_emp",
      .data$value %in% occupation_values ~ "occupation",
      .data$value %in% duration_with_emp_values ~ "duration_with_employer",
      .data$value %in% sector_values ~ "sector",
      .data$value %in% current_lfs_values ~ "current_lfs",
      TRUE ~ NA_character_
    ))


  lfs_lookup <- lfs_lookup %>%
    dplyr::filter(!is.na(.data$category)) %>%
    dplyr::select(
      .data$cat_no,
      .data$table_no,
      .data$series,
      .data$series_id,
      .data$series_type,
      .data$value,
      .data$category
    ) %>%
    tidyr::spread(key = .data$category, value = .data$value) %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else(is.na(.x), "", .x)
    ))

  lfs_lookup <- lfs_lookup %>%
    dplyr::mutate(sex = dplyr::if_else(.data$sex == "Persons", "", .data$sex))

  # Add entries for data series that don't exist -----
  # and that are added using
  # add_missing_data() - eg. part time emp at state level = total - FT

  pt_emp <- data.frame(
    cat_no = "6202.0",
    table_no = "6202012",
    series = "Employed part-time ;  Persons ;  Victoria",
    series_id = "pt_emp_vic",
    series_type = "Seasonally Adjusted",
    indicator = "Employed part-time",
    state = "Victoria"
  )

  lfs_lookup <- dplyr::bind_rows(
    lfs_lookup,
    pt_emp
  )

  # Add entries for data cubes ----
  lfs_pivot <- lfs_pivots

  lfs_lookup <- lfs_pivot %>%
    dplyr::group_by(dplyr::across(!dplyr::one_of(c(
      "value",
      "date",
      "frequency",
      "unit",
      "data_type"
    )))) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(lfs_lookup)

  # Replace NAs with "" ----
  lfs_lookup <- lfs_lookup %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
      tidyr::replace_na,
      replace = ""
    ))
}
