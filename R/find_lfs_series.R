#' Look up the series ID for time series from the ABS Labour Force Survey
#' @return A character vector of ABS Time Series ID(s) that match your query
#' @param indicator the indicator of interest, such as "unemployment rate". See
#' `Details` for possible indicators.
#' @param series_type Default is "seasonally adjusted"; alternatives is
#' "trend" and "original".
#' @param state Default is "", which means total / not specified. See `Details` for possible values.
#' @param sex Default is "", which means total / not specified. See `Details` for possible values.
#' @param age Default is "", which means total / not specified. See `Details` for possible values.
#' @param industry Default is "", which means total / not specified. See `Details` for possible values.
#' @param industry_subdiv Default is "", which means total / not specified. See `Details` for possible values.
#' @param occupation Default is "", which means total / not specified. See `Details` for possible values.
#' @param sa4 Default is "", which means total / not specified. See `Details` for possible values.
#' @param sector Default is "", which means total / not specified. See `Details` for possible values.
#' @param gcc_restofstate Default is "", which means total / not specified. See `Details` for possible values.
#' @param duration_of_unemp Default is "", which means total / not specified. See `Details` for possible values.
#' @param education_attendance Default is "", which means total / not specified. See `Details` for possible values.
#' @param highest_ed Default is "", which means total / not specified. See `Details` for possible values.
#' @param status_in_emp Default is "", which means total / not specified. See `Details` for possible values.
#' @param market_nonmarket Default is "", which means total / not specified. See `Details` for possible values.
#' @param hours Default is "", which means total / not specified. See `Details` for possible values.
#' @param duration_with_employer Default is "", which means total / not specified. See `Details` for possible values.
#' @param exp_future_emp Default is "", which means total / not specified. See `Details` for possible values.
#' @param current_lfs Default is "", which means total / not specified. See `Details` for possible values.
#' @param marital_status Default is "", which means total / not specified. See `Details` for possible values.
#' @param relship_in_hhold Default is "", which means total / not specified. See `Details` for possible values.
#' @param dependents Default is "", which means total / not specified. See `Details` for possible values.
#' @details
#' Arguments can be supplied with either upper or lower case. They
#' must otherwise identically match possible values for the category.
#'
#' `find_lfs_series()` is strict - it will only return time series ID(s) that
#' match the supplied arguments. To browse Labour Force series in a
#' more flexible, fuzzy-matched way, see \code{?browse_lfs_series()}.
#'
#' Possible values for arguments:
#' \itemize{
#' "indicator"
#'   \item{"employed full-time"}
#'   \item{"employed part-time"}
#'   \item{"employed total"}
#'   \item{"unemployed looked for full-time work"}
#'   \item{"unemployed looked for only part-time work"}
#'   \item{"unemployed total"}
#'   \item{"labour force total"}
#'   \item{"unemployment rate"}
#'   \item{"participation rate"}
#'   \item{"unemployment rate looked for full-time work"}
#'   \item{"unemployment rate looked for only part-time work"}
#'   \item{"employment to population ratio"}
#'   \item{"not in the labour force (nilf)"}
#'   \item{"civilian population aged 15 years and over"}
#'   \item{"civilian population aged 15-24 years"}
#'   \item{"civilian population aged 15-19 years"}
#'   \item{"civilian population aged 15-64 years"}
#'   \item{"monthly hours worked in all jobs"}
#'   \item{"monthly hours worked in all jobs (employed full-time)"}
#'   \item{"monthly hours worked in all jobs (employed part-time)"}
#'   \item{"quarterly hours worked in all jobs"}
#'   \item{"underemployed total"}
#'   \item{"underemployment ratio (proportion of employed)"}
#'   \item{"underemployment rate (proportion of labour force)"}
#'   \item{"underutilisation rate"}
#'   \item{"underemployed full-time (worked part-time for economic reasons)"}
#'   \item{"underemployed part-time (prefer more hours)"}
#'   \item{"underemployed full-time (expanded analytical series)"}
#'   \item{"underemployed part-time (expanded analytical series)"}
#'   \item{"underemployed full-time (prefer more hours)"}
#'   \item{"underemployed part-time (worked less than usual hours for economic reasons)"}
#'   \item{"underemployed total (expanded analytical series)"}
#'   \item{"unemployment rate looked for only part time work"}
#'   \item{"number of hours actually worked in all jobs"}
#'   \item{"hours actually worked in all jobs per employed person"}
#'   \item{"number of hours usually worked in all jobs"}
#'   \item{"weekly hours usually worked in all jobs per employed person"}
#'   \item{"number of actual hours worked in all jobs"}
#'   \item{"hours actually worked in all jobs per capita"}
#'   \item{"number of weeks searching for job"}
#'   \item{"average duration of job search"}
#'   \item{"median duration of job search"}
#'   \item{"long-term unemployed"}
#'   \item{"long-term unemployment ratio"}
#'   \item{"unemployed"}
#'   \item{"employed to population ratio"}
#'   \item{"unemployed looked for both full-time and part-time work"}
#'   \item{"unemployed looked for only full-time work"}
#'   \item{"number of hours sought by unemployed"}
#'   \item{"number of additional hours preferred or hours not worked by underemployed"}
#'   \item{"number of potential hours in the labour force"}
#'   \item{"volume measure of unemployment rate"}
#'   \item{"volume measure of underemployment rate"}
#'   \item{"volume measure of underutilisation rate"}
#'   \item{"civilian population aged 20-64 years"}
#'   \item{"engaged in employment and/or study"}
#'   \item{"fully engaged in employment and/or study"}
#'   \item{"partially engaged in employment and/or study"}
#'   \item{"not engaged in either employment or study"}
#'   \item{"engagement rate"}
#'   \item{"fully engaged rate"}
#'   \item{"civilian population aged 15-29 years"}
#'   \item{"retrenched (in previous quarter)"}
#'   \item{"employed total (previous quarter)"}
#'   \item{"retrenchment rate"}
#' }
#' \itemize{
#' "state"
#'   \item{""}
#'   \item{"new south wales"}
#'   \item{"queensland"}
#'   \item{"tasmania"}
#'   \item{"australian capital territory"}
#'   \item{"western australia"}
#'   \item{"northern territory"}
#'   \item{"south australia"}
#'   \item{"victoria"}
#' }
#'
#' \itemize{
#' "sex"
#'   \item{""}
#'   \item{"males"}
#'   \item{"females"}
#' }
#'
#' \itemize{
#' "age"
#'   \item{""}
#'   \item{"20-24 years"}
#'   \item{"45-54 years"}
#'   \item{"40-44 years"}
#'   \item{"55-64 years"}
#'   \item{"35-44 years"}
#'   \item{"65 years and over"}
#'   \item{"55-59 years"}
#'   \item{"15-24 years"}
#'   \item{"15-64 years"}
#'   \item{"25-29 years"}
#'   \item{"35-39 years"}
#'   \item{"25-34 years"}
#'   \item{"30-34 years"}
#'   \item{"50-54 years"}
#'   \item{"45-49 years"}
#'   \item{"15-19 years"}
#'   \item{"60-64 years"}
#'   \item{"20-29 years"}
#'   \item{"20-64 years"}
#'   \item{"55 years and over"}
#' }
#'
#' \itemize{
#' "industry"
#'   \item{""}
#'   \item{"agriculture, forestry and fishing"}
#'   \item{"education and training"}
#'   \item{"accommodation and food services"}
#'   \item{"arts and recreation services"}
#'   \item{"transport, postal and warehousing"}
#'   \item{"financial and insurance services"}
#'   \item{"professional, scientific and technical services"}
#'   \item{"health care and social assistance"}
#'   \item{"electricity, gas, water and waste services"}
#'   \item{"information media and telecommunications"}
#'   \item{"mining"}
#'   \item{"manufacturing"}
#'   \item{"administrative and support services"}
#'   \item{"public administration and safety"}
#'   \item{"retail trade"}
#'   \item{"rental, hiring and real estate services"}
#'   \item{"other services"}
#'   \item{"construction"}
#'   \item{"wholesale trade"}
#' }
#'
#' \itemize{
#' "industry_subdiv"
#'   \item{""}
#'   \item{"fishing, hunting and trapping"}
#'   \item{"metal ore mining"}
#'   \item{"non-metallic mineral mining and quarrying"}
#'   \item{"electricity supply"}
#'   \item{"food and beverage services"}
#'   \item{"rail transport"}
#'   \item{"motion picture and sound recording activities"}
#'   \item{"telecommunications services"}
#'   \item{"information media and telecommunications nfd"}
#'   \item{"building cleaning, pest control and other support services"}
#'   \item{"residential care services"}
#'   \item{"oil and gas extraction"}
#'   \item{"textile, leather, clothing and footwear manufacturing"}
#'   \item{"petroleum and coal product manufacturing"}
#'   \item{"water supply, sewerage and drainage services"}
#'   \item{"water transport"}
#'   \item{"other transport"}
#'   \item{"financial and insurance services nfd"}
#'   \item{"administrative services"}
#'   \item{"public administration"}
#'   \item{"education and training nfd"}
#'   \item{"social assistance services"}
#'   \item{"agriculture"}
#'   \item{"agriculture, forestry and fishing nfd"}
#'   \item{"food product manufacturing"}
#'   \item{"beverage and tobacco product manufacturing"}
#'   \item{"basic chemical and chemical product manufacturing"}
#'   \item{"primary metal and metal product manufacturing"}
#'   \item{"manufacturing nfd"}
#'   \item{"internet service providers, web search portals and data processing services"}
#'   \item{"rental and hiring services (except real estate)"}
#'   \item{"tertiary education"}
#'   \item{"adult, community and other education"}
#'   \item{"heritage activities"}
#'   \item{"repair and maintenance"}
#'   \item{"personal and other services"}
#'   \item{"printing (including the reproduction of recorded media)"}
#'   \item{"polymer product and rubber product manufacturing"}
#'   \item{"non-metallic mineral product manufacturing"}
#'   \item{"fabricated metal product manufacturing"}
#'   \item{"waste collection, treatment and disposal services"}
#'   \item{"heavy and civil engineering construction"}
#'   \item{"basic material wholesaling"}
#'   \item{"retail trade nfd"}
#'   \item{"accommodation"}
#'   \item{"air and space transport"}
#'   \item{"insurance and superannuation funds"}
#'   \item{"property operators and real estate services"}
#'   \item{"professional, scientific and technical services (except computer system design and related services)"}
#'   \item{"defence"}
#'   \item{"public order, safety and regulatory services"}
#'   \item{"arts and recreation services nfd"}
#'   \item{"aquaculture"}
#'   \item{"forestry and logging"}
#'   \item{"agriculture, forestry and fishing support services"}
#'   \item{"exploration and other mining support services"}
#'   \item{"furniture and other manufacturing"}
#'   \item{"construction services"}
#'   \item{"grocery, liquor and tobacco product wholesaling"}
#'   \item{"motor vehicle and motor vehicle parts retailing"}
#'   \item{"other store-based retailing"}
#'   \item{"non-store retailing and retail commission-based buying and/or selling"}
#'   \item{"postal and courier pick-up and delivery services"}
#'   \item{"publishing (except internet and music publishing)"}
#'   \item{"broadcasting (except internet)"}
#'   \item{"rental, hiring and real estate services nfd"}
#'   \item{"gambling activities"}
#'   \item{"other services nfd"}
#'   \item{"wood product manufacturing"}
#'   \item{"pulp, paper and converted paper product manufacturing"}
#'   \item{"construction nfd"}
#'   \item{"other goods wholesaling"}
#'   \item{"commission-based wholesaling"}
#'   \item{"wholesale trade nfd"}
#'   \item{"accommodation and food services nfd"}
#'   \item{"road transport"}
#'   \item{"transport support services"}
#'   \item{"professional, scientific and technical services nfd"}
#'   \item{"public administration and safety nfd"}
#'   \item{"preschool and school education"}
#'   \item{"hospitals"}
#'   \item{"medical and other health care services"}
#'   \item{"creative and performing arts activities"}
#'   \item{"sports and recreation activities"}
#'   \item{"coal mining"}
#'   \item{"mining nfd"}
#'   \item{"machinery and equipment manufacturing"}
#'   \item{"gas supply"}
#'   \item{"electricity, gas, water and waste services nfd"}
#'   \item{"fuel retailing"}
#'   \item{"food retailing"}
#'   \item{"warehousing and storage services"}
#'   \item{"transport, postal and warehousing nfd"}
#'   \item{"internet publishing and broadcasting"}
#'   \item{"library and other information services"}
#'   \item{"auxiliary finance and insurance services"}
#'   \item{"computer system design and related services"}
#'   \item{"administrative and support services nfd"}
#'   \item{"transport equipment manufacturing"}
#'   \item{"building construction"}
#'   \item{"machinery and equipment wholesaling"}
#'   \item{"motor vehicle and motor vehicle parts wholesaling"}
#'   \item{"finance"}
#'   \item{"health care and social assistance nfd"}
#'   \item{"private households employing staff and undifferentiated goods- and service-producing activities of households for own use"}
#' }
#' \itemize{
#' "occupation"
#'   \item{""}
#'   \item{"managers"}
#'   \item{"technicians and trades workers"}
#'   \item{"sales workers"}
#'   \item{"labourers"}
#'   \item{"clerical and administrative workers"}
#'   \item{"machinery operators and drivers"}
#'   \item{"professionals"}
#'   \item{"community and personal service workers"}
#' }
#' \itemize{
#' "sa4"
#'   \item{""}
#'   \item{"sydney - south west"}
#'   \item{"capital region"}
#'   \item{"melbourne - north east"}
#'   \item{"brisbane - east"}
#'   \item{"brisbane - north"}
#'   \item{"brisbane inner city"}
#'   \item{"townsville"}
#'   \item{"adelaide - central and hills"}
#'   \item{"perth - inner"}
#'   \item{"launceston and north east"}
#'   \item{"northern territory - outback"}
#'   \item{"central coast"}
#'   \item{"melbourne - inner"}
#'   \item{"ballarat"}
#'   \item{"geelong"}
#'   \item{"hume"}
#'   \item{"latrobe - gippsland"}
#'   \item{"adelaide - west"}
#'   \item{"perth - north east"}
#'   \item{"perth - south east"}
#'   \item{"tasmania - west and north west"}
#'   \item{"sydney - eastern suburbs"}
#'   \item{"sydney - northern beaches"}
#'   \item{"far west and orana"}
#'   \item{"murray"}
#'   \item{"mackay - isaac - whitsunday"}
#'   \item{"wide bay"}
#'   \item{"south australia - outback"}
#'   \item{"south australia - south east"}
#'   \item{"western australia - outback (north and south)"}
#'   \item{"sydney - outer south west"}
#'   \item{"new south wales - central west"}
#'   \item{"mid north coast"}
#'   \item{"new england and north west"}
#'   \item{"riverina"}
#'   \item{"melbourne - outer east"}
#'   \item{"logan - beaudesert"}
#'   \item{"moreton bay - north"}
#'   \item{"adelaide - north"}
#'   \item{"tasmania - south east"}
#'   \item{"sydney - baulkham hills and hawkesbury"}
#'   \item{"sydney - city and inner south"}
#'   \item{"melbourne - south east"}
#'   \item{"victoria - north west"}
#'   \item{"central queensland"}
#'   \item{"toowoomba"}
#'   \item{"perth - south west"}
#'   \item{"sydney - blacktown"}
#'   \item{"sydney - inner south west"}
#'   \item{"sydney - inner west"}
#'   \item{"sydney - north sydney and hornsby"}
#'   \item{"sydney - outer west and blue mountains"}
#'   \item{"sydney - ryde"}
#'   \item{"hunter valley exc newcastle"}
#'   \item{"richmond - tweed"}
#'   \item{"melbourne - inner east"}
#'   \item{"melbourne - west"}
#'   \item{"bendigo"}
#'   \item{"warrnambool and south west"}
#'   \item{"brisbane - south"}
#'   \item{"moreton bay - south"}
#'   \item{"darling downs - maranoa"}
#'   \item{"queensland - outback"}
#'   \item{"adelaide - south"}
#'   \item{"bunbury"}
#'   \item{"sydney - sutherland"}
#'   \item{"newcastle and lake macquarie"}
#'   \item{"southern highlands and shoalhaven"}
#'   \item{"melbourne - north west"}
#'   \item{"shepparton"}
#'   \item{"brisbane - west"}
#'   \item{"sunshine coast"}
#'   \item{"barossa - yorke - mid north"}
#'   \item{"sydney - parramatta"}
#'   \item{"coffs harbour - grafton"}
#'   \item{"illawarra"}
#'   \item{"melbourne - inner south"}
#'   \item{"mornington peninsula"}
#'   \item{"ipswich"}
#'   \item{"cairns"}
#'   \item{"gold coast"}
#'   \item{"mandurah"}
#'   \item{"perth - north west"}
#'   \item{"western australia - wheat belt"}
#'   \item{"darwin"}
#' }
#' \itemize{
#' "sector"
#'   \item{""}
#'   \item{"public sector"}
#'   \item{"private sector"}
#' }
#'
#' \itemize{
#' "gcc_restofstate"
#'   \item{""}
#'   \item{"greater sydney"}
#'   \item{"rest of nsw"}
#'   \item{"rest of tas."}
#'   \item{"rest of sa"}
#'   \item{"rest of qld"}
#'   \item{"greater hobart"}
#'   \item{"rest of vic."}
#'   \item{"greater melbourne"}
#'   \item{"greater brisbane"}
#'   \item{"greater adelaide"}
#'   \item{"greater perth"}
#'   \item{"rest of wa"}
#' }
#'
#' \itemize{
#' "duration_of_unemp"
#'   \item{""}
#'   \item{"52 weeks and over (long-term unemployed)"}
#'   \item{"under 4 weeks (under 1 month)"}
#'   \item{"4 weeks and under 13 weeks (1-3 months)"}
#'   \item{"52 weeks and under 104 weeks (1-2 years)"}
#'   \item{"104 weeks and over (2 years and over)"}
#'   \item{"13 weeks and under 26 weeks (3-6 months)"}
#'   \item{"26 weeks and under 52 weeks (6-12 months)"}
#'   \item{"under 52 weeks (under 12 months)"}
#'   \item{"52 weeks and over (1 year and over)"}
#'   \item{"under 52 weeks (under 1 year)"}
#'   \item{"13 weeks and under 52 weeks (3-12 months)"}
#'   \item{"under 13 weeks (under 3 months)"}
#' }
#'
#' \itemize{
#' "education_attendance"
#'   \item{""}
#'   \item{"attending tertiary educational institution full-time"}
#'   \item{"not attending full-time education"}
#'   \item{"attending full-time education"}
#'   \item{"attending school (aged 15-19 years)"}
#'   \item{"attending tertiary educational institution part-time"}
#'   \item{"attending educational institution"}
#'   \item{"not attending educational institution"}
#'
#' }
#'
#' \itemize{
#' "highest_ed"
#'   \item{""}
#'   \item{"below year 10/no educational attainment"}
#'   \item{"graduate diploma/graduate certificate"}
#'   \item{"bachelor degree"}
#'   \item{"advanced diploma/diploma"}
#'   \item{"certificate iii/iv"}
#'   \item{"year 11 or equivalent"}
#'   \item{"postgraduate degree"}
#'   \item{"year 10 or equivalent"}
#'   \item{"year 12 or equivalent"}
#' }
#'
#' \itemize{
#' "status_in_emp"
#'   \item{""}
#'   \item{"employee"}
#'   \item{"owner manager of incorporated enterprise with employees"}
#'   \item{"owner manager of incorporated enterprise without employees"}
#'   \item{"owner manager of unincorporated enterprise with employees"}
#'   \item{"owner manager of unincorporated enterprise without employees"}
#'   \item{"contributing family worker"}
#'   \item{"owner manager of enterprise without employees"}
#'   \item{"employee with paid leave entitlements"}
#'   \item{"employee without paid leave entitlements"}
#'   \item{"owner manager"}
#'   \item{"owner manager of enterprise with employees"}
#' }
#'
#' \itemize{
#' "market_nonmarket"
#'   \item{""}
#'   \item{"market"}
#'   \item{"rest of non-market"}
#'   \item{"rest of market"}
#'   \item{"non-market"}
#' }
#'
#' \itemize{
#' "hours"
#'   \item{""}
#'   \item{"1-9 hours"}
#'   \item{"20-29 hours"}
#'   \item{"worked 1 hour or more"}
#'   \item{"40-44 hours"}
#'   \item{"60-69 hours"}
#'   \item{"10-19 hours"}
#'   \item{"worked fewer than 35 hours"}
#'   \item{"35-39 hours"}
#'   \item{"50-59 hours"}
#'   \item{"did not work (0 hours)"}
#'   \item{"30-34 hours"}
#'   \item{"worked 35 hours or more"}
#'   \item{"45-49 hours"}
#'   \item{"70 hours or more"}
#'   \item{"usually works 1 hour or more"}
#'   \item{"usually works fewer than 35 hours"}
#'   \item{"usually doesn't work (0 hours)"}
#'   \item{"usually works 35 hours or more"}
#' }
#'
#' \itemize{
#' "duration_with_employer"
#'   \item{""}
#'   \item{"with current employer or business for 12 months or more"}
#'   \item{"with current employer or business for fewer than 12 months"}
#' }
#'
#' \itemize{
#' "exp_future_emp"
#'   \item{""}
#'   \item{"changing jobs or seeking other employment"}
#'   \item{"retiring"}
#'   \item{"seasonal, temporary, fixed contract or casual job"}
#'   \item{"expects to be with current employer or business in 12 months"}
#'   \item{"employer or own business closing down or downsizing"}
#'   \item{"does not expect to be with current employer or business in 12 months"}
#'   \item{"expects to finish work for other reasons"}
#'   \item{"returning to study, travelling, or family reasons"}
#'
#' }
#'
#' \itemize{
#' "current_lfs"
#'   \item{""}
#'   \item{"currently employed"}
#'   \item{"currently unemployed"}
#'   \item{"currently not in the labour force (nilf)"}
#'
#' }
#'
#' \itemize{
#' "marital_status"
#'   \item{""}
#'   \item{"married"}
#'   \item{"not married"}
#'
#' }
#'
#' \itemize{
#' "relship_in_hhold"
#'   \item{""}
#'   \item{"husband, wife or partner"}
#'   \item{"lone person"}
#'   \item{"lone parent"}
#'   \item{"other related individual"}
#'   \item{"not living alone"}
#'   \item{"family member"}
#'   \item{"relationship not determined"}
#'   \item{"non-family member"}
#'   \item{"non-dependent child"}
#'   \item{"visitor"}
#'   \item{"dependent student"}
#'
#' }
#'
#' \itemize{
#' "dependents"
#'   \item{""}
#'   \item{"with no children under 15 and with dependent students"}
#'   \item{"with children under 15"}
#'   \item{"with non-dependent children only"}
#'   \item{"with dependants"}
#'   \item{"without dependants"}
#'   \item{"without children"}
#' }
#'
#' @importFrom rlang .data .env
#' @export
#' @seealso browse_lfs_series

find_lfs_series <- function(indicator,
                             series_type = "seasonally adjusted",
                             state = "",
                             sex = "",
                             age = "",
                             industry = "",
                             industry_subdiv = "",
                             occupation = "",
                             sa4 = "",
                             sector = "",
                             gcc_restofstate = "",
                             duration_of_unemp = "",
                             education_attendance = "",
                             highest_ed = "",
                             status_in_emp = "",
                             market_nonmarket = "",
                             hours = "",
                             duration_with_employer = "",
                             exp_future_emp = "",
                             current_lfs = "",
                             marital_status = "",
                             relship_in_hhold = "",
                             dependents = "") {

  arg_list <- list(
    indicator,
    series_type,
    state,
    sex,
    age,
    industry,
    industry_subdiv,
    occupation,
    sa4,
    sector,
    gcc_restofstate,
    duration_of_unemp,
    education_attendance,
    highest_ed,
    status_in_emp,
    market_nonmarket,
    hours,
    duration_with_employer,
    exp_future_emp,
    current_lfs,
    marital_status,
    relship_in_hhold,
    dependents
  )

  arg_vec <- c(
    "indicator",
    "series_type",
    "state",
    "sex",
    "age",
    "industry",
    "industry_subdiv",
    "occupation",
    "sa4",
    "sector",
    "gcc_restofstate",
    "duration_of_unemp",
    "education_attendance",
    "highest_ed",
    "status_in_emp",
    "market_nonmarket",
    "hours",
    "duration_with_employer",
    "exp_future_emp",
    "current_lfs",
    "marital_status",
    "relship_in_hhold",
    "dependents"
  )

  arg_list <- purrr::map(arg_list, tolower)
  # arg_list <- purrr::map(arg_list, ~ match.arg(.x))
  arg_list <- purrr::set_names(
    arg_list,
    arg_vec
  )

  # Ignore case when testing for equality
  is_equalish <- function(x, y) {
    x <- tolower(x)
    y <- tolower(y)
    x %in% y
    # To make it less strict?
    # grepl(y, x)
  }

  lfs_lookup %>%
    dplyr::filter(dplyr::across(
      .cols = dplyr::all_of(arg_vec),
      .fns = ~ is_equalish(.x, arg_list[[dplyr::cur_column()]])
    )) %>%
    dplyr::pull(.data$series_id)
}
