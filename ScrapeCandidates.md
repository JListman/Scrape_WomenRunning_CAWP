ScrapeCandidates
================
Jenny Listman
1/26/2018

#### Interactive App To Visualize Women Runnning for State or Federal Offices

This project was created with data scraped from the website of the [Center for American Women in Politics](http://www.cawp.rutgers.edu) at [Rutgers University](https://www.rutgers.edu). The Center conducts research on women's participation in politics in America and maintains [a list](http://cawp.rutgers.edu/buzz-2018-potential-women-candidates-us-congress-and-statewide-elected-executive) of women who are or potentially will be running for US Congress and State offices.

The code scrapes and combines data from their lists of women candidates, creates new variables, and visualizes the data in an interactive `shiny` map and searchable data table using `plotly` and `DT` packages . **The [CAWP](http://www.cawp.rutgers.edu) states that their products or data are available for non-for-profit distribution as long as they are given credit (we are allowed to scrape their data table - thank you CAWP).**

Full `shiny` app can be found in [this github repo](https://github.com/JListman/Scrape_WomenRunning_CAWP).

Load packages needed:

``` r
library(rvest)
library(tidyverse)
library(janitor)
library(shiny)
library(plotly)
library(filesstrings)
library(zoo)
library(ggpubr)
library(DT)
```

To turn the website into data:
1. Specify the url for website with tables to be scraped.
2. Read HTML with `xml2::read_html`.
3. Get HTML nodes with `rvest::html_nodes` and
4. parse table contents into dataframes with `rvest::html_table`.

``` r
url <- 'http://cawp.rutgers.edu/buzz-2018-potential-women-candidates-us-congress-and-statewide-elected-executive'

webpage <- read_html(url)

candidate_tables <- webpage %>%
        html_nodes("table") %>%
        html_table(fill = TRUE)
```

Viewing the tables we've got a list with 2 elements corresponding to 2 of the 3 tables on the website. Looking at the website, we can see that candidate\_tables\[\[1\]\] corresponds to special elections and candidate\_tables\[\[2\]\] corresponds to general elections. *This website has changed its format recently, so the number of tables has to be varified before updating app.*

``` r
glimpse(candidate_tables[[1]])
```

    ## Observations: 10
    ## Variables: 8
    ## $ State                    <chr> "AZ", "", "", "", "", "OH", "", "", "...
    ## $ Office                   <chr> "", "U.S. Rep.", "U.S. Rep.", "U.S. R...
    ## $ Dist.                    <int> NA, 8, 8, 8, NA, NA, 12, 12, 12, 12
    ## $ `Candidate Name & Party` <chr> "", "Brianna Westbrook (D)", "Hiral V...
    ## $ `Seat Status`            <chr> "", "O", "O", "O", "", "", "O", "O", ...
    ## $ `Filing\n\t\t\tDate`     <chr> "", "Filed", "Filed", "Filed", "", ""...
    ## $ `Primary\n\t\t\tDate`    <chr> "", "2/27/2018", "2/27/2018", "2/27/2...
    ## $ `Election\n\t\t\tDate`   <chr> "", "4/24/2018", "4/24/2018", "4/24/2...

``` r
glimpse(candidate_tables[[2]])
```

    ## Observations: 724
    ## Variables: 8
    ## $ State                    <chr> "AK", "", "", "", "AL", "", "", "", "...
    ## $ Office                   <chr> "", "Lt. Gov.", "Lt. Gov.", "", "", "...
    ## $ Dist.                    <chr> "", "", "", "", "", "", "", "", "", "...
    ## $ `Candidate Name & Party` <chr> "", "Lynn Gattis (R)", "Edie Grunwald...
    ## $ `General\n\t\t\tSeat`    <chr> "", "C", "C", "", "", "I", "C", "O", ...
    ## $ `Filing\n\t\t\tDate`     <chr> "", "6/1/2018", "6/1/2018", "", "", "...
    ## $ `Primary\n\t\t\tDate`    <chr> "", "8/21/2018", "8/21/2018", "", "",...
    ## $ `Election\n\t\t\tDate`   <chr> "", "11/6/2018", "11/6/2018", "", "",...

The special elections and general elections dataframes are formatted differently, so must be made compatible before combining. Change Dist. variable from numeric in the special elections table to character to match the general elections table. Use `janitor::clean_names` to remove spaces and special characters from variable names.

``` r
special_elec <- candidate_tables[[1]] %>%
        mutate(Dist. = as.character(Dist.)) %>% 
        mutate(Dist. = replace(Dist., Dist. == "8", "08")) %>% 
        clean_names() 
```

Combine the special elections and general elections dataframes and then clean things up. First rename one variable that has different names in each of these dataframes.

On the [CAWP website database](http://cawp.rutgers.edu/buzz-2018-potential-women-candidates-us-congress-and-statewide-elected-executive), state abbreviation appears once, one row above all entries for a given state. Spaces below it are blank, until the next state. Use `zoo::na.locf` to replace blanks below a state abbreviation with the abbreviation above it and when a new state abbreviation starts, the next set of blanks are replaced with that state, etc. In order for this function to work, the blanks first need to be changed to `NA`.

Remove special characters such as \* with `filesstrings::trim_anything` and remove all blank rows.

``` r
cols <- c(1,5)

elections <- candidate_tables[[2]] %>%
        clean_names() %>%
        rename(seat_status = general_seat) %>%
        bind_rows(special_elec) %>%
        mutate(state = replace(state, state == "", NA)) %>%
        transform(state = na.locf(state)) %>%
        subset(office != "") %>% 
        mutate(office = trim_anything(office, "*", "right")) %>%
        mutate_each(funs(factor(.)),cols)
```

Rename offices from abbreviations to full names. Some are not self-explanatory in their current state. In addition, some offices have more than one name/spelling/punctuation formatting (Comp, Comp., Comtr.) or have been misspelled.

``` r
elections$office <- fct_recode(elections$office,
                   `Commissioner of Agriculture` = "Agriculture", `Commissioner of Agriculture` = "Comm. Agri.",
                   `Attorney General` = "At. Gen.",`Attorney General`= "Atty. Gen.",
                   Auditor =  "Auditor",
                   `Chief Financial Officer` = "CFO",  
                   Comptroller = "Comp", Comptroller = "Comp.", Comptroller = "Comptr.",
                   Governor = "Govenor", Governor = "Governor",
                   `Insurance Commissioner` = "Insurance Comm.",
                   `Land Commissioner` = "Land",`Land Commissioner` = "Land Comm.",
                   `Lieutenant Governor` = "Lt. Gov.",
                   `Railroad Commissioner` = "Rail. Comm.",
                   `Superintendent of Public Instruction` = "S.P.I.",
                   `Secretary of State` = "Sec. St.",
                   `State Treasurer` = "St. Treas.",
                   `US Delegate` = "U.S. Del", `US Delegate` = "U.S. Del.",
                   `US Congressional Representative` = "U.S. Rep", `US Congressional Representative` = "U.S. Rep.",
                   `US Congressional Senator` = "U.S. Sen.")     
```

Candidate name and party affiliation are a single character variable. Separate these into two variables using `filestrings::str_elem` and `base::trimws`.

Some candidates in Minnesota have party listed as DFL. According to [Wikkepedia](https://en.wikipedia.org/wiki/Minnesota_Democratic–Farmer–Labor_Party): Minnesota Democratic–Farmer–Labor Party (DFL) is a social liberal political party in Minnesota affiliated with the Democratic Party. Recode these candidates as Democrats.

Territories that elect US Delegates and states with a small population that have one Federal Representative are listed as having district `AL`, to mean At-Large. Change district `AL` to `At-Large`. State-level offices have blanks for district variable. Change these to `State`.

``` r
elections <- elections %>%
        mutate(party = str_elem(candidate_name_party, -2)) %>%
        mutate(party = replace(party, party == "L", "D")) %>% 
        mutate(party = as.factor(party)) %>%
        mutate(candidate = trimws(str_before_first(candidate_name_party,"\\("), "right")) %>%
        mutate(dist = replace(dist, dist == "", "State")) %>% 
        mutate(dist = replace(dist, dist == "AL", "At-Large")) %>%
        mutate(dist = as.factor(dist)) %>%
        droplevels()
```

Make new variables for mapping and other possible visualizations: \# of women running in each state, \# and % running as Republican, \# and % running as Democrat, \# running for a given office.

Some states or territories have no women running at this time. Combine dataframe with a list of all state names and abbreviations to add missing states as rows with values `NA`. Remove states with no state abbreviation, since those are territories that won't show up on the `statebins` - style map. Capitalize variable names that will be included in the `datatable` tab of the `shiny` app.

``` r
statenames <- read.csv("StateNamesAbbrevPostalCode.csv")

elections <- elections %>%
        add_count(state) %>%
        rename("Women Running" = n) %>%
        add_count(state, office) %>%
        rename(officeperstate = n) %>% 
        add_count(state, party) %>%
        rename(partyperstate = n) %>%
        add_count(state, office, dist) %>%
        rename(officeperdist = n) %>%
        merge(statenames, by.x = "state", by.y = "Postal.Code", all = TRUE) %>%
        mutate(percentparty = round(100*(partyperstate/`Women Running`), digits = 0)) %>%
        mutate(percentdem = ifelse(party == "D", percentparty, 100-percentparty)) %>%
        mutate(percentrepub = 100-percentdem) %>%
        subset(!(Abbreviation %in% c("","Guam"))) %>%
        rename(Office = office,Candidate = candidate,Party = party, District = dist) %>%
        droplevels()
```

Make data for map: select a subset of needed variables and remove duplicates so there is only one row per state. Rename variables to be readable for the map's hover text. Change abbreviation `state` and full name `State` from factor to character variables and change `NA` values to `0` for hover text.

``` r
statedata <- elections[,c(1,11,15,18,19)] %>%
        unique() %>%
        rename("Percent Democrat" = "percentdem") %>%
        rename("Percent Republican" = "percentrepub") %>%
        mutate(state = as.character(state)) %>%
        mutate(State = as.character(State)) %>%
        mutate_all(funs(replace(., is.na(.), 0)))
```

To change continuous `dempercent` variable to categorical for mapping, bin and label it. Currently, `statebins` does not work well with `plotly`, resulting in a non-interactive map. To get around that, based on code from [Kenton Russell](http://bl.ocks.org/timelyportfolio/1cce2d1190460a7f1406945bdc02f4e1) combine dataframe with `statebins` coordinates data `statebins:::state_coords` and plot it with no lines, tickmarks, axis labels, or grid to make a `plotly` map with working interactive hover info.

Rearrange column order to create hover text variable that will be made with `Map` `Reduce`. The default coordinates in `statebins` places Alaska in the bottom left of the plot, just above Hawaii. Intuitively, it should be placed in the upper left corner of the plot, northwest of an imaginary Canada. Change Alaska's `row` variable to `1` in dataframe to move it.

``` r
breaks <- c(0,20,40,60,80,100)

labels <- c("1-20% Dem", "21-40% Dem", "41-60% Dem", "61-80% Dem", "81-100% Dem")

statedata <- statedata %>%
        mutate(bins = cut(statedata$`Percent Democrat`, breaks, 
                          include.lowest = T, right=FALSE, labels = labels)) %>%
        mutate(bins = as.character(bins)) %>%
        mutate(bins = as.factor(ifelse(`Women Running` == 0, "0 Women Running", bins))) %>%
        merge(statebins:::state_coords[,-2], by.x = "state", by.y = "abbrev") %>%
        .[,c(3,2,4,5,1,6:8)] 

vars <- Map(
        function(x, y) paste0(x, ": ", y),
        names(statedata)[1:(ncol(statedata)-4)],
        statedata[,1:(ncol(statedata)-4)]
)

statedata <- statedata %>%
        mutate(txt = Reduce(function(x, y) paste0(x, "<br />", y), vars))

statedata[1,8] <- 1
```

Make color palette with gray to reperesent states with no women currently running and 5 colors ranging from the traditional American political party red (Republican) to blue (Democrat) to represent the bins for percent Republican/Democrat among women candidates per state.

``` r
mapcolors <- c("#808080",get_palette(c("#b2182b","#2166ac"), 5))
```

Make the `plotly` - based map for use in a `shiny` app.

The full `shiny` app code, including code for the `datatable` section of the app, can be found in the `app.R` file in [this GitHub repo](https://github.com/JListman/Scrape_WomenRunning_CAWP).

Make `plotly` - based map for use in a `shiny` app. Full `shiny` app can be found in [this github repo](https://github.com/JListman/Scrape_WomenRunning_CAWP).

``` r
y_Axis <- list(title = "",
                zeroline = FALSE,
                showline = FALSE,
                showticklabels = FALSE,
                showgrid = FALSE
                )
                
x_Axis <- list(title = "Hover Over State for Details",
               zeroline = FALSE,
               showline = FALSE,
               showticklabels = FALSE,
               showgrid = FALSE
                )
                
hovermap<- (plot_ly(statedata, x = ~col, y = ~-row) %>%
        add_markers(color = ~bins,
                    colors = mapcolors,
                    text = ~txt,
                    symbol = I("square"),
                    size = I(35),
                    hoverinfo = "text"
                    ) %>%
        add_text(text = ~state, color = I("white"), hoverinfo = "none") %>%
        layout(showlegend = FALSE, xaxis = x_Axis, yaxis = y_Axis)
)
    
hovermap            
```

![](ScrapeCandidates_files/figure-markdown_github/plot_map-1.png)