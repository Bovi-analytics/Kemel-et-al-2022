Data manipulation
================

  - [Required packages](#required-packages)
  - [Required functions](#required-functions)
  - [Read raw data](#read-raw-data)
      - [Neospora results](#neospora-results)
      - [Bull age](#bull-age)
      - [Join table](#join-table)
      - [Sperm results](#sperm-results)
      - [Meteorologic Data](#meteorologic-data)
  - [Anonymization of the data](#anonymization-of-the-data)
  - [Data joining](#data-joining)
  - [Anonizing the data](#anonizing-the-data)
  - [Data storage](#data-storage)

# Required packages

  - [openxlsx](https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf)
    for excel
  - [dplyr](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8)
    for data pipeline

<!-- end list -->

``` r
knitr::opts_chunk$set(echo = TRUE)

package_list <- c("openxlsx", "dplyr","readr","tidyverse","lubridate","zoo")

for (pkg in package_list) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {library(pkg, character.only = TRUE)}
  print(citation(pkg))
  
}
```

    ## 
    ## To cite package 'openxlsx' in publications use:
    ## 
    ##   Philipp Schauberger and Alexander Walker (2021). openxlsx: Read,
    ##   Write and Edit xlsx Files. R package version 4.2.4.
    ##   https://CRAN.R-project.org/package=openxlsx
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {openxlsx: Read, Write and Edit xlsx Files},
    ##     author = {Philipp Schauberger and Alexander Walker},
    ##     year = {2021},
    ##     note = {R package version 4.2.4},
    ##     url = {https://CRAN.R-project.org/package=openxlsx},
    ##   }

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## 
    ## To cite package 'dplyr' in publications use:
    ## 
    ##   Hadley Wickham, Romain François, Lionel Henry and Kirill Müller
    ##   (2021). dplyr: A Grammar of Data Manipulation. R package version
    ##   1.0.7. https://CRAN.R-project.org/package=dplyr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {dplyr: A Grammar of Data Manipulation},
    ##     author = {Hadley Wickham and Romain François and Lionel Henry and Kirill Müller},
    ##     year = {2021},
    ##     note = {R package version 1.0.7},
    ##     url = {https://CRAN.R-project.org/package=dplyr},
    ##   }
    ## 
    ## 
    ## To cite package 'readr' in publications use:
    ## 
    ##   Hadley Wickham and Jim Hester (2021). readr: Read Rectangular Text
    ##   Data. R package version 2.0.1.
    ##   https://CRAN.R-project.org/package=readr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {readr: Read Rectangular Text Data},
    ##     author = {Hadley Wickham and Jim Hester},
    ##     year = {2021},
    ##     note = {R package version 2.0.1},
    ##     url = {https://CRAN.R-project.org/package=readr},
    ##   }

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v stringr 1.4.0
    ## v tidyr   1.1.3     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## 
    ##   Wickham et al., (2019). Welcome to the tidyverse. Journal of Open
    ##   Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {Welcome to the {tidyverse}},
    ##     author = {Hadley Wickham and Mara Averick and Jennifer Bryan and Winston Chang and Lucy D'Agostino McGowan and Romain François and Garrett Grolemund and Alex Hayes and Lionel Henry and Jim Hester and Max Kuhn and Thomas Lin Pedersen and Evan Miller and Stephan Milton Bache and Kirill Müller and Jeroen Ooms and David Robinson and Dana Paige Seidel and Vitalie Spinu and Kohske Takahashi and Davis Vaughan and Claus Wilke and Kara Woo and Hiroaki Yutani},
    ##     year = {2019},
    ##     journal = {Journal of Open Source Software},
    ##     volume = {4},
    ##     number = {43},
    ##     pages = {1686},
    ##     doi = {10.21105/joss.01686},
    ##   }

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## 
    ## To cite lubridate in publications use:
    ## 
    ##   Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy
    ##   with lubridate. Journal of Statistical Software, 40(3), 1-25. URL
    ##   https://www.jstatsoft.org/v40/i03/.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {Dates and Times Made Easy with {lubridate}},
    ##     author = {Garrett Grolemund and Hadley Wickham},
    ##     journal = {Journal of Statistical Software},
    ##     year = {2011},
    ##     volume = {40},
    ##     number = {3},
    ##     pages = {1--25},
    ##     url = {https://www.jstatsoft.org/v40/i03/},
    ##   }

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## To cite zoo in publications use:
    ## 
    ##   Achim Zeileis and Gabor Grothendieck (2005). zoo: S3 Infrastructure
    ##   for Regular and Irregular Time Series. Journal of Statistical
    ##   Software, 14(6), 1-27. doi:10.18637/jss.v014.i06
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {zoo: S3 Infrastructure for Regular and Irregular Time Series},
    ##     author = {Achim Zeileis and Gabor Grothendieck},
    ##     journal = {Journal of Statistical Software},
    ##     year = {2005},
    ##     volume = {14},
    ##     number = {6},
    ##     pages = {1--27},
    ##     doi = {10.18637/jss.v014.i06},
    ##   }

# Required functions

``` r
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
```

# Read raw data

## Neospora results

Read all excel files in loop

``` r
filenames_list <- list.files("../Data/NeosporaAnalysisResults/", full.names = TRUE)

all_files <- lapply(filenames_list,function(filename){
    print(paste("Merging",filename,sep = " "))
    read.xlsx(filename) %>% dplyr::select(
        "NumberFile",
        "NumberSample",
        "AnimalIdentifier",
        "Result",
        "CellResult"
        ) %>%
      dplyr::mutate(
        "File" = filename
      )
})
```

    ## [1] "Merging ../Data/NeosporaAnalysisResults/Neospora02032020.xlsx"
    ## [1] "Merging ../Data/NeosporaAnalysisResults/Neospora06012020.xlsx"
    ## [1] "Merging ../Data/NeosporaAnalysisResults/Neospora13102020.xlsx"
    ## [1] "Merging ../Data/NeosporaAnalysisResults/Neospora20082020.xlsx"
    ## [1] "Merging ../Data/NeosporaAnalysisResults/Neospora22062020.xlsx"
    ## [1] "Merging ../Data/NeosporaAnalysisResults/Neospora27042020.xlsx"

  - Merge all to one dataframe
  - Make factors instead of characters
  - Make correct numeric from . or ,
  - Create SampleDate from filename
  - Create 1 result from each bull

<!-- end list -->

``` r
df_cell_results <- do.call(rbind.data.frame, all_files) %>%
  dplyr::mutate(AnimalIdentifier = substrRight(AnimalIdentifier,8))%>%
  dplyr::mutate_if(sapply(., is.character), as.factor) %>%
  dplyr::mutate(
    AnimalIdentifier =  as.factor(AnimalIdentifier),
    Result = as.factor(gsub("\\s", "", Result)),
    CellResult=as.numeric(gsub(",",".",CellResult)),
    NeosporaSampleDate=as.Date(gsub(".xlsx","",gsub("../Data/NeosporaAnalysisResults/Neospora","",File)),"%d%m%Y")
  ) %>%
  dplyr::rename(
    NeosporaResult = Result
  ) %>%
  group_by(
    AnimalIdentifier
  ) %>%
  arrange(NeosporaResult) %>%
summarize(NeosporaResult=paste(NeosporaResult,collapse=""),.groups="keep")%>%
#determination of the results should be as long as the number of samples you have
  dplyr::mutate(
 NeosporaResult = as.factor(case_when(
NeosporaResult == "NégatifNégatifNégatifNégatifNégatifNégatif" ~ "Negative",
                  NeosporaResult == "PositifPositifPositifPositifPositifPositif" ~ "Positive",
                   TRUE ~ "Uninterpretable"
                  ))
)
```

## Bull age

MS : Due to longitudinal window aspect of study, I think it would be
better to work with the birthdate compared to the fixed age value in
august.

## Join table

reference table to switch from farm animal name to animal identifier

``` r
df_join <- read.csv2("../Data/AnimalID-AnimalName.csv")%>%
 dplyr::mutate(AnimalIdentifier = substrRight(AnimalIdentifier,8))%>%
  mutate_if(sapply(., is.character), as.factor) 
```

## Sperm results

Added \* SampleNumber \* SampleSeason \* ProdSeason

``` r
df_spermJanAug <- read.xlsx("../Data/SpermQualityAnalysisResults/ResultsJanuarySeptember.xlsx") %>%
  #dplyr::mutate_if(sapply(., is.character), as.factor) %>%
  dplyr::select(
    "N°.site",
    "Date",
    "Nom",
    "Vol",
    "Conc,",
    "%Viv",
    "%Viv.post.cong", 
    "%.prog.post", 
    "%.Sp.Norm.post.cong"
  ) %>%
  dplyr::rename(
    "Site" = "N°.site",
    "AnimalName" = "Nom",
    "Volume" = "Vol",
    "Concentration" = "Conc,",
    "MotilityPercentageFresh" = "%Viv",
    "MotilityPercentageThawedTotal" = "%Viv.post.cong",
    "MotilityPercentageThawedProg" = "%.prog.post",    
    "MorfThawed" = "%.Sp.Norm.post.cong"
    
    
    ) %>%
    dplyr:: mutate(Site = as.factor(Site)) %>%
    dplyr::distinct() %>%
                      # Does not allow groupby if animalNAme is used here 
  dplyr::group_by(AnimalName) %>% 
  # old code did not work "dplyr::group_by(.data = AnimalName)" yieled error "Could not find object AnimalName" works now but don't know what the intention was with initial code MS @ MH?
  arrange(AnimalName, Date) %>%
  dplyr::mutate(
    Date = openxlsx::convertToDate(Date),
       ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    SampleMonth = format(Date, "%m"),
    SampleSeason = as.factor(
      case_when(
        SampleMonth == "01" ~ "Winter",
        SampleMonth == "02" ~ "Winter",
        SampleMonth == "03" ~ "Spring",
        SampleMonth == "04" ~ "Spring",
        SampleMonth == "05" ~ "Spring",
        SampleMonth == "06" ~ "Summer",
        SampleMonth == "07" ~ "Summer",
        SampleMonth == "08" ~ "Summer",
        SampleMonth == "09" ~ "Fall",
        SampleMonth == "10" ~ "Fall",
        SampleMonth == "11" ~ "Fall",
        SampleMonth == "12" ~ "Winter",
        TRUE ~ "Other"
        )
    ), 
    ProductionSeason = as.factor(
      case_when(
        SampleMonth == "01"~ "Winter", 
        SampleMonth == "02"~ "Winter",
        SampleMonth == "03"~ "Winter",
        SampleMonth == "04"~ "Spring",
        SampleMonth == "05"~ "Spring",
        SampleMonth == "06"~ "Spring",
        SampleMonth == "07"~ "Summer", 
        SampleMonth == "08"~ "Summer", 
        SampleMonth == "09"~ "Summer", 
        SampleMonth == "10"~ "Fall",
        SampleMonth == "11"~ "Fall",
        SampleMonth == "12"~ "Fall",
        TRUE~ "Other"
        )
    ),
    TotalSpermOutput = Volume * Concentration
  )%>%
 dplyr::filter(format(Date, "%m")!="09")
```

addition of complementary results

``` r
df_spermSeptDec <- read.xlsx("../Data/SpermQualityAnalysisResults/ResultsSeptemberDecember.xlsx") %>%
  #dplyr::mutate_if(sapply(., is.character), as.factor) %>%
  dplyr::select(
    "N°.site",
    "Date",
    "Nom",
    "Vol",
    "Conc,",
    "%Viv",
    "%Viv.post.cong", 
    "%.prog.post", 
    "%.Sp.Norm.post.cong"
  ) %>%
  dplyr::rename(
    "Site" = "N°.site",
    "AnimalName" = "Nom",
    "Volume" = "Vol",
    "Concentration" = "Conc,",
    "MotilityPercentageFresh" = "%Viv",
    "MotilityPercentageThawedTotal" = "%Viv.post.cong",
    "MotilityPercentageThawedProg" = "%.prog.post",    
    "MorfThawed" = "%.Sp.Norm.post.cong"
    
    
    ) %>%
    dplyr:: mutate(Site = as.factor(Site)) %>%
    dplyr::distinct() %>%
                      # Does not allow groupby if animalNAme is used here 
  dplyr::group_by(AnimalName) %>% 
  # old code did not work "dplyr::group_by(.data = AnimalName)" yieled error "Could not find object AnimalName" works now but don't know what the intention was with initial code MS @ MH?
  arrange(AnimalName, Date) %>%
  dplyr::mutate(
    Date = openxlsx::convertToDate(Date),
       ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    SampleMonth = format(Date, "%m"),
    SampleSeason = as.factor(
      case_when(
        SampleMonth == "01" ~ "Winter",
        SampleMonth == "02" ~ "Winter",
        SampleMonth == "03" ~ "Spring",
        SampleMonth == "04" ~ "Spring",
        SampleMonth == "05" ~ "Spring",
        SampleMonth == "06" ~ "Summer",
        SampleMonth == "07" ~ "Summer",
        SampleMonth == "08" ~ "Summer",
        SampleMonth == "09" ~ "Fall",
        SampleMonth == "10" ~ "Fall",
        SampleMonth == "11" ~ "Fall",
        SampleMonth == "12" ~ "Winter",
        TRUE ~ "Other"
        )
    ), 
    ProductionSeason = as.factor(
      case_when(
        SampleMonth == "01"~ "Winter", 
        SampleMonth == "02"~ "Winter",
        SampleMonth == "03"~ "Winter",
        SampleMonth == "04"~ "Spring",
        SampleMonth == "05"~ "Spring",
        SampleMonth == "06"~ "Spring",
        SampleMonth == "07"~ "Summer", 
        SampleMonth == "08"~ "Summer", 
        SampleMonth == "09"~ "Summer", 
        SampleMonth == "10"~ "Fall",
        SampleMonth == "11"~ "Fall",
        SampleMonth == "12"~ "Fall",
        TRUE~ "Other"
      )
    ),
    TotalSpermOutput = Volume * Concentration
  )
```

Joining of both results

``` r
df_sperm <- union(df_spermJanAug,df_spermSeptDec)%>%
# Addition of meteorologic station for joining purpose later 
dplyr::mutate(Station=case_when(Site =="8"~ "ERNAGE",
                                Site =="5"~ "HUMAIN",
                                Site =="6"~ "HUMAIN",
                                TRUE ~ "No site"))%>% 
  group_by(AnimalName)%>%
  dplyr::mutate(SampleDay = yday(Date)) %>%
  dplyr :: mutate(SampleDay = as.factor(SampleDay))
```

## Meteorologic Data

formula applied for calculating THI: THI = 0.8 x T + (RH/100) x (T
-14.4) + 46.4.

``` r
df_THIRaw <- read_delim("../Data/Meteorologic data.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)%>% 
  dplyr::rename("Year"="YEAR",
                  "Month"="MONTH",
                  "Day"="DAY",
                  "Hour"="HOUR",
                  "Station"= "STATION", 
                  "Code"="CODE",
                  "TemperatureAverage"= "TEMPERATURE AVERAGE",
                  "RelativeHumidity"= "RELATIVE HUMIDITY")%>%

  dplyr::mutate(THI=0.8*TemperatureAverage + (RelativeHumidity/100) * (TemperatureAverage - 14.4) + 46.4,
                  Date= make_date (Year, Month, Day))
```

    ## Rows: 20496 Columns: 8

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ";"
    ## chr (1): STATION
    ## dbl (7): YEAR, MONTH, DAY, HOUR, CODE, TEMPERATURE AVERAGE, RELATIVE HUMIDITY

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df_MaxTHISample <- df_THIRaw %>% dplyr:: arrange(Station,Date)%>%
 dplyr::group_by(Station, Date)%>%
 summarise(SampleMaxTHI= max(THI))
```

    ## `summarise()` has grouped output by 'Station'. You can override using the `.groups` argument.

``` r
df_MeanTHIProduction <- df_THIRaw %>%
  dplyr:: arrange(Station,Date)%>%
  dplyr::group_by(Station, Date)%>%
  summarise(THIDailyMAX= max(THI)) %>% 
  ungroup() %>%
  dplyr::group_by(Station)%>%
  dplyr::mutate( ProductionMeanTHI = zoo::rollmean(THIDailyMAX, k =  28,fill = 0, align = 'right'),
          Date = Date + 14) %>%
  dplyr::select(-THIDailyMAX)
```

    ## `summarise()` has grouped output by 'Station'. You can override using the `.groups` argument.

``` r
df_THI <- df_MaxTHISample %>% dplyr::inner_join(df_MeanTHIProduction, c("Station"="Station","Date"="Date"))
```

# Anonymization of the data

``` r
df_join <- df_join %>% dplyr::mutate(AnonymId = row_number())
```

# Data joining

``` r
df_final <- df_join %>% 
  dplyr::inner_join(df_sperm, AnimalName = AnimalName) %>%
  dplyr::inner_join(df_cell_results, AnimalIdentifier = AnimalIdentifier) %>%   
  dplyr::inner_join(df_ages, AnimalIdentifier = AnimalIdentifier)%>%
  dplyr::inner_join(df_THI, c("Station"="Station","Date"="Date"))%>%
  #Recalculation of age by birth date instead of sample date 
  dplyr::mutate(Age=Date-BirthDate)%>%
  dplyr::filter(NeosporaResult != "Uninterpretable")%>%
  #Removing non Anonymized data
  dplyr::select(-c(AnimalName,AnimalIdentifier,AnimalNameLONG))%>%
  dplyr::rename(AnimalIdentifier = AnonymId)
```

    ## Joining, by = "AnimalName"

    ## Joining, by = "AnimalIdentifier"
    ## Joining, by = "AnimalIdentifier"

# Anonizing the data

# Data storage

``` r
save(df_final, file = "../Data/FinalData.RData")
```
