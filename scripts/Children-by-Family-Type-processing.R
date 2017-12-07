library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(plyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Children by Family Type
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

ctGeos <- getCTGeos()
yearList = c(2010:2016)
tn = "B09002"
acsdata <- getACSData(ctGeos, yearList=yearList, table = tn)

dataset <- data.table()

for(data in acsdata) {
    year <- data@endyear
    print(paste("Processing: ", year))
    year <- paste(year-4, year, sep="-")
    
    #total populations
    total.total <- data[,1 ]
    acs.colnames(total.total) <- "Total:Total"

    # All households
    total.under.3 <- acsSum(data, c(3, 10, 16), "Total:Under 3 years")
    percent.total.under.3 <- divide.acs(total.under.3, total.total, method="proportion")
    total.3.4 <- acsSum(data, c(4, 11, 17), "Total:3 and 4 years")
    percent.total.3.4 <- divide.acs(total.3.4, total.total, method="proportion")
    total.5 <- acsSum(data, c(5, 12, 18), "Total:5 years")
    percent.total.5 <- divide.acs(total.5, total.total, method="proportion")
    total.6.11 <- acsSum(data, c(6, 13, 19), "Total:6 to 11 years")
    percent.total.6.11 <- divide.acs(total.6.11, total.total, method="proportion")
    total.12.17 <- acsSum(data, c(7, 14, 20), "Total:12 to 17 years")
    percent.total.12.17 <- divide.acs(total.12.17, total.total, method="proportion")

    # married households
    married.total <- data[,2 ]
    acs.colnames(married.total) <- "Married:Total"
    percent.married.total <- divide.acs(married.total, total.total, method="proportion")
    married.under.3 <- data[,3 ]
    acs.colnames(married.under.3) <- "Married:Under 3 years"
    percent.married.under.3 <- divide.acs(married.under.3, total.total, method="proportion")
    married.3.4 <- data[,4 ]
    acs.colnames(married.3.4) <- "Married:3 and 4 years"
    percent.married.3.4 <- divide.acs(married.3.4, total.total, method="proportion")
    married.5 <- data[,5 ]
    acs.colnames(married.5) <- "Married:5 years"
    percent.married.5 <- divide.acs(married.5, total.total, method="proportion")
    married.6.11 <- data[,6 ]
    acs.colnames(married.6.11) <- "Married:6 to 11 years"
    percent.married.6.11 <- divide.acs(married.6.11, total.total, method="proportion")
    married.12.17 <- data[,7 ]
    acs.colnames(married.12.17) <- "Married:12 to 17 years"
    percent.married.12.17 <- divide.acs(married.12.17, total.total, method="proportion")

    # single parent households
    single.total <- data[,8 ]
    acs.colnames(single.total) <- "Single:Total"
    percent.single.total <- divide.acs(single.total, total.total, method="proportion")
    single.under.3 <- acsSum(data, c(10, 16), "Single:Under 3 years")
    percent.single.under.3 <- divide.acs(single.under.3, total.total, method="proportion")
    single.3.4 <- acsSum(data, c(11, 17), "Single:3 and 4 years")
    percent.single.3.4 <- divide.acs(single.3.4, total.total, method="proportion")
    single.5 <- acsSum(data, c(12, 18), "Single:5 years")
    percent.single.5 <- divide.acs(single.5, total.total, method="proportion")
    single.6.11 <- acsSum(data, c(13, 19), "Single:6 to 11 years")
    percent.single.6.11 <- divide.acs(single.6.11, total.total, method="proportion")
    single.12.17 <- acsSum(data, c(14, 20), "Single:12 to 17 years")
    percent.single.12.17 <- divide.acs(single.12.17, total.total, method="proportion")

    # single parent households, male parent
    male.total <- data[,9 ]
    acs.colnames(male.total) <- "Male:Total"
    percent.male.total <- divide.acs(male.total, total.total, method="proportion")
    male.under.3 <- data[,10 ]
    acs.colnames(male.under.3) <- "Male:Under 3 years"
    percent.male.under.3 <- divide.acs(male.under.3, total.total, method="proportion")
    male.3.4 <- data[,11 ]
    acs.colnames(male.3.4) <- "Male:3 and 4 years"
    percent.male.3.4 <- divide.acs(male.3.4, total.total, method="proportion")
    male.5 <- data[,12 ]
    acs.colnames(male.5) <- "Male:5 years"
    percent.male.5 <- divide.acs(male.5, total.total, method="proportion")
    male.6.11 <- data[,13 ]
    acs.colnames(male.6.11) <- "Male:6 to 11 years"
    percent.male.6.11 <- divide.acs(male.6.11, total.total, method="proportion")
    male.12.17 <- data[,14 ]
    acs.colnames(male.12.17) <- "Male:12 to 17 years"
    percent.male.12.17 <- divide.acs(male.12.17, total.total, method="proportion")

    # single parent households, female parent
    female.total <- data[,15 ]
    acs.colnames(female.total) <- "Female:Total"
    percent.female.total <- divide.acs(female.total, total.total, method="proportion")
    female.under.3 <- data[,16 ]
    acs.colnames(female.under.3) <- "Female:Under 3 years"
    percent.female.under.3 <- divide.acs(female.under.3, total.total, method="proportion")
    female.3.4 <- data[,17 ]
    acs.colnames(female.3.4) <- "Female:3 and 4 years"
    percent.female.3.4 <- divide.acs(female.3.4, total.total, method="proportion")
    female.5 <- data[,18 ]
    acs.colnames(female.5) <- "Female:5 years"
    percent.female.5 <- divide.acs(female.5, total.total, method="proportion")
    female.6.11 <- data[,19 ]
    acs.colnames(female.6.11) <- "Female:6 to 11 years"
    percent.female.6.11 <- divide.acs(female.6.11, total.total, method="proportion")
    female.12.17 <- data[,20 ]
    acs.colnames(female.12.17) <- "Female:12 to 17 years"
    percent.female.12.17 <- divide.acs(female.12.17, total.total, method="proportion")
    
    datafips <- data.table(fips = getACSFips(data))

    # Cast to separate data frames
    numberEstimates <- data.table(
            datafips$fips,
            estimate(total.total),
            estimate(total.under.3),
            estimate(total.3.4),
            estimate(total.5),
            estimate(total.6.11),
            estimate(total.12.17),
            estimate(married.total),
            estimate(married.under.3),
            estimate(married.3.4),
            estimate(married.5),
            estimate(married.6.11),
            estimate(married.12.17),
            estimate(single.total),
            estimate(single.under.3),
            estimate(single.3.4),
            estimate(single.5),
            estimate(single.6.11),
            estimate(single.12.17),
            estimate(male.total),
            estimate(male.under.3),
            estimate(male.3.4),
            estimate(male.5),
            estimate(male.6.11),
            estimate(male.12.17),
            estimate(female.total),
            estimate(female.under.3),
            estimate(female.3.4),
            estimate(female.5),
            estimate(female.6.11),
            estimate(female.12.17),
            year,
            "Number",
            "Children Under 18"
        )
    numberMOES <- data.table(
            datafips$fips,
            standard.error(total.total) * 1.645,
            standard.error(total.under.3) * 1.645,
            standard.error(total.3.4) * 1.645,
            standard.error(total.5) * 1.645,
            standard.error(total.6.11) * 1.645,
            standard.error(total.12.17) * 1.645,
            standard.error(married.total) * 1.645,
            standard.error(married.under.3) * 1.645,
            standard.error(married.3.4) * 1.645,
            standard.error(married.5) * 1.645,
            standard.error(married.6.11) * 1.645,
            standard.error(married.12.17) * 1.645,
            standard.error(single.total) * 1.645,
            standard.error(single.under.3) * 1.645,
            standard.error(single.3.4) * 1.645,
            standard.error(single.5) * 1.645,
            standard.error(single.6.11) * 1.645,
            standard.error(single.12.17) * 1.645,
            standard.error(male.total) * 1.645,
            standard.error(male.under.3) * 1.645,
            standard.error(male.3.4) * 1.645,
            standard.error(male.5) * 1.645,
            standard.error(male.6.11) * 1.645,
            standard.error(male.12.17) * 1.645,
            standard.error(female.total) * 1.645,
            standard.error(female.under.3) * 1.645,
            standard.error(female.3.4) * 1.645,
            standard.error(female.5) * 1.645,
            standard.error(female.6.11) * 1.645,
            standard.error(female.12.17) * 1.645,
            year,
            "Number",
            "Margins of Error"
        )
    numberNames <- c(
            "FIPS",
            "Total:Total",
            "Total:Under 3 years",
            "Total:3 to 4 years",
            "Total:5 years",
            "Total:6 to 11 years",
            "Total:12 to 17 years",
            "Married-couple Family:Total",
            "Married-couple Family:Under 3 years",
            "Married-couple Family:3 to 4 years",
            "Married-couple Family:5 years",
            "Married-couple Family:6 to 11 years",
            "Married-couple Family:12 to 17 years",
            "Single-parent Family:Total",
            "Single-parent Family:Under 3 years",
            "Single-parent Family:3 to 4 years",
            "Single-parent Family:5 years",
            "Single-parent Family:6 to 11 years",
            "Single-parent Family:12 to 17 years",
            "Single-parent Familiy, Male Parent:Total",
            "Single-parent Familiy, Male Parent:Under 3 years",
            "Single-parent Familiy, Male Parent:3 to 4 years",
            "Single-parent Familiy, Male Parent:5 years",
            "Single-parent Familiy, Male Parent:6 to 11 years",
            "Single-parent Familiy, Male Parent:12 to 17 years",
            "Single-parent Family, Female Parent:Total",
            "Single-parent Family, Female Parent:Under 3 years",
            "Single-parent Family, Female Parent:3 to 4 years",
            "Single-parent Family, Female Parent:5 years",
            "Single-parent Family, Female Parent:6 to 11 years",
            "Single-parent Family, Female Parent:12 to 17 years",
            "Year",
            "Measure Type",
            "Variable"
         )
    setnames(numberEstimates, numberNames)
    setnames(numberMOES, numberNames)
    
    numbersData.melt <- melt(
            rbind(numberEstimates, numberMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Children Under 18",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    
    percentEstimates <- data.table(
            datafips$fips,
            estimate(percent.total.under.3),
            estimate(percent.total.3.4),
            estimate(percent.total.5),
            estimate(percent.total.6.11),
            estimate(percent.total.12.17),
            estimate(percent.married.total),
            estimate(percent.married.under.3),
            estimate(percent.married.3.4),
            estimate(percent.married.5),
            estimate(percent.married.6.11),
            estimate(percent.married.12.17),
            estimate(percent.single.total),
            estimate(percent.single.under.3),
            estimate(percent.single.3.4),
            estimate(percent.single.5),
            estimate(percent.single.6.11),
            estimate(percent.single.12.17),
            estimate(percent.male.total),
            estimate(percent.male.under.3),
            estimate(percent.male.3.4),
            estimate(percent.male.5),
            estimate(percent.male.6.11),
            estimate(percent.male.12.17),
            estimate(percent.female.total),
            estimate(percent.female.under.3),
            estimate(percent.female.3.4),
            estimate(percent.female.5),
            estimate(percent.female.6.11),
            estimate(percent.female.12.17),
            year,
            "Percent",
            "Children Under 18"
        )
    percentMOES <- data.table(
                datafips$fips,
                standard.error(percent.total.under.3) * 1.645,
                standard.error(percent.total.3.4) * 1.645,
                standard.error(percent.total.5) * 1.645,
                standard.error(percent.total.6.11) * 1.645,
                standard.error(percent.total.12.17) * 1.645,
                standard.error(percent.married.total) * 1.645,
                standard.error(percent.married.under.3) * 1.645,
                standard.error(percent.married.3.4) * 1.645,
                standard.error(percent.married.5) * 1.645,
                standard.error(percent.married.6.11) * 1.645,
                standard.error(percent.married.12.17) * 1.645,
                standard.error(percent.single.total) * 1.645,
                standard.error(percent.single.under.3) * 1.645,
                standard.error(percent.single.3.4) * 1.645,
                standard.error(percent.single.5) * 1.645,
                standard.error(percent.single.6.11) * 1.645,
                standard.error(percent.single.12.17) * 1.645,
                standard.error(percent.male.total) * 1.645,
                standard.error(percent.male.under.3) * 1.645,
                standard.error(percent.male.3.4) * 1.645,
                standard.error(percent.male.5) * 1.645,
                standard.error(percent.male.6.11) * 1.645,
                standard.error(percent.male.12.17) * 1.645,
                standard.error(percent.female.total) * 1.645,
                standard.error(percent.female.under.3) * 1.645,
                standard.error(percent.female.3.4) * 1.645,
                standard.error(percent.female.5) * 1.645,
                standard.error(percent.female.6.11) * 1.645,
                standard.error(percent.female.12.17) * 1.645,
                year,
                "Percent",
                "Margins of Error"
            )
    percentNames <- c(
            "FIPS",
            "Total:Under 3 years",
            "Total:3 to 4 years",
            "Total:5 years",
            "Total:6 to 11 years",
            "Total:12 to 17 years",
            "Married-couple Family:Total",
            "Married-couple Family:Under 3 years",
            "Married-couple Family:3 to 4 years",
            "Married-couple Family:5 years",
            "Married-couple Family:6 to 11 years",
            "Married-couple Family:12 to 17 years",
            "Single-parent Family:Total",
            "Single-parent Family:Under 3 years",
            "Single-parent Family:3 to 4 years",
            "Single-parent Family:5 years",
            "Single-parent Family:6 to 11 years",
            "Single-parent Family:12 to 17 years",
            "Single-parent Familiy, Male Parent:Total",
            "Single-parent Familiy, Male Parent:Under 3 years",
            "Single-parent Familiy, Male Parent:3 to 4 years",
            "Single-parent Familiy, Male Parent:5 years",
            "Single-parent Familiy, Male Parent:6 to 11 years",
            "Single-parent Familiy, Male Parent:12 to 17 years",
            "Single-parent Family, Female Parent:Total",
            "Single-parent Family, Female Parent:Under 3 years",
            "Single-parent Family, Female Parent:3 to 4 years",
            "Single-parent Family, Female Parent:5 years",
            "Single-parent Family, Female Parent:6 to 11 years",
            "Single-parent Family, Female Parent:12 to 17 years",
            "Year",
            "Measure Type",
            "Variable"
         )
    setnames(percentEstimates, percentNames)
    setnames(percentMOES, percentNames)
    percentData.melt <- melt(
            rbind(percentEstimates, percentMOES),
            id.vars=c("FIPS", "Year", "Measure Type", "Variable"),
            variable.name="Children Under 18",
            variable.factor = F,
            value.name="Value",
            value.factor = F
         )
    dataset <- rbind(dataset, numbersData.melt, percentData.melt)
}

#Final Additions, processing
#Split family type and Age of child from variable column
dataset[,c("Family Type", "Age of Child"):=do.call(Map, c(f=c, strsplit(`Children Under 18`, ":", fixed=T)))]
# Remove unnecessary variable colum, round values for estimates
dataset[,`:=`(
        `Children Under 18` = NULL,
        Value = ifelse(`Measure Type` == "Number", round(Value, 0), round(Value*100, 2))
    )]

# Join town names by FIPS code
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

dataset <- as.data.frame(dataset)

dataset <- merge(towns, dataset, by = "FIPS", all.x=T)

dataset$`Age of Child` <- factor(dataset$`Age of Child`, 
                                 levels=c("Total", "Under 3 years", "3 to 4 years", 
                                          "5 years", "6 to 11 years", "12 to 17 years"))
#set final column order
dataset <- dataset %>% 
  select(Town, Year, `Family Type`, `Age of Child`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Family Type`, `Age of Child`, `Measure Type`, Variable)

# write table
write.table(
    dataset,
    file.path("data", "children_by_family_type-2016.csv"),
    sep = ",",
    row.names=F,
    na = "-9999"
)