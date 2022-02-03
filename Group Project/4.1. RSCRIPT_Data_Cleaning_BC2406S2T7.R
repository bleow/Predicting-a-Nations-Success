#################################  B C 2 4 0 6   S E M I N A R   2   T E A M   7  ##################################
###########################################  D A T A    C L E A N I N G  ###########################################
######################################### Instructor: Prof Vivek Choudhary #########################################

setwd("C:/Users/.../BC2406_S2_Team7_Project/3. Data, Data Dictionary")

library(rpart)
library(rpart.plot)
library(data.table)
library(nnet)
library(caret)
library(caTools)
library(car)
library(dplyr)
library(ggplot2)
library(tidyr)

#######################################################################################
############################ PART 1: VARIABLE SHORTLISTING ############################
###################### (supplementing Section 3.1 of the report) ######################
#######################################################################################

############## import data ##############
earlychildhood = fread("worldbank data/earlychildhood.csv")
govtexp        = fread("worldbank data/govt_exp.csv")
literacyrate   = fread("worldbank data/literacyrate.csv")
primaryedu     = fread("worldbank data/primaryedu.csv")
secedu         = fread("worldbank data/secedu.csv")
teachers       = fread("worldbank data/teachers.csv")
tertiary       = fread("worldbank data/tertiary.csv")
eduoutcomes    = fread("worldbank data/eduoutcomes.csv")
popn           = fread("worldbank data/population.csv")

############## encode .. as proper NA ##############
earlychildhood[earlychildhood == ".."]  = NA
govtexp[govtexp == ".."]                = NA
literacyrate[literacyrate == ".."]      = NA
primaryedu[primaryedu == ".."]          = NA
secedu[secedu == ".."]                  = NA
teachers[teachers == ".."]              = NA
tertiary[tertiary == ".."]              = NA
eduoutcomes[eduoutcomes == ".."]        = NA
popn[popn == ".."]                      = NA


############## covert all number columns into number datatype ##############
earlychildhood[,1:ncol(earlychildhood)]=lapply(1:ncol(earlychildhood),function(x)
{
    tryCatch(
        {
            as.numeric(earlychildhood[[x]])
        }, warning = function(w)
        {
            earlychildhood[[x]]
        })
})
govtexp[,1:ncol(govtexp)]=lapply(1:ncol(govtexp),function(x)
{
    tryCatch(
        {
            as.numeric(govtexp[[x]])
        }, warning = function(w)
        {
            govtexp[[x]]
        })
})
literacyrate[,1:ncol(literacyrate)]=lapply(1:ncol(literacyrate),function(x)
{
    tryCatch(
        {
            as.numeric(literacyrate[[x]])
        }, warning = function(w)
        {
            literacyrate[[x]]
        })
})
primaryedu[,1:ncol(primaryedu)]=lapply(1:ncol(primaryedu),function(x)
{
    tryCatch(
        {
            as.numeric(primaryedu[[x]])
        }, warning = function(w)
        {
            primaryedu[[x]]
        })
})
secedu[,1:ncol(secedu)]=lapply(1:ncol(secedu),function(x)
{
    tryCatch(
        {
            as.numeric(secedu[[x]])
        }, warning = function(w)
        {
            secedu[[x]]
        })
})
teachers[,1:ncol(teachers)]=lapply(1:ncol(teachers),function(x)
{
    tryCatch(
        {
            as.numeric(teachers[[x]])
        }, warning = function(w)
        {
            teachers[[x]]
        })
})
tertiary[,1:ncol(tertiary)]=lapply(1:ncol(tertiary),function(x)
{
    tryCatch(
        {
            as.numeric(tertiary[[x]])
        }, warning = function(w)
        {
            tertiary[[x]]
        })
})
eduoutcomes[,1:ncol(eduoutcomes)]=lapply(1:ncol(eduoutcomes),function(x)
{
    tryCatch(
        {
            as.numeric(eduoutcomes[[x]])
        }, warning = function(w)
        {
            eduoutcomes[[x]]
        })
})
popn[,1:ncol(popn)]=lapply(1:ncol(popn),function(x)
{
    tryCatch(
        {
            as.numeric(popn[[x]])
        }, warning = function(w)
        {
            popn[[x]]
        })
})

############## VISUALISATION // AMOUNT OF MISSING DATA PER CATEGORY (GGPLOT) ##############
# early childhood #
colSums(is.na(earlychildhood))
# ^simple method. for ggplot method:
countNA = earlychildhood %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Early Childhood")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1))+
    scale_fill_gradient(low = "springgreen1", high = "firebrick1")
# The best metrics are:
## Enrolment in pre-primary education, both sexes (number) [SE.PRE.ENRL]
## School life expectancy, pre-primary, both sexes (years) [UIS.SLE.02]
## Gross enrolment ratio, pre-primary, both sexes (%) [SE.PRE.ENRR]

# government expenditure #
colSums(is.na(govtexp))
countNA = govtexp %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Government Expenditure")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.title=element_text(size=8), plot.title = element_text(size=12))+
    scale_fill_gradient(low="springgreen1", high="firebrick1")
# The best metrics are:
## Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]
## Government expenditure on education, constant US$ (millions) [UIS.X.USCONST.FSGOV]

# literacy rate #
countNA = literacyrate %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Literacy Rate")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.title=element_text(size=8), plot.title = element_text(size=12))+
    scale_fill_gradient(low="springgreen1", high="firebrick1")

colSums(is.na(literacyrate))
# The best metrics are:
## Adult illiterate population, 15+ years, both sexes (number) [UIS.LP.AG15T99]
## Adult literacy rate, population 15+ years, both sexes (%) [SE.ADT.LITR.ZS]

# primary education #
countNA = primaryedu %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Primary Education")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.title=element_text(size=8), plot.title = element_text(size=12))+
    scale_fill_gradient(low="springgreen1", high="firebrick1")
# The best metrics are:
## Enrolment in primary education, both sexes (number) [SE.PRM.ENRL] - 7688
## School life expectancy, primary, both sexes (years) [UIS.SLE.1] - 7982
## Gross enrolment ratio, primary, both sexes (%) [SE.PRM.ENRR] - 7982
## [worth exploring] Completion rate, primary education, both sexes (%) [UIS.CR.1] - 17363

# secondary education #
countNA = secedu %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Secondary Education")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.title=element_text(size=8), plot.title = element_text(size=12))+
    scale_fill_gradient(low="springgreen1", high="firebrick1")

# The best metrics are:
## Enrolment in secondary education, both sexes (number) [SE.SEC.ENRL] - 8877
## Gross enrolment ratio, secondary, both sexes (%) [SE.SEC.ENRR]  - 9160
## School life expectancy, primary and secondary, both sexes (years) [UIS.SLE.123] - 9284
## School life expectancy, secondary, both sexes (years) [UIS.SLE.23] - 11500
## [worth exploring] Completion rate, lower secondary education, both sexes (%) - 17245
## [worth exploring] Completion rate, upper secondary education, both sexes (%) - 17250

# teachers #
countNA = teachers %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Teachers")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.title=element_text(size=8), plot.title = element_text(size=12))+
    scale_fill_gradient(low="springgreen1", high="firebrick1")
# The best metrics are:
## Pupil-trained teacher ratio in pre-primary education (headcount basis) [UIS.PTRHC.02.TRAINED] - 16737

# tertiary education #
countNA = tertiary %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Tertiary Education")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.title=element_text(size=8), plot.title = element_text(size=12))+
    scale_fill_gradient(low="springgreen1", high="firebrick1")
# The best metrics are:
## Enrolment in tertiary education, all programmes, both sexes (number) [SE.TER.ENRL] - 9770
## [worth exploring] Percentage of graduates from Science, Technology, Engineering and Mathematics programmes in tertiary education, both sexes (%) [UIS.FOSGP.5T8.F500600700] - 16683

# educational outcomes #
countNA = eduoutcomes %>% summarise_all(funs(sum(is.na(.)))); countNA = countNA[,-c(1:4)]
countNAcols = gsub(".*\\[|\\].*", "", colnames(countNA)); countNAvals = transpose(countNA)
rm(countNA); countNA = countNAvals; countNA$col = countNAcols; colnames(countNA) = c("Number_of_NAs", "Metric"); rm(countNAvals); rm(countNAcols)
ggplot(data=countNA, aes(x=reorder(Metric, Number_of_NAs), y=Number_of_NAs, fill=Number_of_NAs))+
    geom_bar(stat="identity")+
    labs(y = "Number of NAs", x = "Metric", title="Amount of Missing Data for Educational Outcome")+
    guides(fill=guide_legend(title = "Total rows: 9548"))+
    theme(axis.text.x=element_text(angle=90, hjust=1), legend.title=element_text(size=8), plot.title = element_text(size=12))+
    scale_fill_gradient(low="springgreen1", high="firebrick1")
# The best metrics are:
## PISA: Mean performance on the science scale [LO.PISA.SCI] - 17539
## PISA: Mean performance on the reading scale [LO.PISA.REA] - 17541
## PISA: Mean performance on the mathematics scale [LO.PISA.MAT] - 17542

############## selecting out the best metrics per category ##############
edustats = subset(earlychildhood, select=c(`Country Name`, `Country Code`, Time,`Time Code` ))

edustats$Pre_pri_sch_expectancy     = earlychildhood$`School life expectancy, pre-primary, both sexes (years) [UIS.SLE.02]`
edustats$Pri_sch_expectancy         = primaryedu$`School life expectancy, primary, both sexes (years) [UIS.SLE.1]`
edustats$Sec_sch_expectancy         = secedu$`School life expectancy, secondary, both sexes (years) [UIS.SLE.23]`
edustats$Pri_sec_sch_expectancy     = secedu$`School life expectancy, primary and secondary, both sexes (years) [UIS.SLE.123]`

edustats$Lower_sec_completion       = secedu$`Completion rate, lower secondary education, both sexes (%)`
edustats$Upper_sec_completion       = secedu$`Completion rate, upper secondary education, both sexes (%)`

edustats$Gov_ex                     = govtexp$`Government expenditure on education, constant US$ (millions) [UIS.X.USCONST.FSGOV]`
edustats$Gov_ex_percent_GDP         = govtexp$`Government expenditure on education as % of GDP (%) [SE.XPD.TOTL.GD.ZS]`

edustats$Literacy_rate              = literacyrate$`Adult literacy rate, population 15+ years, both sexes (%) [SE.ADT.LITR.ZS]`

edustats$Population = popn$`Population, total [SP.POP.TOTL]`

edustats$Pre_pri_enrolment_num      = earlychildhood$`Enrolment in pre-primary education, both sexes (number) [SE.PRE.ENRL]`
edustats$Pre_pri_enrolment_ratio    = earlychildhood$`Gross enrolment ratio, pre-primary, both sexes (%) [SE.PRE.ENRR]`

edustats$Pri_enrolment_num          = primaryedu$`Enrolment in primary education, both sexes (number) [SE.PRM.ENRL]`
edustats$Pri_enrolment_ratio        = primaryedu$`Gross enrolment ratio, primary, both sexes (%) [SE.PRM.ENRR]`

edustats$Pri_completion             = primaryedu$`Completion rate, primary education, both sexes (%) [UIS.CR.1]`

edustats$Sec_enrolment_num          = secedu$`Enrolment in secondary education, both sexes (number) [SE.SEC.ENRL]`
edustats$Sec_enrolment_ratio        = secedu$`Gross enrolment ratio, secondary, both sexes (%) [SE.SEC.ENRR]`

edustats$Ter_enrolment_num          = tertiary$`Enrolment in tertiary education, all programmes, both sexes (number) [SE.TER.ENRL]`

edustats$STEM_ter_grad_percent      = tertiary$`Percentage of graduates from Science, Technology, Engineering and Mathematics programmes in tertiary education, both sexes (%) [UIS.FOSGP.5T8.F500600700]`

edustats$Illiterate_pop             = literacyrate$`Adult illiterate population, 15+ years, both sexes (number) [UIS.LP.AG15T99]`

edustats$Pre_pri_pupil_teacher_ratio= teachers$`Pupil-trained teacher ratio in pre-primary education (headcount basis) [UIS.PTRHC.02.TRAINED]`

edustats$PISA_math                  = eduoutcomes$`PISA: Mean performance on the mathematics scale [LO.PISA.MAT]`
edustats$PISA_sci                   = eduoutcomes$`PISA: Mean performance on the science scale [LO.PISA.SCI]`
edustats$PISA_read                  = eduoutcomes$`PISA: Mean performance on the reading scale [LO.PISA.REA]`

rm(earlychildhood, govtexp, literacyrate, primaryedu, secedu, teachers, tertiary, eduoutcomes)
rm(countNA)

########################## END PART 1: VARIABLE SHORTLISTING ##########################

#                                        #~-~#                                        #

#######################################################################################
################# PART 2: VISUALISATION // MISSING VALUES, EA (GGPLOT) ################
##################### (supplementing Section 3.1.2 of the report) #####################
#######################################################################################
############## missing values in PISA ##############
countNA = aggregate(PISA_math ~ Time+`Country Code`, data=edustats, function(x) {sum(is.na(x))}, na.action = NULL)
countNA = subset(countNA, countNA$Time >= 2000 & countNA$Time <=2018) #pisa started in 2000 and the latest was 2018
countNA$PISA_math = as.factor(countNA$PISA_math)
levels(countNA$PISA_math) = c("Not Missing", "Missing")
ggplot(countNA, aes(Time, `Country Code`, fill=PISA_math)) + 
    geom_tile() + 
    labs(y = "Country", x = "Year", title="Missing Data Distribution for PISA Math Scores") +
    guides(fill=guide_legend(title = "Data is...")) +
    theme(axis.text.y=element_blank())+
    scale_fill_manual(values = c("#00B0F6", "#F8766D"))

rm(countNA)

############### END PART 2: VISUALISATION // MISSING VALUES, EA (GGPLOT) ##############

#                                        #~-~#                                        #

#######################################################################################
########################## PART 3: INTERPOLATING PISA SCORES ##########################
##################### (supplementing Section 3.2.1 of the report) #####################
#######################################################################################
pisa = edustats[, .(`Country Name`, `Country Code`, `Time`, `Time Code`, PISA_math, PISA_read, PISA_sci)]
pisaViz = subset(pisa, pisa$Time >= 2000 & pisa$Time <=2018) #pisa started in 2000 and the latest was 2018
pisaViz = pisaViz[ which (pisaViz$`Country Name`=="Australia" | pisaViz$`Country Name`=="Singapore")]
pisaViz = pisaViz[!is.na(pisaViz$PISA_math),]

############## VISUALISATION // PISA SCORES with TIME (GGPLOT) ##############
ggplot(data=pisaViz, aes(x=Time, y=PISA_math, group=`Country Name`, color=`Country Name`)) +
    geom_line(aes(group=`Country Name`))+
    geom_point()+
    labs(y = "PISA Math Scores", x = "Year", title="Distribution of PISA Data Points for Certain Countries")+
    guides(fill=guide_legend(title = "Country"))
rm(pisaViz)
############## split the data into separate data tables by country ##############
country_pisa <- split(pisa, f=pisa$`Country Name`)

############## perform the interpolation for by country across time ##############
for (m in 1:length(country_pisa)){
    # Vectors to store PISA values and which year they were recorded
    math_x <- math_y <- sci_x <- sci_y <- read_x <- read_y <- c()
    
    # Iterate through country_pisa[[m]] which is the data for 1 country only
    for (i in 1:nrow(country_pisa[[m]])){
        # If PISA score exists, then append score and year to the corresponding vector
        if (!is.na(country_pisa[[m]][i]$PISA_math)){
            math_x <- append(math_x,country_pisa[[m]][i]$Time)
            math_y <- append(math_y,country_pisa[[m]][i]$PISA_math)
        }
        if (!is.na(country_pisa[[m]][i]$PISA_sci)){
            sci_x <- append(sci_x,country_pisa[[m]][i]$Time)
            sci_y <- append(sci_y,country_pisa[[m]][i]$PISA_sci)
        }
        if (!is.na(country_pisa[[m]][i]$PISA_read)){
            read_x <- append(read_x,country_pisa[[m]][i]$Time)
            read_y <- append(read_y,country_pisa[[m]][i]$PISA_read)
        }
    }
    
    # If there is more than 1 PISA score available, then we carry out the interpolation
    if (length(math_x)>1){
        math_years <- min(math_x):max(math_x)                       # Get range of years to interpolate
        math_inter <- approx(math_x, math_y, xout=math_years)       # Actual interpolation
        math_count <- 1
        
        # Iterate through each row to check if the year matches
        for (i in 1:nrow(country_pisa[[m]])){
            # If match, then overwrite the data with the interpolated value (Original values will remain the same)
            if ((math_count<=length(math_inter[[1]])) && (country_pisa[[m]][i]$Time == math_inter[[1]][math_count])){
                country_pisa[[m]][i]$PISA_math = math_inter[[2]][math_count]
                math_count <- math_count+1
            }
        }
    }
    if (length(sci_x)>1){
        sci_years <- min(sci_x):max(sci_x)
        sci_inter <- approx(sci_x, sci_y, xout=sci_years)
        sci_count <- 1
        for (i in 1:nrow(country_pisa[[m]])){
            if ((sci_count<=length(sci_inter[[1]])) && (country_pisa[[m]][i]$Time == sci_inter[[1]][sci_count])){
                country_pisa[[m]][i]$PISA_sci = sci_inter[[2]][sci_count]
                sci_count <- sci_count+1
            }
        }
    }
    if (length(read_x)>1){
        read_years <- min(read_x):max(read_x)
        read_inter <- approx(read_x, read_y, xout=read_years)
        read_count <- 1
        for (i in 1:nrow(country_pisa[[m]])){
            if ((read_count<=length(read_inter[[1]])) && (country_pisa[[m]][i]$Time == read_inter[[1]][read_count])){
                country_pisa[[m]][i]$PISA_read = read_inter[[2]][read_count]
                read_count <- read_count+1
            }
        }
    }
}

############## merge all the separate data tables back ##############
pisa2 <- country_pisa[[1]]
for (i in 2:length(country_pisa)){
    pisa2 <- rbind(pisa2, country_pisa[[i]])
}

rm(i, m, math_count, math_x, math_y, math_years, read_count, read_x, read_y, read_years, sci_count, sci_x, sci_y, sci_years)
rm(math_inter, read_inter, sci_inter)
rm(country_pisa)

######################## END PART 3: INTERPOLATING PISA SCORES ########################

#                                        #~-~#                                        #

#######################################################################################
##################### PART 4: PREPARING PISA PREDICTOR VARIABLES ######################
##################### (supplementing Section 3.3.2/1 of the report) ###################
#######################################################################################
############## getting government expenditure on education per capita ##############
edustats2 = edustats
edustats2$Gov_ex_capita = (edustats2$Gov_ex)*1000000/edustats2$Population

############## split into separate data tables based on country ##############
country_edustats2 <- split(edustats2, f=edustats2$`Country Name`)

############## creating lagged variable for 3 years ##############
lag = 3
for (m in 1:length(country_edustats2)){
    country_edustats2[[m]]$`Gov_ex_3` = NA
    country_edustats2[[m]]$`Gov_ex_3` <- as.numeric(country_edustats2[[m]]$`Gov_ex_3`)
    for (i in 1:((nrow(country_edustats2[[m]]))-lag)){
        country_edustats2[[m]][i+lag]$Gov_ex_3 <- country_edustats2[[m]][i]$Gov_ex_capita
    }
}

############## merge all the separate data tables back ##############
edustats2 <- country_edustats2[[1]]
for (i in 2:length(country_edustats2)){
    edustats2 <- rbind(edustats2, country_edustats2[[i]])
}

rm(lag, popn, i, m, country_edustats2)

############## end: generating the merged dataset for PISA predictions ##############
pisa_predict           = edustats2
pisa_predict$PISA_math = pisa2$PISA_math
pisa_predict$PISA_sci  = pisa2$PISA_sci
pisa_predict$PISA_read = pisa2$PISA_read

write.csv(pisa_predict, "PISA_PREDICTORS.csv", row.names=FALSE)

rm(list = ls())


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>> end of data cleaning for part 4. continued in the analytics Rscript >>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >1>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #



# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<       <2<       <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<<< we now need to clean GDP for the next part of the analytics Rscript <<<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #

############## start: read in the raw data ##############
# Getting the predicted PISA results from the ML part
pisa <- fread("PISA_results_cart.csv")
# Getting the GDP indicators from the World Bank
econ <- fread("worldbank data/gdp.csv")
#Merging both data tables to prepare for cleaning and lagging
pred <- merge(pisa,econ, all.x=TRUE)

############## split the data into separate data tables by country ##############
country_dt <- split(pred, f=pred$`Country Name`)

############## lagging the variables ##############
# Bring GDP variables up by 1 year so we can use previous year predictors to predict next year GDP
# Less computationally intensive than bringing all the predictors up (the end result is the same)
for (m in 1:length(country_dt)){
    country_dt[[m]]$GDP_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_1[i-1] = country_dt[[m]]$`GDP`[i]
    }
    country_dt[[m]]$GDP_growth_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_growth_1[i-1] = country_dt[[m]]$`GDP_growth`[i]
    }
    country_dt[[m]]$GDP_capita_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_capita_1[i-1] = country_dt[[m]]$`GDP_capita`[i]
    }
    country_dt[[m]]$GDP_capita_growth_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_capita_growth_1[i-1] = country_dt[[m]]$`GDP_capita_growth`[i]
    }
}

############## merge all the separate data tables back ##############
new_dt<-country_dt[[1]]
for (i in 2:length(country_dt)){
    new_dt <- rbind(new_dt, country_dt[[i]])
}

############## formatting all exponents to numeric ##############
new_dt$GDP_1 <- as.numeric(as.character(new_dt$GDP_1))
new_dt$GDP_capita_1 <- as.numeric(as.character(new_dt$GDP_capita_1))
new_dt$GDP_growth_1 <- as.numeric(as.character(new_dt$GDP_growth_1))
new_dt$GDP_capita_growth_1 <- as.numeric(as.character(new_dt$GDP_capita_growth_1))

############## saving this dataframe for a later part ##############
saved_dt = new_dt 

############## end: removing irrelevant columns: time/country and non-lagged gdp  ##############
new_dt <- new_dt[,-c(1:4)]
new_dt <-dplyr::select(new_dt, -c('GDP', 'GDP_growth','GDP_capita_growth','GDP_capita'))

write.csv(new_dt, "GDP_INPUT_EDU.csv", row.names=FALSE)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>> end of data cleaning for between part 6 and 7 >>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>> continued in the analytics Rscript >>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >3>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #



# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<       <4<       <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<< we now need to clean macroeconmic data for the next part of the analytics Rscript <<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #


############## start: read in the raw data ##############
econ   <- fread("worldbank data/macroecons.csv")
#Merging both data tables to prepare for cleaning and lagging
new_dt <- merge(saved_dt,econ, all.x=TRUE)

############## saving this dataframe for a later part ##############
saved2_dt = new_dt 

############## end: removing irrelevant columns: time/country and non-lagged gdp  ##############
new_dt <- new_dt[,-c(1:4)]
new_dt <-dplyr::select(new_dt, -c('GDP', 'GDP_growth','GDP_capita_growth','GDP_capita'))

write.csv(new_dt, "GDP_INPUT_ALL.csv", row.names=FALSE)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>> end of data cleaning for between part 7 and 8 >>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>> continued in the analytics Rscript >>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >5>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #



# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<       <6<       <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<< we now need to calculate % change for the next part of the analytics Rscript <<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #


#######################################################################################
#################### PART 10: DATA PREPARATION FOR % CHANGE OF VARS ###################
###################### (supplementing Section 3.7 of the report) ######################
#######################################################################################

# this was the dataframe we saved previously around line 590
dt <- select(saved2_dt, -c('Lower_sec_completion', 'Upper_sec_completion','Pri_completion','Literacy_rate',
                          'STEM_ter_grad_percent','Illiterate_pop','Pre_pri_pupil_teacher_ratio','Gov_ex_capita',
                          'Gov_ex_3','Log_gov_ex_3', 'GDP_1', 'GDP_growth_1', 'GDP_capita_1', 'GDP_capita_growth_1'))

country_dt <- split(dt, f=dt$`Country Name`)

############## making a pisa score % change variable year on year ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$PISA_math_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$PISA_math_new[i]) && !is.na(country_dt[[m]]$PISA_math_new[i+1])){
            country_dt[[m]]$PISA_math_change[i+1] = (country_dt[[m]]$PISA_math_new[i+1]-country_dt[[m]]$PISA_math_new[i])/country_dt[[m]]$PISA_math_new[i]
        }
    }
    country_dt[[m]]$PISA_sci_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$PISA_sci_new[i]) && !is.na(country_dt[[m]]$PISA_sci_new[i+1])){
            country_dt[[m]]$PISA_sci_change[i+1] = (country_dt[[m]]$PISA_sci_new[i+1]-country_dt[[m]]$PISA_sci_new[i])/country_dt[[m]]$PISA_sci_new[i]
        }
    }
    country_dt[[m]]$PISA_read_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$PISA_read_new[i]) && !is.na(country_dt[[m]]$PISA_read_new[i+1])){
            country_dt[[m]]$PISA_read_change[i+1] = (country_dt[[m]]$PISA_read_new[i+1]-country_dt[[m]]$PISA_read_new[i])/country_dt[[m]]$PISA_read_new[i]
        }
    }
}

############## lagging pisa score % change by 3 years ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$PISA_math_change_3 = NA
    for (i in 1:(nrow(country_dt[[m]])-3)){
        country_dt[[m]]$PISA_math_change_3[i+3] = country_dt[[m]]$PISA_math_change[i]
    }
    country_dt[[m]]$PISA_sci_change_3 = NA
    for (i in 1:(nrow(country_dt[[m]])-3)){
        country_dt[[m]]$PISA_sci_change_3[i+3] = country_dt[[m]]$PISA_sci_change[i]
    }
    country_dt[[m]]$PISA_read_change_3 = NA
    for (i in 1:(nrow(country_dt[[m]])-3)){
        country_dt[[m]]$PISA_read_change_3[i+3] = country_dt[[m]]$PISA_read_change[i]
    }
}

############## lagging pisa scores by 3 years ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$PISA_math_new_3 = NA
    for (i in 1:(nrow(country_dt[[m]])-3)){
        country_dt[[m]]$PISA_math_new_3[i+3] = country_dt[[m]]$PISA_math_new[i]
    }
    country_dt[[m]]$PISA_sci_new_3 = NA
    for (i in 1:(nrow(country_dt[[m]])-3)){
        country_dt[[m]]$PISA_sci_new_3[i+3] = country_dt[[m]]$PISA_sci_new[i]
    }
    country_dt[[m]]$PISA_read_new_3 = NA
    for (i in 1:(nrow(country_dt[[m]])-3)){
        country_dt[[m]]$PISA_read_new_3[i+3] = country_dt[[m]]$PISA_read_new[i]
    }
}

############## bring gdp variables up by 1 year ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$GDP_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_1[i-1] = country_dt[[m]]$`GDP`[i]
    }
    country_dt[[m]]$GDP_growth_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_growth_1[i-1] = country_dt[[m]]$`GDP_growth`[i]
    }
    country_dt[[m]]$GDP_capita_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_capita_1[i-1] = country_dt[[m]]$`GDP_capita`[i]
    }
    country_dt[[m]]$GDP_capita_growth_1 = NA
    for (i in 2:(nrow(country_dt[[m]]))){
        country_dt[[m]]$GDP_capita_growth_1[i-1] = country_dt[[m]]$`GDP_capita_growth`[i]
    }
}

############## % change for economic variables ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$Trade_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Trade[i]) && !is.na(country_dt[[m]]$Trade[i+1])){
            country_dt[[m]]$Trade_change[i+1] = (country_dt[[m]]$Trade[i+1]-country_dt[[m]]$Trade[i])/country_dt[[m]]$Trade[i]
        }
    }
    country_dt[[m]]$Life_expectancy_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Life_expectancy[i]) && !is.na(country_dt[[m]]$Life_expectancy[i+1])){
            country_dt[[m]]$Life_expectancy_change[i+1] = (country_dt[[m]]$Life_expectancy[i+1]-country_dt[[m]]$Life_expectancy[i])/country_dt[[m]]$Life_expectancy[i]
        }
    }
    country_dt[[m]]$Gross_capital_formation_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Gross_capital_formation[i]) && !is.na(country_dt[[m]]$Gross_capital_formation[i+1])){
            country_dt[[m]]$Gross_capital_formation_change[i+1] = (country_dt[[m]]$Gross_capital_formation[i+1]-country_dt[[m]]$Gross_capital_formation[i])/country_dt[[m]]$Gross_capital_formation[i]
        }
    }
    country_dt[[m]]$Govt_expenditure_total_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Govt_expenditure_total[i]) && !is.na(country_dt[[m]]$Govt_expenditure_total[i+1])){
            country_dt[[m]]$Govt_expenditure_total_change[i+1] = (country_dt[[m]]$Govt_expenditure_total[i+1]-country_dt[[m]]$Govt_expenditure_total[i])/country_dt[[m]]$Govt_expenditure_total[i]
        }
    }
}

############## % change for school expectancy ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$Pre_pri_sch_expectancy_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Pre_pri_sch_expectancy[i]) && !is.na(country_dt[[m]]$Pre_pri_sch_expectancy[i+1])){
            country_dt[[m]]$Pre_pri_sch_expectancy_change[i+1] = (country_dt[[m]]$Pre_pri_sch_expectancy[i+1]-country_dt[[m]]$Pre_pri_sch_expectancy[i])/country_dt[[m]]$Pre_pri_sch_expectancy[i]
        }
    }
    country_dt[[m]]$Pri_sch_expectancy_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Pri_sch_expectancy[i]) && !is.na(country_dt[[m]]$Pri_sch_expectancy[i+1])){
            country_dt[[m]]$Pri_sch_expectancy_change[i+1] = (country_dt[[m]]$Pri_sch_expectancy[i+1]-country_dt[[m]]$Pri_sch_expectancy[i])/country_dt[[m]]$Pri_sch_expectancy[i]
        }
    }
    country_dt[[m]]$Sec_sch_expectancy_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Sec_sch_expectancy[i]) && !is.na(country_dt[[m]]$Sec_sch_expectancy[i+1])){
            country_dt[[m]]$Sec_sch_expectancy_change[i+1] = (country_dt[[m]]$Sec_sch_expectancy[i+1]-country_dt[[m]]$Sec_sch_expectancy[i])/country_dt[[m]]$Sec_sch_expectancy[i]
        }
    }
    country_dt[[m]]$Pri_sec_sch_expectancy_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Pri_sec_sch_expectancy[i]) && !is.na(country_dt[[m]]$Pri_sec_sch_expectancy[i+1])){
            country_dt[[m]]$Pri_sec_sch_expectancy_change[i+1] = (country_dt[[m]]$Pri_sec_sch_expectancy[i+1]-country_dt[[m]]$Pri_sec_sch_expectancy[i])/country_dt[[m]]$Pri_sec_sch_expectancy[i]
        }
    }
}

############## % change for educational expenditure ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$Gov_ex_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Gov_ex[i]) && !is.na(country_dt[[m]]$Gov_ex[i+1])){
            country_dt[[m]]$Gov_ex_change[i+1] = (country_dt[[m]]$Gov_ex[i+1]-country_dt[[m]]$Gov_ex[i])/country_dt[[m]]$Gov_ex[i]
        }
    }
    country_dt[[m]]$Gov_ex_percent_GDP_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Gov_ex_percent_GDP[i]) && !is.na(country_dt[[m]]$Gov_ex_percent_GDP[i+1])){
            country_dt[[m]]$Gov_ex_percent_GDP_change[i+1] = (country_dt[[m]]$Gov_ex_percent_GDP[i+1]-country_dt[[m]]$Gov_ex_percent_GDP[i])/country_dt[[m]]$Gov_ex_percent_GDP[i]
        }
    }
    country_dt[[m]]$Population_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Population[i]) && !is.na(country_dt[[m]]$Population[i+1])){
            country_dt[[m]]$Population_change[i+1] = (country_dt[[m]]$Population[i+1]-country_dt[[m]]$Population[i])/country_dt[[m]]$Population[i]
        }
    }
}

############## % change for enrolment num ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$Pre_pri_enrolment_num_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Pre_pri_enrolment_num[i]) && !is.na(country_dt[[m]]$Pre_pri_enrolment_num[i+1])){
            country_dt[[m]]$Pre_pri_enrolment_num_change[i+1] = (country_dt[[m]]$Pre_pri_enrolment_num[i+1]-country_dt[[m]]$Pre_pri_enrolment_num[i])/country_dt[[m]]$Pre_pri_enrolment_num[i]
        }
    }
    country_dt[[m]]$Pri_enrolment_num_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Pri_enrolment_num[i]) && !is.na(country_dt[[m]]$Pri_enrolment_num[i+1])){
            country_dt[[m]]$Pri_enrolment_num_change[i+1] = (country_dt[[m]]$Pri_enrolment_num[i+1]-country_dt[[m]]$Pri_enrolment_num[i])/country_dt[[m]]$Pri_enrolment_num[i]
        }
    }
    country_dt[[m]]$Sec_enrolment_num_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Sec_enrolment_num[i]) && !is.na(country_dt[[m]]$Sec_enrolment_num[i+1])){
            country_dt[[m]]$Sec_enrolment_num_change[i+1] = (country_dt[[m]]$Sec_enrolment_num[i+1]-country_dt[[m]]$Sec_enrolment_num[i])/country_dt[[m]]$Sec_enrolment_num[i]
        }
    }
    country_dt[[m]]$Ter_enrolment_num_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Ter_enrolment_num[i]) && !is.na(country_dt[[m]]$Ter_enrolment_num[i+1])){
            country_dt[[m]]$Ter_enrolment_num_change[i+1] = (country_dt[[m]]$Ter_enrolment_num[i+1]-country_dt[[m]]$Ter_enrolment_num[i])/country_dt[[m]]$Ter_enrolment_num[i]
        }
    }
}

############## % change for enrolment ratio ##############
for (m in 1:length(country_dt)){
    country_dt[[m]]$Pre_pri_enrolment_ratio_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Pre_pri_enrolment_ratio[i]) && !is.na(country_dt[[m]]$Pre_pri_enrolment_ratio[i+1])){
            country_dt[[m]]$Pre_pri_enrolment_ratio_change[i+1] = (country_dt[[m]]$Pre_pri_enrolment_ratio[i+1]-country_dt[[m]]$Pre_pri_enrolment_ratio[i])/country_dt[[m]]$Pre_pri_enrolment_ratio[i]
        }
    }
    country_dt[[m]]$Pri_enrolment_ratio_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Pri_enrolment_ratio[i]) && !is.na(country_dt[[m]]$Pri_enrolment_ratio[i+1])){
            country_dt[[m]]$Pri_enrolment_ratio_change[i+1] = (country_dt[[m]]$Pri_enrolment_ratio[i+1]-country_dt[[m]]$Pri_enrolment_ratio[i])/country_dt[[m]]$Pri_enrolment_ratio[i]
        }
    }
    country_dt[[m]]$Sec_enrolment_ratio_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Sec_enrolment_ratio[i]) && !is.na(country_dt[[m]]$Sec_enrolment_ratio[i+1])){
            country_dt[[m]]$Sec_enrolment_ratio_change[i+1] = (country_dt[[m]]$Sec_enrolment_ratio[i+1]-country_dt[[m]]$Sec_enrolment_ratio[i])/country_dt[[m]]$Sec_enrolment_ratio[i]
        }
    }
    country_dt[[m]]$Ter_enrolment_num_change = NA
    for (i in 1:(nrow(country_dt[[m]])-1)){
        if (!is.na(country_dt[[m]]$Ter_enrolment_num[i]) && !is.na(country_dt[[m]]$Ter_enrolment_num[i+1])){
            country_dt[[m]]$Ter_enrolment_num_change[i+1] = (country_dt[[m]]$Ter_enrolment_num[i+1]-country_dt[[m]]$Ter_enrolment_num[i])/country_dt[[m]]$Ter_enrolment_num[i]
        }
    }
}

############## merging all the separate data tables back ##############
new_dt <- country_dt[[1]]
for (i in 2:length(country_dt)){
    new_dt <- rbind(new_dt, country_dt[[i]])
}

############## formatting all exponents to numeric ##############
new_dt$GDP_1                            <- as.numeric(as.character(new_dt$GDP_1))
new_dt$GDP_capita_1                     <- as.numeric(as.character(new_dt$GDP_capita_1))
new_dt$GDP_growth_1                     <- as.numeric(as.character(new_dt$GDP_growth_1))
new_dt$GDP_capita_growth_1              <- as.numeric(as.character(new_dt$GDP_capita_growth_1))
new_dt$Trade_change                     <- as.numeric(as.character(new_dt$Trade_change ))
new_dt$Life_expectancy_change           <- as.numeric(as.character(new_dt$Life_expectancy_change ))
new_dt$Gross_capital_formation_change   <- as.numeric(as.character(new_dt$Gross_capital_formation_change))
new_dt$Govt_expenditure_total_change    <- as.numeric(as.character(new_dt$Govt_expenditure_total_change ))

############## dropping those non-%change variables ##############
new_dt <- new_dt[,-c(1:29)]

write.csv(new_dt, "GDP_INPUT_ALL_P.csv", row.names=FALSE)

rm(list = ls())

################ END PART 10: DATA PREPARATION FOR % CHANGE OF VARS ###################

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>> end of percentage change calculations and end of part 10 >>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>> continued in the analytics Rscript >>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >7>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #

# END DATA CLEANING