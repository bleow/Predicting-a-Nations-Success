####################################### BC2406 ANALYTICS I #######################################
######################################## SEMINAR 2 TEAM 7 ########################################
############################################# WEEK 4 #############################################

library(data.table)     #Read csv as data table
library(ggplot2)        #visualisation

dt = fread('Lawsuit.csv')   

# convert dept, gender, clin, cert and rank to factor
dt$Dept = factor(dt$Dept)
dt$Gender = factor(dt$Gender)
dt$Clin = factor(dt$Clin)
dt$Cert = factor(dt$Cert)
dt$Rank = factor(dt$Rank)

# rename the number levels to text
levels(dt$Gender) = c("Female", "Male")
levels(dt$Clin) = c("Clinical", "Research")
levels(dt$Rank) = c("Asst Prof", "Assoc Prof", "Full Prof")
levels(dt$Dept) = c("Biochemistry/Molecular Biology", "Physiology", "Genetics", "Pediatrics", "Medicine", "Surgery")
levels(dt$Cert) = c("Not Certified", "Certified")


#Show whole number instead of scientific notation
getOption("scipen")        
options("scipen" = 100)



################ SLIDE 3 // BOXPLOT // Salary in 1994 against Gender ################
ggplot(data = dt, mapping = aes(x = Gender, y = Sal94, fill=Gender)) +
    geom_boxplot(show.legend = TRUE) +
    labs(title = "Salary in 1994 vs Gender", y="Salary in 1994")       #rename y axis and add title



################ SLIDE 3 // BOXPLOT // Salary against Year grouped by Gender ################
# transform data to proper "records" data format by unpivoting Sal94/Sal95. this will result in one "Sal94" salary column, and one "year" year column.
# Step 1> create two temporary tables for 1994 and 1995
salIncrease94 = subset(dt, select=-c(Sal95)); salIncrease94$year = "1994"
salIncrease95 = subset(dt, select=-c(Sal94)); salIncrease95$year = "1995"
# Step 2> make column names the same to union the table
colnames(salIncrease95) = colnames(salIncrease94)
# Step 3> union the two tables
salIncrease = rbind(salIncrease94, salIncrease95)
## Step 4> convert to date datatype for time intelligence <-- if drawing line chart, need this to obtain time intelligence. since we are using faceting, no longer need this column to be of date datatype
#salIncrease$year = as.Date(salIncrease$year)
# Step 5> drop the two temporary tables
rm(salIncrease94, salIncrease95)

# calculate the means to layer onto the ggplot manually
# Step 1> rename the levels from 0 and 1 to Female and Male (for the legend)
# Step 2> calculate the means for female and male
meansF <- aggregate(Sal94 ~ year, data = salIncrease[Gender=="Female"], mean)
meansF$Gender = "Female"
meansF$Gender = as.factor(meansF$Gender)
meansM <- aggregate(Sal94 ~ year, data = salIncrease[Gender=="Male"], mean)
meansM$Gender = "Male"
meansM$Gender = as.factor(meansM$Gender)

# plot the graph
ggplot(salIncrease, mapping = aes(x = Gender, y = Sal94, color=Gender))+
    geom_boxplot()+
    labs(y = "Salary", title = "Salary vs Year by Gender")+
    geom_text(meansF, mapping = aes(label=round(Sal94,0)), position = position_nudge(y=5000), show.legend=FALSE)+ #show female mean label
    geom_text(meansM, mapping = aes(label=round(Sal94,0)), position = position_nudge(y=-5000), show.legend=FALSE)+ # show male mean label
    facet_grid(. ~ year) #facet by year to show year-on-year increase.



################ SLIDE 4 // SCATTER PLOT + LINE CHART // Salary against Experience grouped by Rank ################
# Plot of Years of Experience (x-axis) against Salary (y-axis), with geom_smooth layer to show general trend
# Coloured by gender to show difference between the general trend of both genders
ggplot(dt) + aes(x=Exper, y=Sal94) + geom_point(aes(colour = Gender)) + scale_y_continuous(labels=scales::dollar_format()) + labs(x = "Years of Experience", y = "Salary", title = "Salary vs Experience by Gender") + geom_smooth(aes(x = Exper, y = Sal94, colour=Gender), alpha=0.2, method = 'glm', se = FALSE)



################ SLIDE 5 // BOXPLOT + LINE CHART // Salary  ################
#Plot of Experience (x-axis) vs Salary (y-axis), grouped by gender, faceted by rank, with geom_smooth layer to represent the general trend
ggplot(dt) + aes(x=Exper, y=Sal94) + geom_boxplot(aes(colour = Gender)) + facet_wrap(~Rank) + scale_y_continuous(labels=scales::dollar_format()) + labs(x = "Years of Experience", y = "Salary", title = "Salary vs Experience by Rank and Gender") + geom_smooth(aes(x = Exper, y = Sal94, colour=Gender), alpha=0.2, method = 'glm', se = FALSE)



################ SLIDE 6 // BAR CHART // Proportion of Full Professors grouped by Department ################
#Plot the number of full professors by gender according to departments
ggplot(data = dt[Rank=="Full Prof"], mapping = aes(x = Gender, y = ..count.., fill=Gender)) +  
    geom_bar()+ labs(title = "Number of Full Professors by Gender")+
    facet_grid(. ~ Dept)



################ SLIDE 7 // STACKED BAR CHART // Overall Porportion of Professors' Rank grouped by Certification ################
# Plotting Number of Professors in each rank by certification according to gender
# Chose this particular colour template to show emphasis on Asst and Full Profs
ggplot(dt) + aes(x=Gender) + geom_bar(aes(fill = Rank), position = 'stack') + facet_wrap(~Cert) + labs(x = "Certification", y = "Number of Profs", title = "Number of Professors in each Rank by Certification") + scale_fill_brewer(palette="BrBG")



################ SLIDE 8 // STACKED BAR CHART // Porportion of Professors' Rank grouped by Departments  ################
#Stacked filled percentage bar plot showing that there is a higher percentage of male full professors and higher percentage of female assistant professors
#Chose this particular colour template to show emphasis on Asst and Full Profs
ggplot(dt) + aes(x=Gender) + geom_bar(aes(fill = Rank), position = 'fill') + facet_wrap(~Dept)  + scale_y_continuous(labels = scales::percent) + labs(x = "Rank", y = "% of Profs ", title = "Percentage of Professors in each Rank by Dept") + scale_fill_brewer(palette="BrBG")

