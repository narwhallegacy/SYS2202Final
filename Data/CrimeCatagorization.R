library("tidyverse")


#read in crime data
crime = read.csv("Sports_Crime.csv", header = TRUE)





#Read in specific crime types and then add to 'crimeCat' vector of broader catagories
Theft = ((crime$crime_type == 'Larceny - Shoplifitng') | (crime$crime_type == 'Burglary') | 
  (crime$crime_type == 'Larceny - All Other') | (crime$crime_type == 'Larceny - From Motor Vehicle') | (crime$crime_type == 'Larceny - Of Veh Parts/Access') | 
  (crime$crime_type == 'Larceny - Pocket Picking') | (crime$crime_type == 'Larceny - Theft from Building') | (crime$crime_type == 'Motor Vehicle Theft') | 
  (crime$crime_type == 'Larceny - Purse Snatching') | (crime$crime_type == 'Robbery - Strong Arm') | (crime$crime_type == 'Robbery/Conversion'))

crimeCat = factor(Theft, labels = c('', 'Theft'))

VehAccid = ((crime$crime_type == 'Accident - w/out Injuries') | (crime$crime_type == 'Hit and Run') | 
                      (crime$crime_type == 'Accident Property Damage') | (crime$crime_type == 'Accident w/Injuries'))

crimeCat = paste(crimeCat, factor(VehAccid, labels = c('', 'Vehicle Accident')), sep = "")

Animal = ((crime$crime_type == 'Animal - Bite') | (crime$crime_type == 'Animal Complaint'))

crimeCat = paste(crimeCat, factor(Animal, labels = c('', 'Animal-related')), sep = "")

Noncrim = ((crime$crime_type == 'Assist Agency - Backup/Assist') | (crime$crime_type == 'Assist Citizen - Medical') | 
             (crime$crime_type == 'Assist Citizen - Welfare Check') | (crime$crime_type == 'Assist Citizen - Mental/TDO/ECO') | (crime$crime_type == 'Found/Recovered Property') | 
             (crime$crime_type == 'Lost/FoundProperty') | (crime$crime_type == 'Missing Person-adult') | (crime$crime_type == 'Missing Person-juvenile') | 
             (crime$crime_type == 'Comm Relations Initiative - CRI') | (crime$crime_type == 'Misc - Non-Criminal Call') | (crime$crime_type == 'Fires Not Arson'))

crimeCat = paste(crimeCat, factor(Noncrim, labels = c('', 'Non-criminal report')), sep = "")

DisCon = ((crime$crime_type == 'Disorderly Conduct') | (crime$crime_type == 'Disturbance - Non Domestic') | 
            (crime$crime_type == 'Domestic Disturbance') | (crime$crime_type == 'Shots Fired/Illegal Hunting') | (crime$crime_type == 'Suspicious Activity') | 
            (crime$crime_type == 'Suspicious Person') | (crime$crime_type == 'Trespass') | (crime$crime_type == 'Vandalism'))

crimeCat = paste(crimeCat, factor(DisCon, labels = c('', 'Disorderly Conduct')), sep = "")

SubAbuse = ((crime$crime_type == 'Driving Under the Influence') | (crime$crime_type == 'Drug Investigation') | 
              (crime$crime_type == 'Drug/Narcotics Violation') | (crime$crime_type == 'Drunkeness DIP') | (crime$crime_type == 'Liquor Law Violation') | 
              (crime$crime_type == 'Narcotics'))

crimeCat = paste(crimeCat, factor(SubAbuse, labels = c('', 'Substance Abuse')), sep = "")

Fraud = ((crime$crime_type == 'Embezzelment') | (crime$crime_type == 'Forgery/Counterfeiting') | 
           (crime$crime_type == 'Fraud-credit card') | (crime$crime_type == 'Fraud-false pretense') | (crime$crime_type == 'Fraud-impersonation') | 
           (crime$crime_type == 'Fraud-wire fraud'))

crimeCat = paste(crimeCat, factor(Fraud, labels = c('', 'Fraud')), sep = "")

AssThreat = ((crime$crime_type == 'Extortion/Blackmail') | (crime$crime_type == 'Harassment') | 
            (crime$crime_type == 'Phone Calls - Threat or Obscene') | (crime$crime_type == 'Assault Aggravated') | (crime$crime_type == 'Assault Intimidation') | (crime$crime_type == 'Assault Simple'))

crimeCat = paste(crimeCat, factor(AssThreat, labels = c('', 'Threat and/or Assault')), sep = "")

sexOff = ((crime$crime_type == 'Sex Offense') | (crime$crime_type == 'Sex Offense - Forcible Rape') | 
            (crime$crime_type == 'Sex Offense - Forcible Sodomy') | (crime$crime_type == 'Sex Offense-forcible fondling'))

crimeCat = paste(crimeCat, factor(sexOff, labels = c('', 'Sex Offense')), sep = "")

other = ((crime$crime_type == 'Death Investigation - DOA') | (crime$crime_type == 'Runaway') | (crime$crime_type == "Misc - Criminal Call") | 
           (crime$crime_type == 'Solicitation illegal w/o permit') | (crime$crime_type == 'Towed Vehicle') | (crime$crime_type == 'Traffic Stops') | 
           (crime$crime_type == 'Unauthorized Use of Motor Veh') | (crime$crime_type == 'Warrant Service') | (crime$crime_type == 'Weapons Violations') |
          (crime$crime_type == 'Kidnap/Abduction'))

crimeCat = paste(crimeCat, factor(other, labels = c('', 'Other')), sep = "")

crime = data.frame(crime,crimeCat)
names(crime)[10] = 'Crime Category'

write.csv(crime, 'crime_data_categorized.csv')
