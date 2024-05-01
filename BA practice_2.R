###PART 1
library(rvest)

# web url
freelancer = read_html("https://www.freelancer.com/jobs/data-entry/")

# product
title = html_nodes(freelancer, ".JobSearchCard-primary-heading") %>% html_text() 
title <- gsub("\\n", "", title)
title <- gsub("\\s*6 days left\\s*", "", title)
title <- gsub("\\s*VERIFIED\\s*", "", title)
title <- gsub("\\s{2,}", " ", title)
title


#description
description = html_nodes(freelancer, ".JobSearchCard-primary-description") %>% html_text() 
description <- gsub("\\n", "", description)
description <- gsub("\n", "", description)
description <- gsub("\\s+", " ", description)
description

#tags 
tags = html_nodes(freelancer, ".JobSearchCard-primary-tags") %>% html_text() 
tags <- gsub("\\n", "", tags)
tags <- gsub("\\s{2,}", " ", tags)
tags

#price
price = html_nodes(freelancer, ".JobSearchCard-secondary-price") %>% html_text()
price <- as.numeric(gsub("[^0-9.]+", "", price))
price

#하나의 dataframe으로 합치기
freelancer = data.frame(title, description, tags, price)
head(freelancer)


###PART2
fintech=read.csv("C:/Users/82107/OneDrive/문서/2024-1/fintech.csv")
str(fintech)
head(fintech)

#Q1 avg month income / avg month income of the currently employed
avg_income <- mean(fintech$monthly_income)
avg_income

employed <- fintech[fintech$currently_employed == "t", ]
employed_avg_income <- mean(employed$monthly_income, na.rm = TRUE)
employed_avg_income 

#Q2 loan amount histogram
hist(fintech$loan_amount, 
     col = 'lightblue', 
     main ="Histogram of Loan Amount", 
     xlab = "loan amount", 
     ylab= "frequency" )

#Q3 friends_facebook 0->NA avg # of facebook friends among facebook account provided
fintech$friends_facebook[fintech$friends_facebook == 0] <- NULL
numeric_friends_facebook <- as.numeric(fintech$friends_facebook)
numeric_friends_facebook

avg_facebook_friends<- mean(numeric_friends_facebook, na.rm = TRUE)
avg_facebook_friends

#Q4 scatter plot of month_of_service, credit_score / monthly_income, credit_score -> relationship, cor test
plot(x=fintech$month_of_service, y=fintech$credit_score, 
     col = 'lightgreen', 
     main = 'correlation between employment period and credit score',
     xlab = 'employment period',
     ylab = 'credit score')

cor(fintech$month_of_service, fintech$credit_score)

plot(x=fintech$monthly_income, y=fintech$credit_score,
     col = 'lightgreen', 
     main = 'correlation between monthly income and credit score',
     xlab = 'monthly income',
     ylab = 'credit score')

cor(fintech$monthly_income, fintech$credit_score)

#Q5 automatic_approved (t-approved by the decision engine / f-rejected / NA-reviewed manually) -> count
fintech$automatic_approved <- NA

fintech$automatic_approved = 
  ifelse(fintech$approved=="t" & is.na(fintech$manual_approved)==TRUE, 
         "t", fintech$automatic_approved)

fintech$automatic_approved =
  ifelse(fintech$approved=="f" & is.na(fintech$manual_approved)==TRUE, 
         "f", fintech$automatic_approved)

fintech$automatic_approved

table(fintech$automatic_approved)
manual_review <- sum(is.na(fintech$automatic_approved))
manual_review

#Q6 automatically approved and rejected
#1 loan amount
t.test(loan_amount ~ automatic_approved, data = fintech)

#2 tenor
t.test(tenor ~ automatic_approved, data = fintech)

#3 age
t.test(age ~ automatic_approved, data = fintech)

#4 month_of_service
t.test(month_of_service ~ automatic_approved, data = fintech)

#5 residential_status
residential_status_table <- table(fintech$residential_status, fintech$automatic_approved)
chisq.test(residential_status_table)

#6 monthly_income
t.test(monthly_income ~ automatic_approved, data = fintech)

#7 bankrupted
bankrupted_table <- table(fintech$bankrupted, fintech$automatic_approved)
chisq.test(bankrupted_table)

#8 currently_employed
currently_employed_table <- table(fintech$currently_employed, fintech$automatic_approved)
chisq.test(currently_employed_table)

#9 channel
channel_table <- table(fintech$channel, fintech$automatic_approved)
chisq.test(channel_table)

#10 language
language_table <- table(fintech$language, fintech$automatic_approved)
chisq.test(language_table)

#11 credit_score
t.test(credit_score ~ automatic_approved, data = fintech)

#12 friends_facebook
t.test(numeric_friends_facebook ~ automatic_approved, data= fintech)

#13 location_application
location_application_table <- table(fintech$location_application, fintech$automatic_approved)
chisq.test(location_application_table)
  