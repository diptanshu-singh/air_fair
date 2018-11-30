# air_fair

This analysis hopes to anser the question, whether it is the right time to book the flight

Data: 
The data has been scrapped from expedia. Current analysis focuses on one flight route which had a lot of variance in fare on consecutive days. 

Data has been pulled for the coming 3 months continuously for the coming 14 days. Daily trends have been analysed. Using these, we will be analysing what is the risk or reward for waiting for n_days. In time and out of time validation will be done to optimise the app. 

Data for LAX airport had been taken from a Kaggle dataset to understand the traffic in previous year on those dates

Factors in data : 
Day of the weeek 
Carrier 
Traffic on the airport on those days
N_days left for the flight 

Final Output: 
The aim is to build a shiny web app which shows whats the best day to book the ticket for you in the coming days 
