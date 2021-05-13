# JusPayDatathon
Dashboard made using R, R shiny and R dashboard for JusPayDatathon <hr>
<h3>Providing an Idea for Automating the Process</h3>
<strong>Below is Data for UPI downtime only</strong><br><br>
Data for UPI only :- 
1. CountryDelight had (13th/18:00 - 14th/9:00) (13 hours of downtime)

2. Drivezy had (12th/20:00 - 12th/21:00, 12th/22:00 - 12th/23:00, 13th/5:00 - 13th/6:00, 13th/18:00 - 14th/9:00) (16 hours of downtime)

3. fanfight had (13th/3:00 - 13th/9:00, 13th/14:00 - 13th/16:00, 13th/17:00 - 13th/18:00, 13th/23:00 - 14th/00:00, 14th/17:00 - 14th/19:00, 14th/20:00 - 14th/21:00) (13 hours of downtime)

4. Medlife_prod had ( 13th/18:00 - 14th/08:00) (12 hours of downtime)

5. Pharmeasytech had (13th/8:00 - 13th/9:00, 13th/18:00 - 14th/12:00, 14th/19:00 - 14th/20:00) (18 hours of downtime)

6. purplle.com had (12th/11:00 - 12th/12:00, 12th/14:00 - 12th/15:00, 12th/19:00 - 12th/21:00, 13th/1:00 - 13th/2:00, 13th/3:00 - 13th/4:00, 13th/5:00 - 13th/9:00, 13th/12:00 - 13th/13:00, 13th/14:00 - 13th/15:00, 13th/20:00 - 13th/21:00, 14th/00:00 - 14th/3:00, 14th/19:00 - 14th/20:00, 14th/21:00) (18 hours of downtime)

7. UrbanClap had (12th/4:00 - 12th/6:00, 12th/19:00 - 12th/21:00, 12th/23:00 - 13th/2:00, 13th/8:00 - 13th/9:00, 13th/14:00 - 13th/16:00, 13th/19:00 - 13th/20:00, 13th/21:00 - 13th/22:00) (12 hours of downtime)

8. Zivame had (12th/00:00 - 12th/2:00, 12th/9:00 - 12/10:00, 12th/13:00 - 12th/14:00, 13th/10:00 - 13th/11:00, 13th/16:00 - 13th/17:00, 13th/18:00 - 13th:19:00, 14th/00:00 - 14th/1:00) (8 hours of downtime)

The reason as to why fanfight or purplle.com didn't got its UPI down in period 13th/18:00 - 14th/9:00 is beacause as it can be seen from graph problem occured in UPI (PAYTM_V2 gateway), Since value of other type of UPI remained high we got an averaged graph of UPI for fanfight and purplle (no distinction between payment gateways). This caused the down period to not get noticed. I have also attached a graph for the same in the code.

Downperiod for fanflight and purplle.com was noticed for UPI for PAYTM_V2 gateway in my code when distinction was made on basis of each combinations of mid, pg and pmt or specifically where pmt = UPI and different pg were taken into consideration.

The reason why UrbanClap and Zivame also saw a downfall was because the data was almost consistent this caused standard deviation to drop, which now meant that it would be more sensitive to errors which has resulted in the following problem.

A better approach might have been using 1.5 times or 2 times the std. It might solve most of cases but will still be susceptible to false alarm as seen in case of UrbanClap where most of data is constant. To solve this issue we can set up flags like if the difference is greater than 10% or 2/1.5 times std (whichever is larger).

Using 10% or some other constant can be problem when all values have lower accuracy to which the solution has been addressed below.

Current approach of using only standard deviation is not good since it ignore the following case of (Purplle.com, PAYTM & NB), (fanfight, razorpay, NB), (zaakpay,drivezy,NB), (there are many others) where the graph is 0 most of the time which isn't reported when we use std only.

To correct this we can use mean instead of median which might eliminate some of these issues, also note it would classify the above mentioned (Purplle.com, PAYTM & NB) and other such combinations(Refer Graph below) as low Success Rate most of time, which I think is good in our case here since we want our merchants and their customers to have seamless experience with high Success Rate not low Success Rate, so it shall be reported.

Using mean might solve some of the above problem, in such a case I think we shall also set up flags like if the accuracy is less than (15%) a level 1 flag can been raised to merchant, similarly if it drops down to (10%) a level 2 flag can be raised and so on. If its above our level 2/1 flag we use our normal means to calculate the error (using std2 or 1.5*std with mean).
