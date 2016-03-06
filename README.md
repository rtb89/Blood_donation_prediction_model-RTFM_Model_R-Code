Blood_donation_prediction_model-RTFM_Model_R-Code
A study of probability of Blood Donation in voluntary Donors-Building a prediction model for the blood donation in regular donors based on their historic data.

The data set I used is available on the web. 

It is a data set from donor database of Blood Transfusion Service Center in Hsin-Chu City in Taiwan. The center passes their blood transfusion service bus to one university in Hsin-Chu City to gather blood donated about every three months. The data was donated by Yeh, I-Cheng and contain data for 748 donors.

The logistic regression equation as:

log⁡(odds)=-1.0668-0.0794*(Recency)+1.1*log⁡(Frequency)-0.034*Time+0.32*RBD 

 
It can be interpreted as, a unit increase in Recency (1 month) decreases the log of odds by 0.079 while keeping the other factors unchanged. Similarly, a unit increase in Time (1 months) decrease the log of odds by 0.034.

