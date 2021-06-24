#### <h4> Customer-Analytics-for-Online-Music-Streaming-Service
Introduction
###### <h6> The “freemium” business model — widely used by online services such as LinkedIn, Match.com, Dropbox, and music-listening sites — divides user populations into groups that use the service for free and groups that pay a fee for additional features. Key points related to the freemium model: 
###### <h6> Free accounts are monetized using online advertising.
###### <h6> Premium subscribers (those paying a fee) are typically 24 times more profitable than free users. However, premium subscribers are rare.
  
###### <h6> Some useful background information in the following paper:
###### <h6> https://www.dropbox.com/s/0elh9skbuys9ytw/oestreicher-singer%20%26%20zalmanson%202013-misq.pdf?dl=0 (Links to an external site.)

###### <h6> High Note is an anonymized real music streaming company --- similar to Last.fm, Spotify or Pandora --- that uses a freemium business model. Information on High Note customer data, including:  
###### <h6> Demographic characteristics such as age, gender and country.
###### <h6> Social network characteristics such as number of friends a user has on the network.
###### <h6> Engagement level data on activities performed when using the service, which include the number of songs the user has listened to, playlists created, “shouts” sent to friends, etc.

Objective 
###### <h6> Given the higher profitability of premium subscribers, it is generally in the interest of company to motivate users to go from “free to fee”; that is, convert free accounts to premium subscribers. The task in regards to this case is to analyze the data for potential insight to inform a “free-to-fee” strategy. 
###### <h6> Literature shows that peer influence and user engagement can affect users’ decisions to pay for a premium subscription. Using the High Note data, we predict the decision to buy using both types of variables. The results will quantify the effect of social engagement on revenue, as well as how valuable a premium subscriber can be in a freemium social community. 

###### <h6> Summary statistics: Generate descriptive statistics for the key variables in the data set. Analyze the differences in the mean values of the variables, comparing the adopter and non-adapter subsamples. Tentative conclusionsdrawn from these comparisons 
###### <h6> Data Visualization: Generate a set of charts (e.g., scatter plots, box plots, etc) to help visualize how adopters and non-adopters (of the premium subscription service) differ from each other in terms of (i) demographics, (ii) peer influence, and (iii) user engagement.
###### <h6> Propensity Score Matching (PSM):Use PSM to test whether having subscriber friends affects the likelihood of becoming an adopter (i.e., fee customer). For this purpose, the "treatment" group will be users that have one or more subscriber friends (subscriber_friend_cnt >= 1), while the "control" group will include users with zero subscriber friends. Use PSM to first create matched treatment and control samples, then test whether there is a significant average treatment effect. 
###### <h6> Regression Analyses: Now, we will use a logistic regression approach to test which variables (including subscriber friends) are significant for explaining the likelihood of becoming an adopter.
###### <h6> Takeaways: Discuss some key takeaways from the analysis. Specifically, how the results inform a “free-to-fee” strategy for High Note.
