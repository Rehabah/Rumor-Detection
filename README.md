# Rumor-Detection
A classifier that can detect rumors in Arabic tweets based on features' values that had been extracted from tweets.

# Functionality and Design
The process of designing rumors detection classifier can be divided into five basic categories ,they are as follows:
1. Data Collection (from twitter).
2. Data Preprocessing.
3. Data Labeling.
4. Feature Extraction.
5. Classification.

# Dataset
The following features were extracted from a user, account, and tweet's content which will be used to train the model. The features were chosen based on some
previous studies on detecting rumors' tweets and empirical analysis that was performed on a sample set of Saudi rumors' tweets that were collected from trending
hashtags.

# The User Based Features

o User Name: a name of the user that was written in account profile (personal, organization).

# Account Based Features

o Verified: account is authentic, it will have a blue badge appears next to the name on account’s profile. It has two values either True or False.

o Account protection: it has two values True or False.

o Account Age: the time passed since the user created the account.

o Description: some information was written in account's bio.

o Location: indicates the location of the user that was entered in his/her profile.

o Number of Followers: the number of people who are following and reading the user's tweets.

o Numbers of Friends: the number of people who the user is following them.

o Number of Statuses: the number of tweets that the user posted.

o Favorites Number: the number of tweets the user liked.

o Number of Lists: the number of lists that user belongs to them.

o Account Name (Screen Name): the name that was written after @.

# Content-Based Features

o Question Mark: checking the content if includes (?) or not.

o URL: the tweet includes URL and the numbers of it.

o Multimedia: the tweet includes images, videos, and the number of them.

o Hashtag: the tweet includes hashtags and numbers of it.

o Replay/post/re-tweet: to know if the tweet is replied tweet or posted tweet by user or is re-tweeted by the user.

o Number of re-tweets: the number of re-tweets for the tweet.

o Number of likes: the number of likes for the tweet.

o Engagement Score: the number of tweets divided by the number of days since the user account creation.

o Time Span: the difference between the date of tweet and the account registration date.

o Phone Number: tweet includes a phone number that started with (05).

# Extra features
o Number of Statues / account age

o Number of followers / number of friends

o Number of favorites / number of statues



