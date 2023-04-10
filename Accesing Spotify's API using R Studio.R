#Project 1-STQD6114

##Task 2

#1. Find any website that have multiple pages regarding one of the following:
  #i. Online purchase website (example, Amazon, lazada, etc). Select two different products with the 
  #same categories (example, Sneekers and high heels).
  #ii. Movies of two different genres.
  #iii. Songs from two different artists. The artists must have produced at least 20 songs.
  #iv. Providers of two different services/industries.

#iii

#Load the packages
library(spotifyr)
library(devtools)
library(dplyr)
library(plotly)
library(ggplot2)

#Development version

devtools::install_github('charlie86/spotifyr')

#Authentication
Sys.setenv(SPOTIFY_CLIENT_ID = '7a17c3182380409cb97c220aebf95a1b')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'bc38e38a2bb64b8790c5df7979062ac8')
access_token <- get_spotify_access_token()

##Justin Bieber 
Justin_Bieber <- get_artist_audio_features('Justin Bieber')
head(Justin_Bieber)

##Justin Bieber top 10 tracks
top_Justin_Bieber<-get_artist_top_tracks('1uNFoZAHBGtllmzznpCI3s')
top_Justin_Bieber$name

##Justin Bieber related artist(same genre)
Justin_Bieber_related<-get_related_artists('1uNFoZAHBGtllmzznpCI3s')
head(Justin_Bieber_related %>% select(name,genres))
#Comment: Shawn Mendes was listed as one of related artist with Justin Bieber because most of their songs in same genres


##Shawn Mendes
Shawn_Mendes <- get_artist_audio_features('Shawn Mendes')
head(Shawn_Mendes)

##Shawn Mendes top 10 tracks
top_Shawn_Mendes<-get_artist_top_tracks('7n2wHs1TKAczGzO7Dd2rGr')

##Shawn Mendes related artist(same genre)
Shawn_Mendes_related<-get_related_artists('7n2wHs1TKAczGzO7Dd2rGr')
head(Shawn_Mendes_related$name)


#MytopArtist

my_top_artist<-get_my_top_artists_or_tracks(
type = "artists",
limit = 20,
offset = 0,
time_range = "medium_term",
authorization = get_spotify_authorization_code(),
include_meta_info = FALSE)

unique(Justin_Bieber$key_name)
unique(Justin_Bieber$track_uri)


##Combine 2 data frames JB & SM

JB_SM <- rbind(Justin_Bieber,Shawn_Mendes)
str(JB_SM)


is.na(JB_SM)
#create new column to calculate difference of speech
JB_SM <- JB_SM%>%
  mutate(difference=speechiness-0.33)

#plot cplours
#to see how far each bar goes above or below zero
pink <- "#ff6f59"
blue <- "#17bebb"
?reorder
names(JB_SM)

str(JB_SM)

##Plot
viz1 <- ggplot(JB_SM, aes(x=reorder(track_name, -difference), y=difference, fill=artist_name, text=(paste("Track:", track_name, "<br>",
                                                                                                              "Artist:", artist_name, "<br>",
                                                                                                              "Speechiness:", speechiness))))+
  geom_col()+
  scale_fill_manual(values=c(pink, blue))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        legend.position="none")+
  ylab("Speechiness Difference")+
  facet_wrap(~ artist_name)+
  ggtitle("Speechiness Difference")

ggplotly(viz1, tooltip=c("text"))


##Key_name variable analysis
#how many songs from each artist are in certain keys
#the total number of songs in each key
#the percentage of songs in each key that come from each artist.


key_artist <- JB_SM%>%
  select(artist_name, key_name)%>%
  group_by(artist_name, key_name)%>%
  mutate(n=n())%>%
  unique()%>%
  group_by(key_name)%>%
  mutate(total=sum(n))%>%
  mutate(percent=round((n/total)*100))

head(key_artist, 10)

#Using ggplot2 and plotly, I represented key as a geom_bar in two different graphs. 
#In the first, I used position="fill" to show the percentage of songs in each key that come from each country. 
#My second graph is almost exactly the same but does not use position="fill".


green <- "#1ed760"
yellow <- "#e7e247"

#with position="fill"
viz2 <- ggplot(key_artist, aes(x=key_name, fill=artist_name, y = n, 
                                text = paste("Number of Songs: ", n, "<br>", "Percent Songs in Key: ", percent, "%")))+
                                      geom_bar(position="fill", width=0.5, stat = "identity")+
                                      scale_fill_manual(values=c(green, yellow))+
                                      labs(x="Key", y="Percent of Songs")+
                                      guides(fill=guide_legend(title="Artist"))+
                                      theme_minimal()+
                                      ggtitle("Musical Key Percentage by Artistt")

ggplotly(viz2, tooltip=c("text"))


#without position="fill"
viz3 <- ggplot(key_artist, aes(x=key_name, fill=artist_name, y = n, 
                               text = paste("Number of Songs: ", n, "<br>", "Percent Songs in Key: ", percent, "%")))+
  geom_bar(width=0.5, stat = "identity")+
  scale_fill_manual(values=c(green, yellow))+
  labs(x="Key", y="Percent of Songs")+
  guides(fill=guide_legend(title="Artist"))+
  theme_minimal()+
  ggtitle("Musical Key Percentage by Artistt")

ggplotly(viz3, tooltip=c("text"))

##Danceability Analysis

#Spotify's danceability index is based on "tempo, rhythm stability, beat strength, and overall regularity". 
#To see how the both artists compared in danceability, I decided to make a density plot, which shows distribution of data.
#I used ggplot2 to make a geom_density of the danceability data for the four playlists. 
#I changed the alpha to 0.7 so all four density plots would be visible.

viz4 <- ggplot(JB_SM, aes(x=danceability, fill=artist_name,
                            text = paste(artist_name)))+
                            geom_density(alpha=0.7, color=NA)+
                            scale_fill_manual(values=c(green,blue))+
                            labs(x="Danceability", y="Density") +
                            guides(fill=guide_legend(title="Artist"))+
                            theme_minimal()+
                            ggtitle("Distribution of Danceability Data")

ggplotly(viz4, tooltip=c("text"))

#This graph seems to suggest that the Shawn Mendes has the widest range of danceability compared to Justin Bieber, 
#Justin Bieber made up of songs on the higher end of the danceability spectrum.

##Range between each artist's most and least danceable song

#First, I used group_by, mutate, select, and unique to create a new datatable with just four rows that show:
#the artist name
#the maximum danceability value of songs on that artist
#the minimum danceability value of songs on that artist

most_least_danceability <- JB_SM %>%
  group_by(artist_name)%>%
  mutate(max=max(danceability))%>%
  mutate(min=min(danceability))%>%
  select(artist_name, max, min)%>%
  unique()

head(most_least_danceability)

#I used plotly to make a dumbbell plot showing the range in danceability values for each artist.

viz5 <- plot_ly(most_least_danceability, color = I("gray80"),  
                hoverinfo = 'text') %>%
                add_segments(x = ~max, xend = ~min, y = ~artist_name, yend = ~artist_name, showlegend = FALSE) %>%
                add_markers(x = ~max, y = ~artist_name, name = "Maximum Danceability Value", color = I(pink), text=~paste('Max Danceability: ', max)) %>%
                add_markers(x = ~min, y = ~artist_name, name = "Minimum Danceability Value", color = I(blue), text=~paste('Min Danceability: ', min))%>%
                layout(title = "Artist Danceability Range",
                xaxis = list(title = "Danceability"),
                yaxis= list(title=""))

ggplotly(viz5)

#As shown from the plot, Shawn Mendes'songs has the widest range in danceability compared to Justin Bieber's songs




