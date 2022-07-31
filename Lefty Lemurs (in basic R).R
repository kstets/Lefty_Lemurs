Loading library

```{r}
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
```

#Here's what I did to clean the data

#Importing dataset
L_PreClean <- read.csv("~/Documents/GitHub/Lefty_Lemurs/LeftyLemurs_Original.csv")

summary(L_PreClean)

#rename long column names to simpler ones
L_PreClean <- rename(L_PreClean, Troop = Troop.............A..B..C..D...., Time = Time..H.M.S., Note = Focal.sample.note......................................................................................................................................................please.include.as.much.detail.as.possible...If.a.note.is.about.a.grooming.bout..for.example..include.stop.time.in.the.same.cell.)


# remove commas in the category column
L_PreClean$Category <- str_replace(L_PreClean$Category, ",", "")

# Why on earth did it make it into a value instead of a new data frame??? I guess it doesn't matter because it still works...
# Sometimes I wrote behaviors in all caps, and sometimes I didn't. I'm just going to put all of them in all caps because it's easier.

L_PreClean$Category = toupper(L_PreClean$Category)

#I have some data where I didn't actually record the limb used. Why did I do that? I'm going to delete the rows where I forgot to list the limb.

#Only keep rows in categories that don't contain RH, LH, RF, and/or LF
L_HFdata <-
  L_PreClean %>%
  filter(str_detect(Category, "RH|RF|LH|LF"))

# Squeaky clean

# Now I want to create a separate data frame that only contains behaviors from my ethogram:


# do not run!! <- note from future me
# lefty_justEthogram <-
# L %>%  select(Category) %>%
# filter(str_detect(Category, "BP|QP|L|AG|SG|F|E|CL|HC|R|G|"))


# Why didn't it work

# This did not work: Would it be possible to split the "Category" column into 2 columns? 1 that says "Task" and another that says "Limb"?

# "Limb" would have LH/RH/LF/RH and "Task" would have the other stuff

# I think if I duplicate the column and rename it and then eliminate the limbs from "Task" and keep only the limbs in "Limbs" it might work.

# Then I can rename the tasks to "Eating/Grooming/Locomotion/etc.

# Duplicating "Category" and calling the new one "Limb"


# do not run: L_HFdata$Limb <- L_PreClean$Category
# do not run: L_PreClean2 <- L_PreClean %>% relocate (Limb, .before = Note)


# *This did not work*

# Replacing limb with nothing

# do not run: lefty_split <- L_PreClean2$Category <- str_replace(L_PreClean$Limb, "BP|QP|L|AG|SG|F|E|CL|HC|R|G|", "")

# do not run: lefty_fullsplit <- L_PreClean2$Limb <- str_replace(L_PreClean$Category, "LF|LH|RF|RH|", "")

# Categories kept limbs because they include an R (like rest) or L (like locomotion) I think limb section stayed the same? I think it kept all the columns that contained the limbs, which was all of them ofc. I might be running the wrong code.

# Okay I'm going to use find and replace in Excel to fix it because R doesn't know how to do anything apparently and I literally want to cry rn

# Wait wtf why does it say 30,000+ rows columns

## okay I've spent 4 hours trying to get it to work and tbh this is a waste of time so I'm just going to export the behavior data as a CSV and finish cleaning it in Google Sheets

# Writing as CSV bc my attempts at making limb column completely failed after working on it for 2 days

write_csv(L_PreClean,"Lefty_Lemurs.csv")


# **Here's what I did in Google Sheets with the CSV file** (I really wanted to do it in R but I could not figure it out and wasted hours and hours)

- # Deleted rows that didn't contain behaviors from my ethogram
- # Copied "Category" into 2 columns
- # In old Category column, deleted limb data
- #  In new "Limb" data, kept only limb data (not behavior)
- # Deleted rows that had question marks in data
- # Deleted data where category was "Other"
- #  Deleted data that had rows with both hands
- #  Merged "F" and "E" into one category "E"
- #  Looked through notes to change category when it was just "G". Most got changed to L, some got changed to R, a few got deleted
- #  Added new column for H/F to say whether limb was hand or foot
- #  Added new column for side to say whether limb was left or right

# New categories - E (eating/foraging) - R (resting) (lemur is still) - L (locomotion) (active motion like leaping, climbing, or jumping) - CL (cross legs) - HC (hand clasp) - BP (bipedal walking) - QP (quadrupedal walking) - LA (land) - SG (self grooming) - AG (allo-grooming)

# ***Upload clean data*** (it was a nightmare of cleaning this data in R and Excel and took many terrible hours, of which I do not wish to relive)

L <- read.csv("~/Documents/GitHub/Lefty_Lemurs/LeftyLemurs_Clean.csv")

# **Analysis**

#Hex codes for colors (to use in graphs)
# Red: #C84E00 Orange: #E89923 Yellow: #FFD960 Light Green: #A1B70D Turquoise: #339898 Teal: #1D6363 Medium Blue: #005587 Light Blue: #0577B1 Purple: #993399


head(L)

# Let's look at how many total instances of each limb use there was

table(L$Limb)

# Omg 🙊

# Left Hand: 225 Right Hand: 318 Left Foot: 32 Right Foot: 47

# It looks like lemurs used their right hands a lot more than they used their left hands...

ggplot (L, aes(x= reorder (Limb, Limb, table ))) +
  geom_bar(color = "black", fill = "#012169") +
  theme_classic() +
  xlab("Limb") +
  ylab("Total Times Using Each Limb") +
  ggtitle("Total Limb Use Across Lemurs") +
  coord_flip()

# Yep, there definitely appears to be a hand/foot preference

ggplot (L, aes(x= reorder (Limb, Limb, table ))) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Limb") +
  ylab("Total Times Using Each Limb") +
  ggtitle("Total Limb Use Across Lemur Species") +
  coord_flip() +
  facet_wrap (~ Species, scales = "free")

ggplot (L, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Limb Use Between Lemur Species") +
  facet_wrap (~ Species, scales = "free")

ggplot (L, aes(x= reorder (Side, Side, table ))) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right") +
  ylab("Total Times Using Each Limb") +
  ggtitle("Limb Use Between Lemur Species") +
  coord_flip() +
  facet_wrap (~ Species, scales = "free")

# Same thing but on the same scale (I got more data from sifakas as u can see)
ggplot (L, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Limb Use Between Lemur Species") +
  facet_wrap (~ Species,)


# 👀 Sifakas have higher left hand use than the other 2 species. Sifakas overall seem more ambidextrous. However, this could be due to them foraging more (maybe all lemurs forage more with their left hands or something idk). Therefore I should separate it out by eating behaviors.

# What about by sex

ggplot (L, aes(x = reorder (Limb, Limb, table ))) +
  geom_bar(color = "black", fill = "#1D6363") +
  theme_classic() +
  xlab("Limb") +
  ylab("Total Times Using Each Limb") +
  ggtitle("Total Limb Use Between Sexes: Lemurs Are Not Lefties") +
  coord_flip() +
  facet_wrap (~ Sex, scales = "free")

# This is the same thing but just divided by left and right side (left hand and left foot, etc. are in the same category)

ggplot (L, aes(x= reorder (Side, Side, table ))) +
  geom_bar(color = "black", fill = "#1D6363") +
  theme_classic() +
  xlab("Left or Right") +
  ylab("Total Times Using Each Limb") +
  ggtitle("Total Limb Use Between Sexes: Lemurs Are Not Lefties") +
  coord_flip() +
  facet_wrap (~ Sex, scales = "free")

# Definitely more right hand than left HAND use for both males and females. Not sure about foot use, but I didn't get much foot use data to begin with. Is there a way to exclude foot use?

# Males might me a little less lateralized than females? Left hand use is also (slightly) higher in males like I thought it would be!

ggplot (L, aes(x= reorder (Category, Category, table ))) +
  geom_bar(color = "black", fill = "#0577B1") +
  theme_classic() +
  xlab("Type of Behavior") +
  ylab("Total Times Using Each Limb") +
  ggtitle("Behavior Across Sexes") +
  coord_flip() +
  facet_wrap (~ Sex,scales = "free")

# Everyone eats a lot

# Some calculations

summary(L)

library(psych)

describeBy(L[c(4,1:12)], group = "Category")


# Making a DF with just hand grasp data

# take out feet use data

l_hands <-
  L %>%
  select(H.F) %>%
  filter(str_detect(H.F, "H"))

# add the other stuff back
l_hands <- inner_join(l_hands, L, by = "H.F")

l_hands <- unique(l_hands)

# actually I also don't want to look at all the categories bc some were added just for fun and are probably not important

l_handstuff <-
  l_hands %>%
  select(Category) %>%
  filter(str_detect(Category, "E|L|R|SG"))


# WHY WON'T IT TAKE OUT LA

# add the other stuff back why did it even go away

l_handstuff <- inner_join(l_handstuff, l_hands, by = "Category")

# I can remove the H.F row because all of it is hand 

l_handstuff <- l_handstuff %>% select(-H.F)

# removing Limb row bc side says the same thing

l_handstuff <- l_handstuff %>% select(-Limb)

# removing mystery duplicates

l_handstuff <- unique(l_handstuff)


l_noLA <- 
  l_handstuff %>% 
  filter(Category == "E"  |  Category == "L" |  Category == "R" |  Category == "SG" | Category != "LA" )

#wow I forgot you can do that

#Compare hand use by species
ggplot (l_noLA, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use Between Lemur Species") +
  facet_wrap (~ Species, scales = "free")

ggplot (l_noLA, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("Type of Behavior") +
  ggtitle("Hand Use Between Behaviors") +
  facet_wrap (~ Category, scales = "free")

# So it looks like lemurs mostly use their right hand for everything except for grooming! Interesting

# Now I'm going to filter out the grooming data so it's all grasping

l_grasps <-
  l_noLA %>% 
  filter(Category == "E"  |  Category == "L" |  Category == "R")

# Compare grasping hand by species


ggplot (l_grasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Grasping Between Lemur Species") +
  facet_wrap (~ Species, scales = "free")

# What about by sex?

ggplot (l_grasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Grasping Between Sexes") +
  facet_wrap (~ Sex, scales = "free")

# It doesn't look like there's really a difference

# What about just food grasps?
 
l_foodGrasps <-
  l_grasps %>% 
  filter(Category == "E")

# Compare grasping hand by species


ggplot (l_foodGrasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Food Grasping Between Lemur Species") +
  facet_wrap (~ Species, scales = "free")


# It really looks like sifakas are more ambidextrous when it comes to food grasping!
  
# What about by individual?
  
ggplot (l_foodGrasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Food Grasping Between Individuals") +
  facet_wrap (~ Focal, scales = "free")


# It kind of looks like Sophia and Thrax are lefties!!! And Licinius is close... The other lemurs look like righties!
  
# Look at them all on the same scale

ggplot (l_foodGrasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Food Grasping Between Individuals") +
  facet_wrap (~ Focal)

# Okay there isn't much data for Sophia so it could just be "noise" for her... But Thrax has a lot of data

# But is all of this actually statistically significant
# Don't know

ggplot (l_noLA, aes(x= Category)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Activity") +
  ylab("Occurrences") +
  ggtitle("Hand Use for Activities by Species") +
  facet_wrap (~ Species)

# This just does occurrences. How can I get the Y-axis to be % of L compared to R?
  
#  Idk

ggplot (l_foodGrasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Activity") +
  ylab("Occurrences") +
  ggtitle("Hand Used for Food Grasping Between Species") +
  facet_wrap (~ Species)

ggplot (l_foodGrasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Grasping Between Individuals") +
  facet_wrap (~ Focal, scales = "free" )

ggplot (l_foodGrasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Side") +
  ylab("Occurrences") +
  ggtitle("Hand Used for Food Grasping Between Species") +
  facet_wrap (~ Species)

ggplot (L, aes(x= Focal)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Focal") +
  ylab("Occurrences") +
  ggtitle("# of Times Using Each Limb") +
  facet_wrap (~ Side)

ggplot (l_foodGrasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Side") +
  ylab("Occurrences") +
  ggtitle("Hand Used for Food Grasping Between Sex") +
  facet_wrap (~ Sex, scales = "free")

# Trying some statistical tests


L %>% 
  group_by(Side) %>% 
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

l_handstuff %>% 
  group_by(Side) %>% 
  summarise (n = n()) %>%
  mutate(proportion = n / sum(n))

binom.test(218, 305, p = .75, alternative = "two.sided")


ggplot (l_grasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Grasping Between Sexes") +
  facet_wrap (~ Sex, scales = "free")

ggplot (L, aes(x= Limb)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Limb") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Limb Use Between Individuals") +
  facet_wrap (~ Focal)

# Not a lot of foot use. It might not be very useful to include these data

# Just look at bipedal locomotion


l_feet <- 
L %>% 
  filter(Category == "BP")

ggplot (l_feet, aes(x= Limb)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Limb") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Foot Use Between Individuals") +
  facet_wrap (~ Focal)

# I don't really see any overall patterns here. It looks like Furia leads with her right foor more than left and Thrax leads a little more with his left foot than right.

# Does age impact hand use?
  

ggplot(L, aes(x = Category, y = Age, color = Side)) + 
  geom_jitter()  +
  theme_classic() +
  xlab("Type of Behavior") +
  ylab("Age of Lemur") +
  ggtitle("Does Age Affect Limb Use?") + 
  scale_color_manual(values = c("#1D6363", "#E89923"))

# Yeah I don't think it does lol

# Trying to make the graph I yearn for

ggplot (l_noLA, aes(x= Category, fill = Side)) +
  geom_bar(color = "black") +
  theme_classic() +
  xlab("Type of Behavior") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Limb Use Between Lemur Species") +
  facet_wrap (~ Species, scales = "free") + 
  scale_fill_manual(values = c("#1D6363", "#E89923"))

# It's still not exactly what I want. I want the Y-axis to be % of grasps being with left hand, not total times used. Splitting by color makes it really hard to read, but Idk how else to do it :P

ggplot (l_noLA, aes(x= Side, fill = interaction(Category, Side))) +
  geom_bar(color = "black") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Limb Use Between Lemur Species") +
  facet_wrap (~ Species, scales = "free")

# Wow that it was too complicated. Why did I dare make this monstrosity?
  
  # Okay, the landing and foot use data is fun and all, but the hand grasps it what I actually need to look at. I'm going to make that into a .CSV and analyze the data in JMP

write_csv (l_grasps, "HandGrasps.csv")

## Testing significance: <- future me says this was not the right way to do this analysis

table(l_grasps$Side, l_grasps$Species)
chisq.test(table(l_grasps$Side, l_grasps$Species))

table(l_grasps$Side, l_grasps$Sex)
chisq.test(table(l_grasps$Side, l_grasps$Sex))

table(l_grasps$Side, l_grasps$Focal)
chisq.test(table(l_grasps$Side, l_grasps$Focal))

table(l_grasps$Side, l_grasps$Category)
chisq.test(table(l_grasps$Side, l_grasps$Category))

table(l_grasps$Side, l_grasps$Age)
chisq.test(table(l_grasps$Side, l_grasps$Age))

ggplot (l_grasps, aes(x= Side)) +
  geom_bar(color = "black", fill = "#C84E00") +
  theme_classic() +
  xlab("Left or Right Side") +
  ylab("# of Times Using Each Limb") +
  ggtitle("Hand Use for Grasping Between Individuals") +
  facet_wrap (~ Focal, scales = "free")


# ***All of my analyses were done wrong!!***

# I literally wish I were dead rn


##Is hand preference independent of individual?##
# (need to do 9 tests for 3 species x 3 grasps)

# Make DF separated by *type of grasp*

l_foodGrasps <-
l_grasps %>% 
  filter(Category == "E")

l_locGrasps <-
l_grasps %>% 
  filter(Category == "L")

l_RestGrasps <-
l_grasps %>% 
  filter(Category == "R")

# Finding left food grasps by individual

left_fg <-
  l_foodGrasps %>%
  filter(Side == "L")
 
table(left_fg$Focal)

# Right food grasps by individual

right_fg <-
  l_foodGrasps %>%
  filter(Side == "R")
 
table(right_fg$Focal)

# Find type of grasp by species first

left_fg <-
  l_foodGrasps %>%
  filter(Side == "L")
 
table(left_fg$Species)

right_fg <-
  l_foodGrasps %>%
  filter(Side == "R")
 
table(right_fg$Species)

# Also getting count by sex

table(left_fg$Sex)

table(right_fg$Sex)

# Loading stuff for G-Test

library(RVAideMemoire)

# Food grasps for sifakas

FG_Sifaka <- matrix(c(3, 6, 2, 7, 32, 21, 16, 16), nrow = 4, ncol = 2, byrow = T)
rownames(FG_Sifaka) <- c('Furia', 'Gisela', 'Thrax', 'Rupert')
colnames(FG_Sifaka) <- c('Freq_L', 'Freq_R')

G.test(FG_Sifaka)

# Food grasps for mongoose lemurs

FG_Mongoz <- matrix(c(28, 6, 9, 19, 4, 14, 1, 9), nrow = 4, ncol = 2, byrow = T)
rownames(FG_Mongoz) <- c('Rico', 'Carolina', 'Duggan', 'Maddie')
colnames(FG_Mongoz) <- c('Freq_L', 'Freq_R')

G.test(FG_Mongoz)

FG_lcat <- matrix(c(9, 1, 10, 23, 0, 47, 16, 19 ), nrow = 4, ncol = 2, byrow = T)
rownames(FG_lcat) <- c('Sophia', 'Sierra Mist', 'Randy', 'Licinius')
colnames(FG_lcat) <- c('Freq_L', 'Freq_M')

G.test(FG_lcat)

# Finding left and right resting grasps by individual

left_rg <-
  l_RestGrasps %>%
  filter(Side == "L")
 
table(left_rg$Focal)

right_rg <-
  l_RestGrasps %>%
  filter(Side == "R")
 
table(right_rg$Focal)

# Resting grasps for sifakas

RG_Sifaka <- matrix(c(7, 4, 16, 16, 8, 11, 16, 22), nrow = 4, ncol = 2, byrow = T)
rownames(RG_Sifaka) <- c('Furia', 'Gisela', 'Thrax', 'Rupert')
colnames(RG_Sifaka) <- c('Freq_L', 'Freq_M')

G.test(RG_Sifaka)

#Resting grasps for mongoose lemurs
RG_Mongoz <- matrix(c(2, 4, 1, 3, 0, 0, 1, 5), nrow = 4, ncol = 2, byrow = T)
rownames(RG_Mongoz) <- c('Rico', 'Carolina', 'Duggan', 'Maddie')
colnames(RG_Mongoz) <- c('Freq_L', 'Freq_M')

G.test(RG_Mongoz)

# Resting data for catta

FG_lcat <- matrix(c(0, 0, 0, 2, 0, 0, 1, 7), nrow = 4, ncol = 2, byrow = T)
rownames(FG_lcat) <- c('Sophia', 'Sierra Mist', 'Randy', 'Licinius')
colnames(FG_lcat) <- c('Freq_L', 'Freq_M')

G.test(FG_lcat)

# Finding left and right locomotion grasps by individual
l_locGrasps <-
l_grasps %>% 
  filter(Category == "L")
  
left_lg <-
  l_locGrasps %>%
  filter(Side == "L")
 
table(left_lg$Focal)

right_lg <-
  l_locGrasps %>%
  filter(Side == "R")
 
table(right_lg$Focal)

# Loc grasps for sifakas
LG_Sifaka <- matrix(c(3, 2, 4, 2, 0, 1, 3, 0), nrow = 4, ncol = 2, byrow = T)
rownames(LG_Sifaka) <- c('Furia', 'Gisela', 'Thrax', 'Rupert')
colnames(LG_Sifaka) <- c('Freq_L', 'Freq_M')

G.test(LG_Sifaka)


# Loc grasps for mongoose lemurs
LG_Mongoz <- matrix(c(7, 10, 8, 11, 1, 4, 4, 2), nrow = 4, ncol = 2, byrow = T)
rownames(LG_Mongoz) <- c('Rico', 'Carolina', 'Duggan', 'Maddie')
colnames(LG_Mongoz) <- c('Freq_L', 'Freq_M')

G.test(LG_Mongoz)

# Loc data for catta
LG_lcat <- matrix(c(1, 3, 0, 2, 2, 0, 2, 2), nrow = 4, ncol = 2, byrow = T)
rownames(LG_lcat) <- c('Sophia', 'Sierra Mist', 'Randy', 'Licinius')
colnames(LG_lcat) <- c('Freq_L', 'Freq_M')

G.test(LG_lcat)

##Is hand preference independent of sex?##
left_grasps <-
l_grasps %>%
  filter(Side == "L")

table(left_grasps$Focal)

right_grasps <-
l_grasps %>%
  filter(Side == "R")

table(right_grasps$Focal)

#Sifaka
g_sifaka <- matrix(c(67, 71, 35, 37), nrow = 2, ncol = 2, byrow = T)
rownames(g_sifaka) <- c('Males', 'Females')
colnames(g_sifaka) <- c('Freq_L', 'Freq_R')

fisher.test(g_sifaka)

#Mongoz
g_mongoz <- matrix(c(21, 35, 10, 31), nrow = 2, ncol = 2, byrow = T)
rownames(g_mongoz) <- c('Males', 'Females')
colnames(g_mongoz) <- c('Freq_L', 'Freq_R')

fisher.test(g_mongoz)

#Lcat
g_lcat <- matrix(c(42, 79, 24, 49), nrow = 2, ncol = 2, byrow = T)
rownames(g_lcat) <- c('Males', 'Females')
colnames(g_lcat) <- c('Freq_L', 'Freq_R')

fisher.test(g_lcat)

table(left_fg$Focal)
table(right_fg$Focal)

fg_all <- matrix(c(16, 25, 19, 24, 32, 61, 10, 28, 40, 37, 5, 23), nrow = 6, ncol = 2, byrow = T)
rownames(fg_all) <- c('LcatM', 'LcatF', 'EmonM', 'EmonF', 'PcoqM', 'PcoqF')
colnames(fg_all) <- c('FreqL', 'FreqR')

G.test(fg_all)


l_locGrasps <-
l_grasps %>% 
  filter(Category == "L")

left_lg <-
  l_locGrasps %>%
  filter(Side == "L")

table(left_lg$Focal)


right_lg <-
  l_locGrasps %>%
  filter(Side == "R")

table(right_lg$Focal)

lg_all <- matrix(c(4, 3, 1, 5, 8, 14, 12, 13, 3, 1, 7, 4), nrow = 6, ncol = 2, byrow = T)
rownames(lg_all) <- c('LcatM', 'LcatF', 'EmonM', 'EmonF', 'PcoqM', 'PcoqF')
colnames(lg_all) <- c('FreqL', 'FreqR')

G.test(lg_all)


l_FoodGrasps <-
l_grasps %>% 
  filter(Category == "R")

left_fg <-
  l_FoodGrasps %>%
  filter(Side == "L")

table(left_fg$Focal)


right_fg <-
  l_FoodGrasps %>%
  filter(Side == "R")

table(right_fg$Focal)

fg_all <- matrix(c(1, 7, 0, 2, 2, 4, 2, 8, 24, 33, 23, 20), nrow = 6, ncol = 2, byrow = T)
rownames(fg_all) <- c('LcatM', 'LcatF', 'EmonM', 'EmonF', 'PcoqM', 'PcoqF')
colnames(fg_all) <- c('FreqL', 'FreqR')

G.test(fg_all)

# Wow I have no idea if I did any of those right


# Well anyway let's make a graph

by_focal <- read.csv("~/Documents/GitHub/Lefty_Lemurs/ByFocal.csv")

mean(by_focal$P.Value)

ggplot(by_focal, aes(x= Species, y= P.Value)) +
  geom_bar(stat= 'identity', color = "black", fill = "#C84E00") +
  theme_classic()+
  xlab("Species of Lemur") +
  ylab("P-Value (less than 0.05 is significant)") +
  ggtitle("Is Hand Preference Independent of Individual?") +
  facet_wrap (~ Behavior) +
  geom_text(aes(label= P.Value, x= Species, y= P.Value), vjust= -0.5) +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 10))

# Graph about biological sex

by_sex <- read.csv("~/Documents/GitHub/Lefty_Lemurs/BySex.csv")

mean(by_sex$P.Value)

ggplot(by_sex, aes(x= Species, y= P.Value)) +
  geom_bar(stat= 'identity', color = "black", fill = "#1D6363") +
  theme_classic()+
  xlab("Species of Lemur") +
  ylab("P-Value (less than 0.05 is significant)") +
  ggtitle("Is Hand Preference Independent of Sex?") +
  geom_text(aes(label= P.Value, x= Species, y= P.Value), vjust= -0.5) +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 10))

by_species <- read.csv("~/Documents/GitHub/Lefty_Lemurs/BySpecies.csv")

mean(by_species$P.Value)

ggplot(by_species, aes(x= Behavior, y= P.Value)) +
  geom_bar(stat= 'identity', color = "black", fill = "#993399") +
  theme_classic()+
  xlab("Behavior") +
  ylab("P-Value (less than 0.05 is significant)") +
  ggtitle("Is Hand Preference Independent of Species?") +
  geom_text(aes(label= P.Value, x= Behavior, y= P.Value), vjust= -0.5) +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(size = 10))