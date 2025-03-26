library(tidyverse)
library(stringr)
library(tidytext)
library(stm)
library(quanteda)


files = list.files("path")


#Initialize dataframes

#Gender
men = data.frame(convo=character(), dialogue=character())
women = data.frame(convo=character(), dialogue=character())
mixsex= data.frame(convo=character(), dialogue=character())

#Race
white= data.frame(convo=character(), dialogue=character())
black= data.frame(convo=character(), dialogue=character())
asian= data.frame(convo=character(), dialogue=character())
latino= data.frame(convo=character(), dialogue=character())
mixed= data.frame(convo=character(), dialogue=character())
indigenous= data.frame(convo=character(), dialogue=character())
mixrace = data.frame(convo=character(), dialogue=character())

#Age
young= data.frame(convo=character(), dialogue=character())
middle= data.frame(convo=character(), dialogue=character())
old= data.frame(convo=character(), dialogue=character())
ym= data.frame(convo=character(), dialogue=character())
yo= data.frame(convo=character(), dialogue=character())
mo= data.frame(convo=character(), dialogue=character())

# Make dialogue data frames for each demographic combination

for (file in files){
  transcript = read.csv(paste("path", file, "/transcription/transcript_cliffhanger.csv", sep = ""))
  survey = read.csv(paste("path", file, "/survey.csv", sep = ""))
  convo = survey$convo_id[1]
  dialogue = transcript$utterance %>%
    paste(collapse = " ")
  if(all(!is.na(survey$sex))){
    if(length(unique(survey$sex)) == 2){
      mixsex[nrow(mixsex)+1,] = c(convo, dialogue)
    } else if (survey$sex[1] == "male"){
      men[nrow(men)+1,] = c(convo, dialogue)
    } else {
      women[nrow(women)+1,] = c(convo, dialogue)
    }
  }
  if(all(!is.na(survey$race) & survey$race != "other" & survey$race != "" & survey$race != "prefer_not_to_say")){
    if(length(unique(survey$race)) == 2){
      mixrace[nrow(mixrace)+1,] = c(convo, dialogue)
    } else if (survey$race[1] == "white"){
      white[nrow(white)+1,] = c(convo, dialogue)
    } else if (survey$race[1] == "black_or_african_american"){
      black[nrow(black)+1,] = c(convo, dialogue)
    } else if (survey$race[1] == "asian"){
      asian[nrow(asian)+1,] = c(convo, dialogue)
    } else if (survey$race[1] == "hispanic_or_latino"){
      latino[nrow(latino)+1,] = c(convo, dialogue)
    } else if (survey$race[1] == "mixed"){
      mixed[nrow(mixed)+1,] = c(convo, dialogue)
    } else {
      ''' NOTE: The survey data in this dataset included multiple options for 
      indigenous American people (i.e. Alaskan Native, Pacific Islander, etc.) 
      Because there were not enough data points in the corpus to make coherent 
      topic models for conversations between two indigenous people (0, to be
      exact), I did not split further. If there were enough data points, I would
      split further or consider the limitations of putting all
      indigenous Americans into one category as is done here.'''
      indigenous[nrow(indigenous)+1,] = c(convo, dialogue)
    }
  }
  if(all(!is.na(survey$age))){
    ages = sort(survey$age)
    if(ages[1] < 35){
      if(ages[2] < 35){
        young[nrow(young)+1,] = c(convo, dialogue)
      } else if (ages[2] < 51){
        ym[nrow(ym)+1,] = c(convo, dialogue)
      } else {
        yo[nrow(yo)+1,] = c(convo, dialogue)
      }
    }
    else if (ages[1] < 51){
      if(ages[2] < 51){
        middle[nrow(middle)+1,] = c(convo, dialogue)
      } else {
        mo[nrow(mo)+1,] = c(convo, dialogue)
      } 
    }
    else {
      old[nrow(old)+1,] = c(convo, dialogue)
    }
  }
}

filler_list = c("yeah", "uh", "mhm", "um", "huh", "people", "time", "lot", 
                "stuff", "pretty", "gonna", "mm", "nice", "day", "wow", "cool", 
                "prolific", "guess", "feel", "bad", "couple", "minutes", 
                "basically", "bit", "talking", "love", "10", "hmm", "crazy", 
                "shit", "started", "week", "person", "life", "sort", "weird",
                "guy", "talk", "true","telling","guys","steps","supposed",
                "start","reason","understand","hour","yep","heard", "18", 
                "luckily","hey", "exact", "ideal", "wait", "restart", "hear",
                "heard", "days", "gray", "interact", "task", "scam", "called")

# Making topic models

#Gender

men_sc = corpus(men$dialogue)
men_sc_tokens = men_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
men_sc_tokens_nostop = men_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
men_dfm = dfm(men_sc_tokens_nostop)
men_stm = stm(men_dfm, K = 5, data = docvars(men_sc_tokens))

women_sc = corpus(women$dialogue)
women_sc_tokens = women_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
women_sc_tokens_nostop = women_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
women_dfm = dfm(women_sc_tokens_nostop)
women_stm = stm(women_dfm, K = 5, data = docvars(women_sc_tokens))

mixsex_sc = corpus(mixsex$dialogue)
mixsex_sc_tokens = mixsex_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
mixsex_sc_tokens_nostop = mixsex_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
mixsex_dfm = dfm(mixsex_sc_tokens_nostop)
mixsex_stm = stm(mixsex_dfm, K = 5, data = docvars(mixsex_sc_tokens))

#Race

white_sc = corpus(white$dialogue)
white_sc_tokens = white_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
white_sc_tokens_nostop = white_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
white_dfm = dfm(white_sc_tokens_nostop)
white_stm = stm(white_dfm, K = 5, data = docvars(white_sc_tokens))

black_sc = corpus(black$dialogue)
black_sc_tokens = black_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
black_sc_tokens_nostop = black_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
black_dfm = dfm(black_sc_tokens_nostop)
black_stm = stm(black_dfm, K = 5, data = docvars(black_sc_tokens))

latino_sc = corpus(latino$dialogue)
latino_sc_tokens = latino_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
latino_sc_tokens_nostop = latino_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
latino_dfm = dfm(latino_sc_tokens_nostop)
latino_stm = stm(latino_dfm, K = 5, data = docvars(latino_sc_tokens))

asian_sc = corpus(asian$dialogue)
asian_sc_tokens = asian_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
asian_sc_tokens_nostop = asian_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
asian_dfm = dfm(asian_sc_tokens_nostop)
asian_stm = stm(asian_dfm, K = 5, data = docvars(asian_sc_tokens))

mixrace_sc = corpus(mixrace$dialogue)
mixrace_sc_tokens = mixrace_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
mixrace_sc_tokens_nostop = mixrace_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
mixrace_dfm = dfm(mixrace_sc_tokens_nostop)
mixrace_stm = stm(mixrace_dfm, K = 5, data = docvars(mixrace_sc_tokens))

#Age

young_sc = corpus(young$dialogue)
young_sc_tokens = young_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
young_sc_tokens_nostop = young_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
young_dfm = dfm(young_sc_tokens_nostop)
young_stm = stm(young_dfm, K = 5, data = docvars(young_sc_tokens))

middle_sc = corpus(middle$dialogue)
middle_sc_tokens = middle_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
middle_sc_tokens_nostop = middle_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
middle_dfm = dfm(middle_sc_tokens_nostop)
middle_stm = stm(middle_dfm, K = 5, data = docvars(middle_sc_tokens))

old_sc = corpus(old$dialogue)
old_sc_tokens = old_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
old_sc_tokens_nostop = old_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
old_dfm = dfm(old_sc_tokens_nostop)
old_stm = stm(old_dfm, K = 5, data = docvars(old_sc_tokens))

'''NOTE: The abbreviations below correspond to the following age combinations
ym: one young person and one middle-aged person
yo: one young person and one old person
mo: one middle-aged person and one old person''' 

ym_sc = corpus(ym$dialogue)
ym_sc_tokens = ym_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
ym_sc_tokens_nostop = ym_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
ym_dfm = dfm(ym_sc_tokens_nostop)
ym_stm = stm(ym_dfm, K = 5, data = docvars(ym_sc_tokens))

yo_sc = corpus(yo$dialogue)
yo_sc_tokens = yo_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
yo_sc_tokens_nostop = yo_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
yo_dfm = dfm(yo_sc_tokens_nostop)
yo_stm = stm(yo_dfm, K = 5, data = docvars(yo_sc_tokens))

mo_sc = corpus(mo$dialogue)
mo_sc_tokens = mo_sc %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE)%>%
  tokens_tolower()
mo_sc_tokens_nostop = mo_sc_tokens%>%
  tokens_select(pattern = stop_words$word,selection = "remove")%>%
  tokens_select(pattern = filler_list, selection = "remove")
mo_dfm = dfm(mo_sc_tokens_nostop)
mo_stm = stm(mo_dfm, K = 5, data = docvars(mo_sc_tokens))

# Topic model visualizations

#Gender

plot(men_stm, n = 5, labeltype = "frex",text.cex = 0.7, main = "")
plot(women_stm, n = 5, labeltype = "frex",text.cex = 0.7, main = "")
plot(mixsex_stm,n = 5, labeltype="frex", text.cex = 0.8, main = "")

#Race

plot(white_stm,n = 5, labeltype= "frex", text.cex = 0.7, main = "")
plot(black_stm,n = 5, labeltype = "frex",text.cex = 0.8, main = "")
plot(latino_stm,n = 5, labeltype = "frex",text.cex = 0.75, main = "")
plot(asian_stm,n = 5, labeltype = "frex",text.cex = 0.8, main = "")
plot(mixrace_stm,n = 5, labeltype = "frex",text.cex = 0.75, main = "")

#Age

plot(young_stm,n = 5, labeltype = "frex",text.cex = 0.7, main = "")
plot(middle_stm,n = 5, labeltype = "frex",text.cex = 0.75, main = "")
plot(old_stm,n = 5, labeltype = "frex",text.cex = 0.7)
plot(ym_stm,n = 5, labeltype = "frex",text.cex = 0.75, main = "")
plot(yo_stm,n = 5, labeltype = "frex",text.cex = 0.7, main = "")
plot(mo_stm,n = 5, labeltype = "frex",text.cex = 0.8, main = "")