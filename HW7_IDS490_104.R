# HW7_490IDS_104.R

#1.
#1 a)
pattern1.1 = "[[:alpha:]][[:punct:]][[:alnum:]]+"
grep(pattern1.1, c("Toady", "is h@ate", "vp|", "v|c0din"))
#1 b)
pattern1.2 = "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
grep(pattern1.2,"100.1.12.100")
#1 c)
pattern1.3 = "[[:alnum:]_]+\\@[[:alnum:]]+\\.(com|gov|edu|net)$"
grep(pattern1.3, "hrwang@3uiuc.gov")

#2 a)
filename = "/Users/wanghaoran/Downloads/stateoftheunion1790-2012.txt"
dataset = readLines(filename)

#2 b)
star = grep("\\*{3}",dataset) 
# find all indices matched with "***"

#2 c)

findDate = function(dataset, star){
  dates = c()
  for(i in star){
    dates = c(dates, dataset[i+4])
  }
  dates = dates[!is.na(dates)]
  return (dates)
}
dates = findDate(dataset, star)

#2 d)

findYear = function(dates){
  years = c()
  pattern = ".*,[[:blank:]]"
  for (i in dates){
    years = c(years,gsub(pattern, "", i))
  }
  return (years)
}

years = findYear(dates)

#2 e) get all the months
findMonth = function(dates){
  mon = c()
  pattern = "[^[:alpha:]]"
  for (i in dates){
    mon = c(mon, gsub(pattern, "", i))
  }
  return (mon)
}

mons = findMonth(dates)

#2 f)
findPre = function(dataset, star){
  names = c()
  for(i in star){
    names = c(names, dataset[i+3])
  }
  names = names[!is.na(names)]
  return (names)
}
names = findPre(dataset, star)

#2 g)
amount.speech = length(names)
amount.pre = length(unique(names))

#2 h)
# pass the star list to get the intervals of one speech
chop = function(s){
  result = list()
  
  for(i in 1:length(s)){
    interval= c()
    if (i==length(s)){
      break
    }
    start = s[i]
    end = s[i+1]
    interval = c(start,end)
    
    result = c(result, list(interval))
    
  }
    
  return (result)
  }

cut_dataset = function(dataset,invl){
  result = list()
  for(i in invl){
    str = ''
    start = unlist(i)[1]
    end = unlist(i)[2]-1
    for (j in start:end){
      str = paste(str,dataset[j])
    }
    result = c(result, list(str))
  }
  return (result)
}  

intervals = chop(star)
chopped_set = cut_dataset(dataset, intervals)
# check whether the number in the list match with the previous answer
length(chopped_set)

# so the number of list elements could match with the previous answer,
# they are both 222 elements in the list.
   
#2 i)
pattern2i = "[[:punct:]]?(A|a)pplause[[:punct:]]*|[0-9]|[\\.{3}]"
pattern2i2 = "[0-9]"
pattern2i3 = "[[:punct:]]?(A|a)pplause[[:punct:]]*"

new_chopped_set =list()
for(i in chopped_set){
    temp = gsub(pattern2i,"",unlist(i))
    new_chopped_set = c(new_chopped_set,list(temp))
}


#2 j)
# convert letter to lower case
temp_set = list()
for (i in new_chopped_set){
    i = tolower(unlist(i))
    temp_set = c(temp_set,list(i))
}
new_chopped_set = temp_set

#2 k)
# split the sentence into words, return a list of words for different speech
createWords = function(chopped_set){
  after = list()
  pattern = "[[:punct:]]|[[:blank:]]"
  for (i in chopped_set){
    word_vec = unlist(strsplit(unlist(i), pattern))
    after = c(after, list(word_vec))
  }
  
  return (after)
}
new_chopped_set = createWords(new_chopped_set)
 
#2 l)
# use the function to remove empty word
removeEmpty = function(chopped_set){
  temp_list=list()
  for(i in chopped_set){
    un_i = unlist(i)
    un_i = un_i[un_i != ""] # whether the element is "" or not
    temp_list = c(temp_list,list(un_i))
  }
  return(temp_list)
}

new_chopped_set = removeEmpty(new_chopped_set)

#2 m)
createWordVec = function(chopped_set){
  w_set = c()
  for(i in chopped_set){
    word_vec=c()
    unlisted_i = unlist(i)
    for(j in unlisted_i){
      word_vec = c(word_vec, j)
    }
    w_set = c(w_set, list(word_vec))
  }
  return(w_set)
}

#2 n)
# function to normalize the word to get the frequencies
normalize = function(chopped_set){
  after =list()
  for (i in chopped_set){
    temp_table = table(unlist(i)) # get the word counter in a table
    #temp_table = temp_table/sum(temp_table)
    # get the frequency by dividing the total amount of words
    after = c(after, list(temp_table))
  }
  return(after)
}

normalized_list = normalize(new_chopped_set)

#2 o)

#  1)
# function to find the number of sentences in each speech
findSen = function(chopped_set){
  result = list()
  pattern = "[,\\.\\?!;:]"
  for (i in chopped_set){
    unlist_i = unlist(i)
    sent = strsplit(i,pattern)
    result = c(result, length(unlist(sent)))
  }
  return (result)
}
# we could get a list of the number of sentences in each speech
num_sen_list = findSen(chopped_set)
num_sen_list[1]

hist(unlist(num_sen_list),breaks = 20, col = "blue", main = "Histogram of number of sentences", xlab = "number of sentences", xlim = c(0,3200),border="black")

# the graph implies that it is more likely that there are less than 1000 sentences
# in a speech. 

# 2)
# find the long words in the speech by quantile funciton with p = 0.99
findLongWords = function(new_chopped_set){
  result = list()
  for (i in new_chopped_set){
    limit = quantile(sapply(unlist(i),nchar),p = 0.99)
    long_words = unlist(i)[which(sapply(i,nchar) >= limit)]
    result = c(result, list(long_words))
  }
  return (result)
}
# get the long words list for each speech
long_words_list = findLongWords(new_chopped_set)
long_words_list[1]

# plot the histogram of long words of each speech
hist(sapply(sapply(long_words_list,unlist),length),breaks = 20, col = "darkgrey", main = "Histogram of number of long words", xlab = "number of long words",border="black", xlim = c(0,500), ylim = c(0,100))

# from the histogram, we could see that most presidents used less than 200 long words
# in the speech.

# 3) 
# plot the word whose frequency is bigger than 30 in the last speech
test = data.frame(word = names(unlist(normalized_list[222])[unlist(normalized_list[222])>30]),freq = unlist(normalized_list[222])[unlist(normalized_list[222])>30])

library(ggplot2) 
wfp = ggplot(test,aes(word,freq))    
wfp = wfp + geom_bar(stat="identity")   
wfp = wfp + theme(axis.text.x=element_text(angle=45, hjust=1))
# plot the term frequency which has appeared more than 30 times in last speech 
# presented by the President Barack Obama
wfp

# from the plot, we could see the term frequency in the last speech of the
# President Barack Obama. For the most used words shown in the graph, we could
# notice that majority of them are daily used words like 'I', 'You', 'the' and 
# so on. However, we also notice Mr.President mentions 'America', 'Jobs', 'More'
# more times than other general words in the speech. And we could infer that the
# topic or in other words, majority of the content in speech is about the adding
# more jobs in america for americans.

# 4)
# we could try to find the averaged number of words used by each 
# president in one speech
findAvg = function(new_chopped_set, names){
  pre_word_list = data.frame(name = names, number = sapply(new_chopped_set,length))
  average_list = aggregate(list(number = pre_word_list$number), list(name = pre_word_list$name), mean)
  # aggregrate funciton would return a data.frame 
  return (average_list)
  }



avg_list = findAvg(new_chopped_set, names)
avg_list
# plot the average number of words for each president
avgp = ggplot(avg_list,aes(name,number))    
avgp = avgp + geom_bar(stat="identity")   
avgp = avgp + theme(axis.text.x=element_text(angle=45, hjust=1))
avgp

# The plot is arragened by the alphabetic order of the name of presidents. From the
# plot, we could see that Theodore Roosevelt used the most number of words in average
# per speech. And Georoge.W.Washington used the least amount of words in average
# per speech.
