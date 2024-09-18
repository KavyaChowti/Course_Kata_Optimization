page_views = read.csv(file.choose(), header = T)
attach(page_views)

page_views = page_views %>%
  drop_na()

library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)


# Extract hour of the day
page_views$hour <- as.integer(substring(page_views$dt_accessed, 12, 13))

# Convert milliseconds to minutes
page_views$engaged_minutes <- page_views$engaged / 60000

# Group data by hour and chapter, then summarize to get one point per hour per chapter
grouped_data <- page_views %>%
  group_by(hour, chapter_number) %>%
  summarize(mean_engaged_minutes = mean(engaged_minutes, na.rm = TRUE))

# Define the number of colors 
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

# Create the line plot with custom color palette
ggplot(grouped_data, aes(x = hour, y = mean_engaged_minutes, color = as.factor(chapter_number))) +
  geom_line() +
  geom_point() +
  labs(title = "Student Engagement with Pages by Hour of the Day",
       x = "Hour of the Day",
       y = "Engagement (minutes)",
       color = "Chapter Number") +
  scale_x_continuous(breaks = seq(0, 23, by = 1), limits = c(0, 23)) +
  scale_color_manual(values = mycolors) +  # Use custom color palette here
  theme_classic()







# STACKED BAR GRAPH CODE

combined = read.csv(file.choose(), header = T)
attach(combined)


library(ggplot2)
library(tidyr)


# Define the order of the chapters if needed
combined$chapter_number <- factor(combined$chapter_number, levels = unique(combined$chapter_number))

# Create the stacked bar plot
ggplot(combined, aes(x = chapter_number)) +
  geom_bar(aes(y = mcq_questions, fill = "MCQ"), stat = "identity", position = "stack") +
  geom_bar(aes(y = shorttext_questions, fill = "Short Text"), stat = "identity", position = "stack") +
  geom_bar(aes(y = code_questions, fill = "Code"), stat = "identity", position = "stack") +
  geom_bar(aes(y = association_questions, fill = "Association"), stat = "identity", position = "stack") +
  geom_bar(aes(y = choice_questions, fill = "Choice"), stat = "identity", position = "stack") +
  geom_bar(aes(y = formula_v2_questions, fill = "Formula V2"), stat = "identity", position = "stack") +
  geom_bar(aes(y = imageclozequestions, fill = "Image Cloze"), stat = "identity", position = "stack") +
  geom_bar(aes(y = plain_text_question, fill = "Plain Text"), stat = "identity", position = "stack") +
  geom_bar(aes(y = sortlist_questions, fill = "Sort List"), stat = "identity", position = "stack") +
  labs(title = "Number of Questions by Chapter",
       x = "Chapter Number",
       y = "Number of Questions",
       fill = "Question Type") +
  scale_fill_manual(values = mycolors) +
  theme_classic() + theme(text = element_text(size = 17))




