library(tidyverse)
library(stringr)
library(RColorBrewer)
library(gridExtra)
library(readxl)


raw_survey <- read_excel(
  path = file.choose(),
  col_names = FALSE
)


# raw_survey <- read_csv(file = file.choose(),
#                        col_name = FALSE)


survey <- raw_survey %>%
  # Remove unnecessary columns
  select(-c(2:5))

# Create character vector of the question row
headers <- as.character(survey[2, ])

# Convert this vector to byte format
headers <- iconv(headers, "", "ASCII", "byte")

# sub the byte string for curly apostrophe for standard apostrophe
headers <- gsub("<c3><a2><e2><82><ac><e2><84><a2>", "'", headers)

# the vector the column names
colnames(survey) <- headers

# Remove unneccesary rows
survey <- survey[-c(1:3, 5), ]

# Remove columns with names containing practice or investigation
survey <- survey[, !grepl("practice", names(survey), ignore.case = TRUE)]
survey <- survey[, !grepl("Investigation", names(survey), ignore.case = TRUE)]


# Put every question in a character vector
questions <- as.character(colnames(survey)[2:ncol(survey)])

# Put every answer in a character vector
answers <- as.character(survey[1, 2:ncol(survey)])

# Bind into a dataframe
qa <- cbind.data.frame(questions, answers)

# Make a new column of the first 5 words of each question
# qa$questions_short <- word(qa$questions, 1, 5)

# Remove repeated questions and answers
qa <- unique.data.frame(qa) %>%
  # Add column with an identifier
  mutate(QuestionID = paste("Q", row_number(), sep = ""))

# Convert to character vector
qa$answers <- as.character(qa$answers)

# Replace full questions with identifiers
colnames(survey)[2:ncol(survey)] <- qa$QuestionID[match(colnames(survey)[2:ncol(survey)], qa$questions)]

# Rename the first column
colnames(survey)[1] <- "Card"

# Remove row of answers
survey <- survey[-1, ]

# Count the number of unique questions
num_questions <- length(unique(colnames(survey))) - 1

# Vector of all questing identifiers including repeats
identifiers <- colnames(survey)[2:ncol(survey)]

# Find number of groups by taking the total number of questions divided by
# unique questions divided by 2 for pre/post
num_groups <- length(identifiers) / num_questions / 2

# Make a varible for the loop
group_start <- 1

# Loop to add group and pre/post to QuestionID

for (i in seq(0, ((num_groups - 1) * num_questions * 2), num_questions * 2)) {
  identifiers <- replace(
    identifiers,
    (i + 1):(i + num_questions),
    paste(identifiers[(i + 1):(i + num_questions)], ":pre:", "")
  )

  identifiers <- replace(
    identifiers,
    (i + num_questions + 1):(i + num_questions * 2),
    paste(identifiers[(i + num_questions + 1):(i + num_questions * 2)], ":post:", "")
  )

  identifiers <- replace(
    identifiers,
    (i + 1):(i + num_questions * 2),
    paste(identifiers[(i + 1):(i + num_questions * 2)], group_start, "")
  )

  group_start <- group_start + 1
}



colnames(survey)[2:ncol(survey)] <- str_replace_all(identifiers, fixed(" "), "")
survey[survey == "-"] <- NA

survey_gathered <- survey %>%
  gather(key = "Question", value = "Answer", -Card) %>%
  rowwise() %>%
  mutate(Group = unlist(str_split(Question, ":"))[3]) %>%
  mutate(PrePost = unlist(str_split(Question, ":"))[2]) %>%
  mutate(QuestionID = unlist(str_split(Question, ":"))[1])


survey_gathered$Correct_Answer <- qa$answers[match(survey_gathered$QuestionID, qa$QuestionID)]

survey_gathered <- survey_gathered %>%
  rowwise() %>%
  mutate(Correct = Answer == Correct_Answer) %>%
  select(-c(Question, Correct_Answer)) %>%
  mutate(Card = paste(Card, Group, sep = ":")) %>%
  ungroup()



survey_collapsed <- survey_gathered %>%
  group_by(Group, PrePost, QuestionID) %>%
  summarise(
    Number_Correct = length(Correct[which(Correct == TRUE)]),
    Number_Answered = sum(!is.na(Correct))
  ) %>%
  mutate(Percent_Correct = Number_Correct / Number_Answered) %>%
  filter(Number_Answered > 0)

survey_spread <- survey_gathered %>%
  select(-Answer) %>%
  spread(key = PrePost, value = Correct) %>%
  filter(pre != "NA" & post != "NA") %>%
  mutate(Pre = substr(pre, 1, 1)) %>%
  mutate(Post = substr(post, 1, 1))


survey_count <- survey_spread %>%
  group_by(Group, QuestionID, Post, Pre) %>%
  summarise(Count = n()) %>%
  group_by(Group, QuestionID) %>%
  mutate(Percent = signif((Count / sum(Count) * 100), digits = 3))


survey_wide <- survey_collapsed %>%
  select(-c(Number_Answered, Number_Correct)) %>%
  spread(key = PrePost, value = Percent_Correct) %>%
  mutate(Delta = post - pre) %>%
  ungroup() %>%
  mutate(
    Group = factor(Group),
    QuestionID = as.factor(QuestionID)
  )


ggplot(survey_wide) +
  geom_dumbbell(aes(
    x = pre,
    xend = post,
    y = QuestionID
  ),
  colour_x = "#a3c4dc",
  colour_xend = "#0e668b",
  size_x = 1.5,
  size_xend = 3
  ) +

  labs(
    x = "Percent Correct",
    y = "Question ID"
  ) +

  facet_grid(Group ~ ., scales = "free") +
  coord_flip()







ggplot(data = survey_count, aes(
  x = Pre,
  y = Post
)) +

  geom_tile(aes(fill = Percent)) +

  geom_text(aes(label = paste(Percent, "%", sep = ""))) +


  scale_fill_distiller(
    palette = "BuGn",
    direction = "1"
  ) +

  facet_wrap(Group ~ QuestionID, nrow = 2)




wide_data <- survey_collapsed %>%
  select(-c(Number_Answered, Number_Correct)) %>%
  spread(key = PrePost, value = Percent_Correct) %>%
  mutate(Delta = post - pre) %>%
  ungroup() %>%
  mutate(
    Group = factor(Group),
    QuestionID = as.factor(QuestionID)
  )


arrowplot <- ggplot(wide_data) +
  geom_segment(aes(
    x = QuestionID,
    xend = QuestionID,
    y = pre,
    yend = post
  ),
  size = 1,
  arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "open")
  ) +
  labs(
    y = "Percent Correct",
    x = "Question ID"
  ) +
  facet_grid(Group ~ .)


heatplot <- ggplot(wide_data, aes(
  x = QuestionID,
  y = factor(Group, levels = rev(levels(Group)))
)) +
  geom_tile(aes(fill = Delta)) +
  scale_fill_distiller(
    palette = "RdYlGn",
    direction = "1"
  ) +
  labs(
    y = "Group",
    x = "Question ID"
  ) +
  facet_grid(Group ~ ., scales = "free")


grid.arrange(arrowplot, heatplot, ncol = 2)
