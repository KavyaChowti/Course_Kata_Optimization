responses = read.csv(file.choose(), header = T)
attach(responses)
library(dplyr)

responses = responses %>%
  drop_na()

checkpoint_pulse = read.csv(file.choose(), header = T)
attach(checkpoint_pulse)

checkpoint_pulse = checkpoint_pulse %>%
  drop_na()

checkpoint_eoc = read.csv(file.choose(), header = T)         
attach(checkpoint_eoc)

checkpoint_eoc = checkpoint_eoc %>%
  drop_na()

checkpoints = inner_join(checkpoint_pulse, checkpoint_eoc, by = 'student_id', relationship = "many-to-many")

checkpoints = checkpoints %>%
  group_by(checkpoints$student_id, checkpoints$chapter_number.x)

unique(checkpoints$student_id)






# TIME SPENT ON CHAPTERS AND VIDEOS
media_views = read.csv(file.choose(), header = T)
attach(media_views)

media_views = media_views %>%
  drop_na() %>% 
  group_by(book)

media_views_ABC = subset(media_views, book == "College / Statistics and Data Science (ABC)")
media_views_ABCD = subset(media_views, book == "College / Advanced Statistics and Data Science (ABCD)")


tapply(media_views_ABC$access_count, media_views_ABC$chapter, mean)
tapply(media_views_ABC$proportion_video, media_views_ABC$chapter, mean)
tapply(media_views_ABC$proportion_time, media_views_ABC$chapter, mean)


tapply(media_views_ABCD$access_count, media_views_ABCD$chapter, mean)
tapply(media_views_ABCD$proportion_video, media_views_ABCD$chapter, mean)
tapply(media_views_ABCD$proportion_time, media_views_ABCD$chapter, mean)

# Subset data for type of book
version_ABC <- media_views[media_views$book == "College / Statistics and Data Science (ABC)", "access_count"]
version_ABCD <- media_views[media_views$book == "College / Advanced Statistics and Data Science (ABCD)", "access_count"]

# Calculate price difference
access_count_difference <- mean(version_ABC$access_count) - mean(version_ABCD$access_count)

# Bootstrap resampling to compute confidence interval
access_count_diff_bootstrap <- replicate(10000, {
  ABC_sample <- sample(version_ABC$access_count, replace = TRUE)
  ABCD_sample <- sample(version_ABCD$access_count, replace = TRUE)
  mean(ABC_sample) - mean(ABCD_sample)
})

# Compute confidence interval
access_count_diff_ci <- quantile(access_count_diff_bootstrap, c(0.025, 0.975))

# Results
access_count_difference
access_count_diff_ci









# ACCESS COUNT DIFFERENCE
# Subset data for chapter number
chapter4 <- media_views[media_views$chapter_number == "4", "access_count"]
chapter5 <- media_views[media_views$chapter_number == "5", "access_count"]

# Calculate price difference
access_count_difference_between_chapters <- mean(chapter4$access_count) - mean(chapter5$access_count)

# Bootstrap resampling to compute confidence interval
access_count_diff_chap_bootstrap <- replicate(10000, {
  chap4_sample <- sample(chapter4$access_count, replace = TRUE)
  chap5_sample <- sample(chapter5$access_count, replace = TRUE)
  mean(chap4_sample) - mean(chap5_sample)
})

# Compute confidence interval
access_count_chap_diff_ci <- quantile(access_count_diff_chap_bootstrap, c(0.025, 0.975))

# Results
access_count_difference_between_chapters
access_count_chap_diff_ci










# VIDEO PROPORTION
chapter4_prop_video <- media_views[media_views$chapter_number == "4", "proportion_video"]
chapter5_prop_video <- media_views[media_views$chapter_number == "5", "proportion_video"]

# Calculate price difference
prop_video_difference_between_chapters <- mean(chapter4_prop_video$proportion_video) - mean(chapter5_prop_video$proportion_video)

# Bootstrap resampling to compute confidence interval
prop_video_diff_chap_bootstrap <- replicate(10000, {
  chap4_video_sample <- sample(chapter4_prop_video$proportion_video, replace = TRUE)
  chap5_video_sample <- sample(chapter5_prop_video$proportion_video, replace = TRUE)
  mean(chap4_video_sample) - mean(chap5_video_sample)
})

# Compute confidence interval
prop_video_chap_diff_ci <- quantile(prop_video_diff_chap_bootstrap, c(0.025, 0.975))

# Results
prop_video_difference_between_chapters
prop_video_chap_diff_ci










# TIME PROPORTION
chapter4_prop_time <- media_views[media_views$chapter_number == "4", "proportion_time"]
chapter5_prop_time <- media_views[media_views$chapter_number == "5", "proportion_time"]

# Calculate price difference
prop_time_difference_between_chapters <- mean(chapter4_prop_time$proportion_time) - mean(chapter5_prop_time$proportion_time)

# Bootstrap resampling to compute confidence interval
prop_time_diff_chap_bootstrap <- replicate(10000, {
  chap4_time_sample <- sample(chapter4_prop_time$proportion_time, replace = TRUE)
  chap5_time_sample <- sample(chapter5_prop_time$proportion_time, replace = TRUE)
  mean(chap4_time_sample) - mean(chap5_time_sample)
})

# Compute confidence interval
prop_time_chap_diff_ci <- quantile(prop_time_diff_chap_bootstrap, c(0.025, 0.975))

# Results
prop_time_difference_between_chapters
prop_time_chap_diff_ci




# CHECKING THE SCORES BETWEEN THE CHAPTERS

checkpoint_eoc = filter(checkpoint_eoc, checkpoint_eoc$chapter_number == 4 | checkpoint_eoc$chapter_number == 5)

 

video_checkpoints = inner_join(media_views, checkpoint_eoc, by = 'student_id', relationship = "many-to-many")

tapply(video_checkpoints$EOC, video_checkpoints$chapter, mean)

#CHECKING IF THE MEAN SCORES ARE SIGNIFICANT
chapter4_score <- video_checkpoints[video_checkpoints$chapter == "Chapter 4 - Explaining Variation", "EOC"]
chapter5_score <- video_checkpoints[video_checkpoints$chapter == "Chapter 5 - A Simple Model", "EOC"]

# Calculate price difference
EOC_diff_between_chap <- mean(chapter4_score$EOC) - mean(chapter5_score$EOC)

# Bootstrap re-sampling to compute confidence interval
EOC_diff_chap_bootstrap <- replicate(10000, {
  chap4_EOC_sample <- sample(chapter4_score$EOC, replace = TRUE)
  chap5_EOC_sample <- sample(chapter5_score$EOC, replace = TRUE)
  mean(chap4_EOC_sample) - mean(chap5_EOC_sample)
})

# Compute confidence interval
EOC_chap_diff_ci <- quantile(EOC_diff_chap_bootstrap, c(0.025, 0.975))

# Results
EOC_diff_between_chap
EOC_chap_diff_ci





# LINEAR REGRESSION FOR EOC SCORE

eoc.lm = lm(video_checkpoints$EOC ~ video_checkpoints$chapter + video_checkpoints$n_attempt)
summary(eoc.lm)
anova(eoc.lm)
confint(eoc.lm)





# COMPARING CHAPTERS WITH VIDEOS TO CHAPTERS WITHOUT VIDEOS 

checkpoints1 = read.csv(file.choose(), header = T)
attach(checkpoints1)


tapply(EOC, checkpoints1$chapter_number, mean)
