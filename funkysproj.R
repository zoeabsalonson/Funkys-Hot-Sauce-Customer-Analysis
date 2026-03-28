# Zoe Absalonson
# Funky's Hot Sauce
# Customer Data Analysis

dev.new()
rm(list=ls())
library(ggplot2)
library(dplyr)
library(scales)
setwd("/Users/zoeabsalonson/Downloads/Funkys")

#Load in data
spenders <- read.csv('spenders.csv', header=TRUE, sep=",")
attach(spenders)



#Load in revenue summary (done in MySQL) - revenue distribution among three spending groups
rev_summary <- read.csv('rev_summary.csv', header=TRUE, sep=",")
#Get total revenue from customer spending
total_rev <- sum(rev_summary$total_spent)



#Combine SMS and email marketing as one category for marketing acceptance
market_acceptance <- ifelse(
  spenders$sms_opt_in == "yes" | spenders$email_opt_in == "yes",
  1,
  0
)



#Update market acceptance to reflect above changes in main table for spenders 
spenders <- cbind(spenders[, -c(5,10)], market_acceptance)



#Plot revenue share for three spending tiers
pdf("t20m60b20.pdf", onefile = TRUE)
#Plot
ggplot(rev_summary, aes(x = segment, y = total_spent)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(
    aes(label = percent(revenue_share, accuracy = 1)),
    vjust = -0.5,
    size = 5
  ) +
  annotate(
    "text",
    x = 1.25,
    y = 5000,
    label = paste(dollar(rev_summary$total_spent[3])),
    hjust = 1,
    color = "white",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 2.3,
    y = 5000,
    label = paste(dollar(rev_summary$total_spent[2])),
    hjust = 1,
    color = "white",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 3.3,
    y = 5000,
    label = paste(dollar(rev_summary$total_spent[1])),
    hjust = 1,
    color = "white",
    size = 3.5
  ) +
  labs(
    title = "Revenue Share by Customer Segment",
    x = "Segment",
    y = "Revenue"
  ) +
  geom_point(
    aes(x = 0, y = total_rev),
    color = "orange",
    size = 4
  ) +
  geom_hline(
    yintercept = total_rev,
    linetype = "dashed",
    color = "orange"
  ) +
  annotate(
    "text",
    x = 0,
    y = total_rev-3000,
    label = paste("Total Revenue:", dollar(total_rev)),
    hjust = 1,
    color = "orange",
    size = 4
  ) +
  scale_y_continuous(labels = dollar_format()) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(plot.margin = margin(10, 80, 10, 60))
dev.off()



#Plot pareto chart for spending (color code three spending tiers)
pdf("spending_pareto.pdf", onefile = TRUE)
spenders %>%
  mutate(
    segment = case_when(
      total_spent >= quantile(total_spent, 0.8) ~ "Top 20%",
      total_spent <= quantile(total_spent, 0.2) ~ "Bottom 20%",
      TRUE ~ "Middle 60%"
    )
  ) %>%
  arrange(desc(total_spent)) %>%
  mutate(cumulative_spend = cumsum(total_spent),
         cumulative_pct = cumulative_spend/sum(total_spent)
  ) %>%
  ggplot(aes(x=reorder(customer_id, -total_spent))) +
  geom_bar(aes(y=total_spent, fill=segment), stat="identity") +
  geom_line(aes(y=cumulative_pct * max(total_spent), group=1),
            color="red", linewidth=1) +
  scale_y_continuous(
    name="Customer Spending",
    sec.axis=sec_axis(~./max(spenders$total_spent),
                      name="Cumulative Percentage")
  ) +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  labs(
    title="Pareto Chart of Customer Spending",
    x="Customers (ordered by spending)",
    fill="Customer Segment"
  )
dev.off()



#Segment spenders into five spending tiers
spenders_segments <- spenders %>%
  mutate(
    segment = case_when(
      ntile(total_spent, 5) == 5 ~ "Tier 1 (High)",
      ntile(total_spent, 5) == 4 ~ "Tier 2",
      ntile(total_spent, 5) == 3 ~ "Tier 3 (Mid)",
      ntile(total_spent, 5) == 2 ~ "Tier 4",
      TRUE ~ "Tier 5 (Low)"
    )
  ) %>%
  group_by(segment) %>%
  summarise(total_spent = sum(total_spent), .groups = "drop") %>%
  #Fix segments so they are ordered from high to low
  mutate(segment = factor(segment,
                          levels = c("Tier 1 (High)", "Tier 2", "Tier 3 (Mid)", "Tier 4", "Tier 5 (Low)")
  )) %>%
  arrange(segment) %>%  #Arrange by segment spending order
  mutate(
    cumulative_spend = cumsum(total_spent),
    cumulative_pct = cumulative_spend / sum(total_spent)
  )

#Plot pareto chart with grouped spending in five tiers
pdf("segmented_spending_pareto.pdf", onefile = TRUE)
ggplot(spenders_segments, aes(x = segment)) +
  geom_bar(aes(y = total_spent, fill = segment), stat = "identity") +
  geom_text(aes(y = total_spent, label = dollar(total_spent)), 
            vjust = -0.5, size = 4) +
  geom_line(aes(y = cumulative_pct * max(total_spent), group = 1), color = "red", linewidth = 1) +
  geom_point(aes(y = cumulative_pct * max(total_spent)), color = "red", size = 2) +
  geom_text(aes(y = cumulative_pct * max(total_spent), 
                label = percent(cumulative_pct)), 
            vjust = -0.5, color = "black", size = 4) +
  scale_y_continuous(
    name = "Total Spending",
    labels = dollar_format(),
    breaks = pretty_breaks(n = 8),
    sec.axis = sec_axis(~./max(spenders_segments$total_spent),
                        name = "Cumulative Percentage",
                        labels = percent_format())
  ) +
  labs(
    title = "Pareto Chart of Customer Spending by Segment",
    x = "Customer Segment",
    fill = "Segment"
  ) +
  theme_minimal()
dev.off()



#Split spenders into subscribed and non-subscribed
s  <- spenders[spenders$market_acceptance == 1, ]
n <- spenders[spenders$market_acceptance == 0, ]



#Get revenue from subscribed and non-subscribed spenders
sub_revenue  <- sum(s$total_spent)
nsub_revenue <- sum(n$total_spent)



#shapiro.test (see if spending is normally distributed):
#Null hypothesis: data is normally distributed (if p-val < 0.05, data not normally distributed)
shapiro.test(spenders$total_spent)
shapiro.test(s$total_spent)
shapiro.test(n$total_spent)

#wilcox.test (nonparametric test for statistical significance in spending difference between subs vs. nonsubs):
#Null hypothesis: nsub spenders are identically distributed to sub spenders with no change (y identically distributed to x+d, d=0)
#Alt hypothesis: d > 0, sub spenders spend a statistically significant amount more
wilcox.test(s$total_spent, n$total_spent, "g")



#Plot subscribed spenders' spending distribution
pdf("subsc_distr_totalspent.pdf", onefile = TRUE)
hist(sub_spenders$total_spent, breaks=60, main="Distribution of Total Spent Among Subscribed SC", xlab="Total Spent (USD)")
dev.off()

#Plot non-subscribed spenders' spending distribution
pdf("nsubsc_distr_totalspent.pdf", onefile = TRUE)
hist(nsub_spenders$total_spent, breaks=60, main="Distribution of Total Spent Among Non-subscribed SC", xlab="Total Spent (USD)")
dev.off()

#Plot (all) spenders' spending distribution
pdf("sc_distr_totalspent.pdf", onefile = TRUE)
hist(spenders$total_spent, breaks=60, main="Distribution of Total Spent Among SC", xlab="Total Spent (USD)")
dev.off()

#Plot spenders' order distribution
pdf("sc_distr_orders.pdf", onefile = TRUE)
hist(spenders$total_orders, breaks=20, main="Distribution of Orders Among SC", xlab="Total Orders")
dev.off()


#Plot sub vs. nonsub smoothed spending distributions w/ medians
pdf("svn_distr.pdf", onefile = TRUE)
plot(density(n$total_spent), col="red", lwd=3,
     main="Spending Distributions",
     xlab="Spending Distribution Among Subs and Non-subs")

lines(density(s$total_spent), col = "green", lwd=3)

abline(v=median(s$total_spent), col="green", lwd=2, lty=2)
abline(v=median(n$total_spent), col="red", lwd=2, lty=2)

legend("topright",
       legend=c("Subscribed SC", "Non-subscribed SC", "Median (Subscribed)", "Median (Non-subscribed)"),
       col=c("green", "red", "green", "red"),
       lwd=c(3,3,2,2),
       lty=c(1,1,2,2))
dev.off()


#Get medians for subs and nsubs spending
median(n$total_spent)
median(s$total_spent)

#Generate linear model for orders vs. spending
order_model <- lm(spenders$total_spent ~ spenders$total_orders)
summary(order_model)

#Linear model diagnostic plots
pdf(file = "OrderModel_Diagnostic_Plots.pdf", width = 8, onefile = TRUE)
par(mfrow = c(2,2))
plot(order_model)
dev.off()

#Order vs. spending scatterplot
pdf("OrderSpendingScatterplot.pdf", onefile = TRUE)
par(mfrow=c(1,1))
plot(spenders$total_orders, spenders$total_spent, pch=20, xlab="Total Orders", ylab="Total Spent", main="Total Orders vs. Total Spent")
dev.off()

#Plot linear model against scatterplot
pdf("OrderModel_Plot.pdf", onefile = TRUE)
par(mfrow=c(1,1))
plot(spenders$total_orders, spenders$total_spent, pch=20, xlab="Total Orders", ylab="Total Spent", main="Linear Model for Orders vs. Spending")
abline(order_model, col="blue", lwd=2)
dev.off()



#Geographic subscriber information
csub <- read.csv('cityrev_sub.csv', header=TRUE, sep=",")

psub_model <- lm(psub$median_spent ~ psub$sub_proportion)
csub_model <- lm(csub$median_spent ~ csub$sub_proportion)
summary(psub_model)

pdf(file = "csub_diag_plots.pdf", width = 8, onefile = TRUE)
par(mfrow = c(2,2))
plot(csub_model)
dev.off()

#Plot sub_proportion vs median spent by city
pdf("city_subvrev.pdf", onefile = TRUE)
plot(csub$sub_proportion, csub$median_spent, pch=20, xlab="Proportion of Subscribed SC", ylab="Median Spent ($USD)", main="Subscribers and Median Spent by city")
abline(csub_model, col="blue", lwd=2)
dev.off()
