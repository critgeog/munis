

# muni_analysis


# create tbl of key variables: ihprops, all sfs, all sfrs 
ih_acs_sf %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  group_by(is_hms18) %>%
  filter(!(is.na(is_hms18))) %>%
  filter(!(is.na(mm18))) %>%
  select(is_hms18, ihprops, tot_sf, t_rentocc_sf) %>%
  pivot_longer(!is_hms18) %>%
  group_by(name,is_hms18) %>%
  summarize(total = sum(value)) %>%
  mutate(freq = total / sum(total),
         pct = round((freq*100), 1)) %>%
  # select(-(freq))  %>%                # remove freq to make smaller table
  pivot_wider(names_from = name, values_from = c(total:pct))->ih_hms18

# table of above
formattable(ih_hms18)

# Invitation Homes website colors
# #79C23C - Investor Green
# #626262 - Dark Gray
# #F1F1F1 - light Gray

ggplot(ih_hms18, aes(x = reorder(is_hms18, desc(is_hms18)), y = total_ihprops)) +
  geom_col(fill = "#79C23C", position = 'dodge') +
  labs(x = NULL, y = "Total SFRs",
       title = "Invitation Homes SFRs, by Income Category") +
  scale_y_continuous(labels = scales::comma) +
  # theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme_ipsum(axis_text_size = 9) +
  coord_flip()

ih_acs_sf %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  group_by(is_hms18) %>%
  filter(!(is.na(mm18))) %>%
  filter(!(is.na(is_hms18))) %>%
  select(is_hms18, ihprops, tot_sf, t_rentocc_sf) %>%
  pivot_longer(!is_hms18) %>%
  group_by(name,is_hms18) %>%
  summarize(total = sum(value)) %>%
  mutate(freq = total / sum(total),
         pct = round((freq*100), 1)) %>%
  ggplot(mapping = aes(x = reorder(name, desc(name)), y = pct, fill = reorder(is_hms18, desc(is_hms18)),label = pct)) +
  geom_col(position = 'stack', width = .7) +
  # ggplot(mapping = aes(x = urb_yr, y = pct, fill = urb_yr)) +
  # geom_col(position = 'dodge2', show.legend = FALSE) +
  scale_fill_manual(values = inc_col,
                    limits = c("Poor", "Middle", "Professional","Rich"),
                    name = "") +
  scale_x_discrete(labels = c("All Single-Family Units", "All SFRs", "INVH SFRs")) +
  labs(x = '', y = 'Percent', fill = '',
       title = 'Share of Housing Units Across Income Categories',
       subtitle = "Comparing Invitation Homes and the Single-Family Housing Stock") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  coord_flip() +
  theme_ipsum() + 
  theme(legend.position = 'top')