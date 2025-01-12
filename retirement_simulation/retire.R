#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set this to your favorite image viewer, or TRUE to attempt to find one automatically, or FALSE to not load images
imageV <- TRUE

# Try and find an image viewer
if (imageV == TRUE) {
  if (.Platform$OS == "windows") {
    imageV <- "explorer"
  } else {
    for(piv in c("/usr/bin/display", "/usr/bin/pqiv", "/usr/bin/nomacs"))
      if(file.exists(piv))
        imageV <- piv
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
daDat<-
  fread('retire.out') %>%
  mutate(sim=factor(Sim),
         total_savings=SavingsC+SavingsE+SavingsB+SavingsI1+SavingsI2+SavingsR1+SavingsR2,
         total_savings1p=total_savings+1)

ya12map <- unique(daDat %>% select(Year, Age1, Age2)) %>% transmute(lab=paste(Year, Age1, Age2, sep='\n'), Year, Age1, Age2)

y2lab <- function(y) ya12map$lab[ya12map$Year==y]

nSims <- length(unique(daDat$Sim))

maxYear <- max(daDat$Year)
minYear <- min(daDat$Year)

timeBrks <- c(minYear, seq(minYear+4, maxYear, 3))
timeLabs <- sapply(timeBrks, y2lab)
timeLabs[1] <- 'Year\nAge1\nAge2'

if (nSims > 1) {

  bySimSum <- 
    daDat %>% 
    group_by(Sim) %>% 
    summarize(min_savings      = min(total_savings), 
              min_savings_year = Year[which.min(total_savings)], 
              max_savings      = max(total_savings),
              max_savings_year = Year[which.max(total_savings)],
              end_savings      = last(total_savings),
              died_p1          = max(Year[S1!='D'])+1,
              died_p2          = max(Year[S2!='D'])+1,
              end_year         = max(Year))

  if (length(unique(bySimSum$end_year)) > 1) {
    gp <- ggplot(bySimSum %>% 
                 transmute(p1=died_p1, p2=died_p2) %>%
                 tidyr::pivot_longer(cols=c(1:2), names_to='Person', values_to='Year')) +
      geom_histogram(aes(x=Year, y=..density..), fill='pink', col='red', breaks=c(seq(minYear-0.5, maxYear+1, 3))) +
      facet_wrap(~Person, nrow=2) +
      labs(title='Death') +
      scale_x_continuous(breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
      theme(panel.grid.minor.x = element_blank()) + 
      labs(title=paste('Year Of Death Distribution Based On ', 
                       nSims, 
                       ' Simulation Runs',
                       sep='')) +
      xlab('') +
      ylab('Probability')
    fname <- 'deathDist.png'
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  }

  gp <-ggplot(bySimSum) + 
    geom_histogram(aes(x=end_savings+1, y=..density..), bins=50, fill='pink', col='red') +
    scale_x_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') + 
    labs(title=paste('Savings Balance At Death Observed From ', nSims, ' Simulation Runs', sep='')) +
    xlab('Total Savings') +
    ylab('Probability')
  fname <- 'endSavings.png'
  ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

  portfolio_collapse_probability <- 100.0 * sum(bySimSum$min_savings <= 0) / nSims
  if (sum(bySimSum$min_savings <= 0) > 10) { # Only draw histogram if we have enough collapse cases
    gp <- ggplot(bySimSum %>% filter(min_savings <= 0)) + 
      geom_histogram(aes(x=min_savings_year, y=..density..), fill='pink', col='red', breaks=c(seq(minYear-0.5, maxYear+1, 3))) +
      labs(title='Account Collapse vs Age') +
      scale_x_continuous(breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
      theme(panel.grid.minor.x = element_blank()) + 
      labs(title=paste('Age Distribution For Portfolio Collapse Based On ', 
                       nSims, 
                       ' Simulation Runs (Success Rate: ', 
                       100-portfolio_collapse_probability, 
                       ')', 
                       sep='')) +
      xlab('') +
      ylab('Probability')
    fname <- "brokeAge.png"
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  }

  gp <- ggplot(daDat) + 
    geom_boxplot(aes(x=Year, group=Year, y=total_savings1p)) +
    scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') + 
    scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
    coord_cartesian(ylim=c(1, max(daDat$total_savings1p))) +
    theme(panel.grid.minor.x = element_blank()) + 
    labs(title=paste('Composite of ', nSims, ' Simulation Runs', sep='')) +
    ylab('Total Savings')
  fname <- "simCompBox.png"
  ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

  nColps <- sum(bySimSum$min_savings<=0)
  if ((nColps > 0) && (nColps < 4000)) {
    n <- pmin(2000, nSims)
    gp <- ggplot() + 
      geom_line(data=daDat %>% filter(Sim<=n),
                aes(x=Year, y=total_savings1p, group=Sim), linewidth=2, alpha=0.01, show.legend=FALSE) + 
      geom_line(data=daDat %>% filter(Sim %in% bySimSum$Sim[bySimSum$min_savings<=0]), 
                aes(x=Year, y=total_savings1p, group=Sim), show.legend=FALSE, alpha=0.1, linewidth=0.5, col='red') + 
      geom_point(data=daDat %>% filter(Sim<=n & S1=='D' & S2=='D') %>% group_by(Sim) %>% summarize(Year=first(Year), total_savings1p=first(total_savings1p)),
                 aes(x=Year, y=total_savings1p), show.legend=FALSE, alpha=0.1, col='blue') + 
      geom_line(data=daDat %>% 
                  filter(Sim %in% bySimSum$Sim[bySimSum$min_savings<=0]) %>% 
                  group_by(Year) %>% 
                  summarize(maxc=max(total_savings1p), .groups='drop') %>% 
                  group_by(Year),
                aes(x=Year, y=maxc), show.legend=FALSE, alpha=1.0, col='green') + 
      scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') + 
      scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
      coord_cartesian(ylim=c(1, max(daDat$total_savings1p))) + 
      theme(panel.grid.minor.x = element_blank()) + 
      labs(title=paste('Composite of ', n, ' Simulation Runs and ', nColps, ' failure Runs Of ', nSims, ' Total Runs', sep='')) +
      ylab('Total Savings')
    fname <- "compColCases.png"
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  } else {
    n <- pmin(2000, nSims)
    gp <- ggplot(daDat %>% filter(Sim<=n)) + 
      geom_line(aes(x=Year, y=total_savings1p, group=Sim), linewidth=2, alpha=0.01, show.legend=FALSE) + 
      scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') + 
      scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
      coord_cartesian(ylim=c(1, max(daDat$total_savings1p))) + 
      theme(panel.grid.minor.x = element_blank()) + 
      labs(title=paste('Composite of ', n, ' Simulation Runs Of ', nSims, ' Total Runs', sep='')) +
      ylab('Total Savings')
    fname <- "simCompLine.png"
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  }

  bands <- c(90, 80, 50) # 100, 99, 98, 97, 96, 95, 
  bandp <- paste(bands, '%', sep='')
  gp <- ggplot(do.call(rbind, lapply(bands, 
                                     function(b) group_by(daDat, Year) %>% 
                                                   summarize(grp  = paste(b, '%', sep=''), 
                                                             low  = pmax(1, quantile(total_savings, (50-b/2)/100)), 
                                                             high = pmax(1, quantile(total_savings, (50+b/2)/100)), .groups='drop'))) %>% 
                                                   mutate(grp=factor(grp, levels=bandp))) + 
    geom_ribbon(aes(x=Year, ymin=low, ymax=high, fill=grp), alpha=1.0) +
    geom_line(data=group_by(daDat, Year) %>% 
                     summarize(m=pmax(1, quantile(total_savings, .5)), .groups='drop'), 
              aes(x=Year, y=m), col='black', linewidth=1) +
    scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') +
    scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
    coord_cartesian(ylim=c(1, max(daDat$total_savings1p))) + 
    labs(title='Total Savings Balance Probability Ranges') +
    ylab('Value') +
    xlab('Age2') +
    theme(legend.position = "bottom") +
    scale_fill_brewer(name='Probability', palette='RdYlGn', direction=1, labels=bandp)
  fname <- 'savings_crg.png'
  ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

} else {

  gp <- ggplot(daDat %>% 
               select(Year, 
                      S1_Brokerage_Accounts=SavingsB, 
                      S2_ira_p1=SavingsI1, 
                      S3_ira_p2=SavingsI2, 
                      S4_roth_ira_p1=SavingsR1, 
                      S5_roth_ira_p2=SavingsR2, 
                      S6_Emergency_Fund=SavingsE, 
                      S7_Cash=SavingsC) %>% 
               tidyr::pivot_longer(cols=c(2:8), names_to='Savings_Type', values_to='Savings')) + 
    geom_bar(aes(x=Year, y=Savings, fill=Savings_Type), stat='identity') +
    scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
    labs(title='Savings Account Balances') +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()))
  fname <- "savings.png"
  ggsave(fname, width=12, height=7, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

  gp <- ggplot(daDat %>% 
               transmute(Year, 
                         Paid_Taxes=TPaidSR+TPaidI+TPaidST+TPaidSI,
                         Unpaid_Taxes=Taxes-Paid_Taxes,
                         Paid_Living_Expenses=EPaidSR+EPaidI+EPaidST+EPaidSI,
                         Unpaid_Living_Expenses=Expenses-Paid_Living_Expenses) %>% 
               tidyr::pivot_longer(cols=c(2:5), names_to='Expense_Type', values_to='Expense') %>%
               mutate(Expense_Type=factor(Expense_Type, c('Paid_Taxes', 
                                                          'Paid_Living_Expenses',
                                                          'Unpaid_Taxes', 
                                                          'Unpaid_Living_Expenses')))) +
    geom_bar(aes(x=Year, y=Expense, fill=Expense_Type), stat='identity') +
    scale_fill_manual(name='Expense Type: ',
                      values=c('Paid_Taxes'='chartreuse3',
                               'Paid_Living_Expenses'='chartreuse4',
                               'Unpaid_Taxes'='pink',
                               'Unpaid_Living_Expenses'='brown2')) +
    labs(title='Paid & Unpaid Expenses') +
    theme(legend.position = "bottom") +
    scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
    scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()))
  fname <- "paidunpaid.png"
  ggsave(fname, width=12, height=7, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

  gp <- ggplot(daDat %>% 
               transmute(Year, 
                         Income=TPaidI+EPaidI,
                         Ira=TPaidSI+EPaidSI,
                         Roth=TPaidSR+EPaidSR,
                         Savings=TPaidST+EPaidST,
                         Unpaid=(Taxes-TPaidI-TPaidST-TPaidSI-TPaidSR)+(Expenses-EPaidI-EPaidST-EPaidSI-EPaidSR)) %>% 
               tidyr::pivot_longer(cols=c(2:6), names_to='Payment_Source', values_to='Expense') ) +
    geom_bar(aes(x=Year, y=Expense, fill=Payment_Source), stat='identity') +
    scale_fill_manual(name='Payment Source: ',
                      values=c('Income'='chartreuse3',
                               'Ira'='indianred',
                               'Roth'='salmon',
                               'Savings'='pink',
                               'Unpaid'='red')) +
    labs(title='Payment Sources') +
    theme(legend.position = "bottom") +
    scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
    scale_y_continuous(labels = scales::label_dollar(scale=1/1000, suffix="K"))
  fname <- "paidsource.png"
  ggsave(fname, width=12, height=7, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
}
