#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      retire.R
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-13
# @brief     Produce graphs from retire.out.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       GNU-R
# @see       https://github.com/richmit/FortranFinance
# @copyright
#  @parblock
#  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#  DAMAGE.
#  @endparblock
# @filedetails
#
#  This script will read the file retire.out, and produce several image files.  Optionally those images can also be displayed on the screen when they are
#  created (see the code block immediately following this comment and titled "Displaying Images").
#
#########################################################################################################################################################.H.E.##

#===============================================================================================================================================================
# Displaying Images
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

#===============================================================================================================================================================
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

#===============================================================================================================================================================
if (nSims > 1) {
  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Monte Carlo Graphs

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
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


  byYearSum <- left_join(data.table(Year=minYear:maxYear),
                         bySimSum %>%
                           filter(min_savings<=0.1) %>%
                           transmute(Year=min_savings_year) %>%
                           group_by(Year) %>%
                           summarize(fail_per_year=length(Year)),
                         by='Year') %>%
    arrange(Year) %>%
    mutate(fail_per_year=ifelse(is.na(fail_per_year), 0, fail_per_year)) %>%
    mutate(fail_cum=cumsum(fail_per_year), fail_prob=100*fail_cum/nSims, suc_prob=100-fail_prob)

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  if ((length(unique(bySimSum$died_p1)) > 1) || (length(unique(bySimSum$died_p2)) > 1)) {
    gp <- ggplot(bySimSum %>%
                 transmute(p1=died_p1, p2=died_p2) %>%
                 tidyr::pivot_longer(cols=c(1:2), names_to='Person', values_to='Year')) +
      geom_density(aes(x=Year, y=after_stat(density), group=Person, col=Person), linewidth=2)+
      labs(title='Death') +
      scale_x_continuous(breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(legend.position = "bottom") +
      labs(title=paste('Year Of Death Distribution Based On ',
                       nSims,
                       ' Simulation Runs',
                       sep='')) +
      xlab('') +
      ylab('Probability')
    fname <- 'death_distribution.png'
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  }

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  gp <-ggplot(bySimSum) +
    geom_histogram(aes(x=end_savings+1, y=after_stat(density)), bins=50, fill='pink', col='red') +
    scale_x_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') +
    labs(title=paste('Savings Balance At Death Observed From ', nSims, ' Simulation Runs', sep='')) +
    xlab('Total Savings') +
    ylab('Probability')
  fname <- 'final_savings.png'
  ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))


  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  if (maxYear-minYear > 15) {
    tenYrBal <- bind_rows(daDat %>% filter(Sim %in% bySimSum$Sim[bySimSum$min_savings<=0], Year==(minYear+10))  %>% select(Year, total_savings) %>% mutate(Result='Fail'),
                          daDat %>% filter(Year==(minYear+10)) %>% select(Year, total_savings) %>% mutate(Result='Success'))
    gp <-ggplot(tenYrBal) +
      geom_histogram(aes(x=total_savings, fill=Result, col=Result), bins=50, alpha=0.75, show.legend=FALSE) +
      facet_wrap(~Result, ncol=1, scales = "free") +
      scale_x_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') +
      labs(title=paste('Savings Balance At End Of ', (minYear+10), ' Observed From ', nSims, ' Simulation Runs', sep='')) +
      scale_colour_manual("Trajectory Type", values = c("Fail"="Red",
                                                        "Success"="Blue")) +
      scale_fill_manual("Trajectory Type", values = c("Fail"="Pink",
                                                      "Success"="Lightblue")) +
      theme(legend.position = "bottom") +
      xlab('Total Savings') +
      ylab('Simulation Count')
    fname <- 'y10_savings.png'
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  }

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  portfolio_collapse_probability <- 100.0 * sum(bySimSum$min_savings <= 0) / nSims
  if (sum(bySimSum$min_savings <= 0) > 10) { # Only draw histogram if we have enough collapse cases
    gp <- ggplot(bySimSum %>% filter(min_savings <= 0)) +
      geom_histogram(aes(x=min_savings_year, y=after_stat(density)), fill='pink', col='red', breaks=c(seq(minYear-0.5, maxYear+1, 3))) +
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
    fname <- "probability_of_failure_by_year.png"
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  }

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  gp <- ggplot(daDat) +
    geom_boxplot(aes(x=Year, group=Year, y=total_savings1p)) +
    geom_point(data=byYearSum %>% filter(fail_per_year>0), aes(x=Year, y=1, size=fail_per_year), col='red', alpha=1.0) +
    scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') +
    scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
    coord_cartesian(ylim=c(1, max(daDat$total_savings1p))) +
    scale_size_continuous(name="Account Collapse Count") +
    theme(panel.grid.minor.x = element_blank()) +
    theme(legend.position = "bottom") +
    labs(title=paste('Composite of ', nSims, ' Simulation Runs', sep='')) +
    ylab('Total Savings')
  fname <- "savings_by_year_boxplot.png"
  ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  nColps <- sum(bySimSum$min_savings<=0)
  if (nColps > 0) {

    gp <- ggplot(byYearSum) +
      geom_line(aes(x=Year, y=suc_prob), col='red', linewidth=2, alpha=1.0) +
      scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
      #coord_cartesian(ylim=c(0, max100)) +
      theme(panel.grid.minor.x = element_blank()) +
      labs(title='Probability Of Success') +
      ylab('Probability Of Success (%)') +
      geom_text(aes(x=last(Year), y=last(suc_prob), label=paste(last(suc_prob), '%', sep='')),
                size=10, col='black', vjust="bottom", hjust="right")
    fname <- "probability_of_success_by_year.png"
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

    n <- pmin(2000, nSims)
    gp <- ggplot() +
      geom_line(data=daDat %>%
                  filter(Sim<=n),
                aes(x=Year, y=total_savings1p, group=Sim, col='First 2K Simulations'), linewidth=2, alpha=0.01) +
      geom_line(data=daDat %>%
                  filter(Sim %in% bySimSum$Sim[bySimSum$min_savings<=0]),
                aes(x=Year, y=total_savings1p, group=Sim, col='All Collapse Trajectories'), alpha=0.1, linewidth=0.5) +
      geom_point(data=daDat %>%
                   filter(Sim<=n & S1=='D' & S2=='D') %>%
                   group_by(Sim) %>%
                   summarize(Year=first(Year), total_savings1p=first(total_savings1p)),
                 aes(x=Year, y=total_savings1p, col='Death'), alpha=0.1) +
      geom_line(data=daDat %>%
                  filter(Sim %in% bySimSum$Sim[bySimSum$min_savings<=0]) %>%
                  group_by(Year) %>%
                  summarize(maxc=max(total_savings1p), .groups='drop') %>%
                  group_by(Year),
                aes(x=Year, y=maxc, col='Collapse Trajectory Envelope'), alpha=1.0) +
      scale_colour_manual("", values = c("First 2K Simulations"="black",
                                         "All Collapse Trajectories"="red",
                                         "Death"="blue",
                                         "Collapse Trajectory Envelope"="green")) +
      scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') +
      scale_x_continuous(name='', breaks=timeBrks, labels=timeLabs, minor_breaks=minYear:maxYear) +
      coord_cartesian(ylim=c(1, max(daDat$total_savings1p))) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(legend.position = "bottom") +
      labs(title=paste('Composite of ', n, ' Simulation Runs and ', nColps, ' failure Runs Of ', nSims, ' Total Runs', sep='')) +
      ylab('Total Savings')
    fname <- "composite_trajectories.png"
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

    if ((nSims < 100001) && (maxYear-minYear > 15)) {
      tmp <- daDat %>%
        filter(Year<2040) %>%
        group_by(Sim) %>%
        summarize(minB=min(total_savings1p), maxB=max(total_savings1p), .group='drop') %>%
        summarize(ll=quantile(minB, 0), ul=quantile(maxB, .9))
      gp <- ggplot() +
        geom_line(data=daDat %>% filter(Year<2040)%>% filter(Sim %in% bySimSum$Sim[bySimSum$min_savings>0]),
                  aes(x=Year, y=total_savings1p, group=Sim, col='Success Trajectories'), linewidth=2, alpha=0.01) +
        ## geom_line(data=daDat %>% filter(Year<2040) %>% filter(Sim %in% bySimSum$Sim[bySimSum$min_savings<=0]),
        ##           aes(x=Year, y=total_savings1p, group=Sim, col='Fail Trajectories'), alpha=0.5, linewidth=0.5) +
        scale_colour_manual("", values = c("Success Trajectories"="black", "Fail Trajectories"="red")) +
        scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), n.breaks=20) +
        scale_x_continuous(name='', breaks=minYear:(minYear+10)) +
        coord_cartesian(ylim=c(tmp$ll, tmp$ul), xlim=c(minYear, minYear+10)) +
        theme(panel.grid.minor.x = element_blank()) +
        theme(legend.position = "bottom") +
        labs(title=paste('Composite of ', nSims, ' Simulation Runs Over First 10 Years', sep='')) +
        ylab('Total Savings')
      fname <- "composite_trajectories_y10.png"
      ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
      if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
    }

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
    fname <- "composite_trajectories.png"
    ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
    if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
  }

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  fname <- 'savings_by_year_probability_bands.png'
  ggsave(fname, width=15, height=10, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

} else {
  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Non-Monte Carlo Graphs

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  fname <- "savings_by_type.png"
  ggsave(fname, width=12, height=7, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  fname <- "paid_and_unpaid.png"
  ggsave(fname, width=12, height=7, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

  #-------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  fname <- "paid_by_source.png"
  ggsave(fname, width=12, height=7, dpi=100, units='in', plot=gp);
  if (is.character(imageV)) system(paste(imageV, fname, sep=' '))
}
