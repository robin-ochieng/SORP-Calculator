  list(
  
  SORP_Fund <- function(SORP_Contributions){
    fund_FV <- SORP_Contributions[length(SORP_Contributions[, "FundValue"]), "FundValue"]
    return(fund_FV)
  },

  SORP_Pension_Payment <- function(SORP_Fund, SORP_Annuity, freq){
    freq = p_list[match(freq, freq_list)]
    pension_payment = SORP_Fund / SORP_Annuity / freq
    return(pension_payment)
  },

  SORP_Discount <- function(x, age_1, age_2, discount_rate = 2.5){
    discount_factor = 1/((1 + discount_rate/100)^(age_2 - age_1))
    return(x * discount_factor)
  },

  SORP_Cumulative_Payments <- function(SORP_Pension_Payment, time){
    return(SORP_Pension_Payment * time)
  },

  SORP_Plot_FundValue <- function(SORP_Contributions, freq){
    freq = p_list[match(freq, freq_list)]
    AgeandFundValue <- SORP_Contributions %>% select(age_exact, FundValue)
    p <- ggplot(AgeandFundValue, aes(x = age_exact + (1 / freq), y = FundValue, fill = "#4A8DBF", colour = "#4A8DBF")) +
      geom_bar(stat = "identity", colour = "#4A8DBF", fill = "#4A8DBF") + 
      labs(x = "Age", y = "Fund Value", fill = NULL, color = NULL) +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(labels = scales::dollar_format(prefix = "€"), expand = c(0, 0)) + 
      theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    return(p)
  },

    SORP_Contributions <- function(age_1, age_2, salary, current_fundvalue, freq, emp_contri, empr_contri, salary_esc = 1.5, investment_charges = 0.5, equity_prop = 40, equity_rate = 4.5, fixed_prop = 30, fixed_rate = 1, cash_prop = 30, cash_rate = 0){
    
    freq = p_list[match(freq, freq_list)]
    
    contributions_interest_annual = ((equity_prop/100) * (equity_rate/100)) + ((fixed_prop/100) * (fixed_rate/100)) + ((cash_prop/100) * (cash_rate/100)) - (investment_charges/100)
    
    contributions_interest = effective2Convertible(i = contributions_interest_annual, k = freq) / freq
    
    fundValueX = numeric((age_2 - age_1)*freq + 1)
    ages = numeric((age_2 - age_1)*freq + 1)
    ages_exact = numeric((age_2 - age_1)*freq + 1)
    periods = numeric((age_2 - age_1)*freq + 1)
    EEContribution = numeric((age_2 - age_1)*freq + 1)
    ERContribution = numeric((age_2 - age_1)*freq + 1)
    totalContribution = numeric((age_2 - age_1)*freq + 1)
    interestOnFund = numeric((age_2 - age_1)*freq + 1)
    
    ages[1] = age_1 - 1
    ages_exact[1] = age_1 - 1/freq
    periods[1] = freq
    fundValueX[1] = current_fundvalue
    
    for (m in 2:((age_2 - age_1)*freq + 1)){
      ages[m] = ages[m-1]
      ages_exact[m] = ages_exact[m - 1] + 1 / freq
      periods[m] = periods[m - 1] + 1
      EEContribution[m] = salary * (emp_contri / 100) / freq
      ERContribution[m] = salary * (empr_contri / 100) / freq
      totalContribution[m] = EEContribution[m] + ERContribution[m]
      interestOnFund[m] = fundValueX[m - 1] * contributions_interest
      fundValueX[m] = fundValueX[m - 1] + interestOnFund[m] + totalContribution[m]
      
      if((m - 1)%%freq == 0){
        salary = salary * (1 + (salary_esc/100))
      }
      if((m - 2)%%freq == 0){
        periods[m] = 1
        ages[m] = ages[m - 1] + 1
      }
    }
    
    df_fund <- data.frame(age = ages, period = periods, age_exact = ages_exact,
                          EmployeeContribution = EEContribution,
                          EmployerContribution = ERContribution,
                          TotalContribution = totalContribution,
                          InterestOnFund = interestOnFund,
                          FundValue = fundValueX
    )
    return(df_fund)
  },

  # SORP Functions ----------------------------------------------------------
  SORP_Annuity <- function(age_1, age_2 = age_1, relationship, freq, annuity_interest = 0.5, annuity_esc = 1, guaranteed = 5, deferred = F){
    freq = p_list[match(freq, freq_list)]
    
    net_interest = ((1 + (annuity_interest/100))/(1 + (annuity_esc/100))) - 1
    
    # annuities will increase by 0.33% per annum compound
    years_to_buy = as.numeric(format(Sys.Date(), "%Y")) + (age_2 - age_1)
    ann_inc_rate = 1.0033 ^ (years_to_buy - 2013)
    
    if(deferred == F){
      guar_ann = annuity(i = net_interest, n = guaranteed, k = freq, type = "advance")
      if (relationship == 1) {
        annuity = (guar_ann + axn(ILT15_female_reduced, x = age_2, i = net_interest, k = freq, m = guaranteed, payment = "advance")) * ann_inc_rate
      } else {
        annuity = (guar_ann + axyzn(listOfTables, x = c(age_2, age_2), i = net_interest, m = guaranteed, k = freq, status = "last", payment = "advance")) * ann_inc_rate
      }
    } else {
      if (relationship == 1) {
        annuity = axn(ILT15_female_reduced, x = age_1, i = net_interest, k = freq, m = (age_2 - age_1), payment = "advance") * ann_inc_rate
      } else {
        annuity = axyzn(listOfTables, x = c(age_1, age_1), i = net_interest, m = (age_2 - age_1), k = freq, status = "last", payment = "advance") * ann_inc_rate
      }
    }
    return(annuity)
  },

    SORP_Table_Contributions <- function(SORP_Contributions, freq){
    SORP_Contributions[1, 1:(length(SORP_Contributions[1, ]) - 1)] <- "-"
    freq = p_list[match(freq, freq_list)]
    if(freq == 1){
      SORP_Contributions = data.frame(select(SORP_Contributions, -period, -age_exact))
      colnames(SORP_Contributions) = c("Age", "Employee Contribution", "Employer Contribution", "Total Contribution", "Interest on Fund", "Fund Value at End of Period")
    } else {
      SORP_Contributions = data.frame(select(SORP_Contributions, -age_exact))
      colnames(SORP_Contributions) = c("Age", "Period", "Employee Contribution", "Employer Contribution", "Total Contribution", "Interest on Fund", "Fund Value at End of Period")
    }
    SORP_Contributions <- datatable(SORP_Contributions, options = list(scrollX = TRUE, scrollY = "300px", paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames= FALSE)
    SORP_Contributions <- formatCurrency(SORP_Contributions, columns = c("Employee Contribution", "Employer Contribution", "Total Contribution", "Interest on Fund", "Fund Value at End of Period"), currency = "KES ", digits = 0)
    return(SORP_Contributions)
  }

  )

