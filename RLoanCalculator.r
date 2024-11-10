# Loan Calculator in R

# Function to calculate monthly repayment and total interest
calculate_loan <- function(loan_amount, annual_interest_rate, loan_term_years) {
  # Monthly interest rate
  monthly_interest_rate <- (annual_interest_rate / 100) / 12
  
  # Loan term in months
  loan_term_months <- loan_term_years * 12
  
  # Total interest calculation
  total_interest <- loan_amount * monthly_interest_rate * loan_term_months
  
  # Monthly repayment calculation
  monthly_repayment <- (loan_amount + total_interest) / loan_term_months
  
  # Return the results
  list(
    loan_amount = loan_amount,
    annual_interest_rate = annual_interest_rate,
    loan_term_months = loan_term_months,
    monthly_repayment = monthly_repayment,
    total_interest = total_interest
  )
}

# Input prompts
loan_amount <- as.numeric(readline(prompt = "Enter the loan amount (PHP): "))
annual_interest_rate <- as.numeric(readline(prompt = "Enter the annual interest rate (%): "))
loan_term_years <- as.numeric(readline(prompt = "Enter the loan term (years): "))

# Perform calculations
result <- calculate_loan(loan_amount, annual_interest_rate, loan_term_years)

# Display the results
cat("\nLoan Amount: PHP", result$loan_amount, "\n")
cat("Annual Interest Rate:", result$annual_interest_rate, "%\n")
cat("Loan Term:", result$loan_term_months, "months\n")
cat("Monthly Repayment: PHP", round(result$monthly_repayment, 2), "\n")
cat("Total Interest: PHP", round(result$total_interest, 2), "\n")
