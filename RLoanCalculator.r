// ********************
// Last names: Foo
// Language: R
// Paradigm(s): Procedural
// ********************


# Function to validate user input
validate_input <- function(prompt_text) {
  while (TRUE) {
    user_input <- as.numeric(readline(prompt = prompt_text))
    if (!is.na(user_input) && user_input > 0) {
      return(user_input)
    } else {
      cat("Invalid input. Please enter a positive number.\n")
    }
  }
}

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

# Main loop for the program
repeat {
  cat("\n--- Simple Loan Calculator ---\n")
  
  # Input prompts with validation
  loan_amount <- validate_input("Enter the loan amount (PHP): ")
  annual_interest_rate <- validate_input("Enter the annual interest rate (%): ")
  loan_term_years <- validate_input("Enter the loan term (years): ")
  
  # Perform calculations
  result <- calculate_loan(loan_amount, annual_interest_rate, loan_term_years)
  
  # Display the results
  cat("\nLoan Amount: PHP", result$loan_amount, "\n")
  cat("Annual Interest Rate:", result$annual_interest_rate, "%\n")
  cat("Loan Term:", result$loan_term_months, "months\n")
  cat("Monthly Repayment: PHP", round(result$monthly_repayment, 2), "\n")
  cat("Total Interest: PHP", round(result$total_interest, 2), "\n")
  
  repeat {
    continue <- readline(prompt = "\nDo you want to perform another calculation? (yes/no): ")
    continue <- tolower(trimws(continue))  # Normalize and trim whitespace
    if (continue %in% c("yes", "y")) {
      break  # Continue the main loop for another calculation
    } else if (continue %in% c("no", "n")) {
      cat("\nExiting the program. Goodbye!\n")
      quit(save = "no")  # Exit the program
    } else {
      cat("Invalid input. Please type 'yes' or 'no'.\n")
    }
  }
}
