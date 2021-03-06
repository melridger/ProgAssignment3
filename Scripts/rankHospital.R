rankhospital <- function(state, outcome, num = "best") {
        
        out_dt <- data.table::fread('outcome-of-care-measures.csv')
        
        outcome <- tolower(outcome)
        
        # Change Column name  
        chosen_state <- state 
        
        
        if (!chosen_state %in% unique(out_dt[["State"]])) {
                stop('invalid state')
        }
        
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop('invalid outcome')
        }
        
        # Renaming Columns 
        setnames(out_dt
                 , tolower(sapply(colnames(out_dt), gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
        )
        
        #Filter by state
        out_dt <- out_dt[state == chosen_state]
        
       
        col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(out_dt))
        
        # Filtering out unnecessary data 
        out_dt <- out_dt[, .SD ,.SDcols = col_indices]
        
        
        out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
        
        
        # Remove Missing Values for numerical datatype 
        out_dt <- out_dt[complete.cases(out_dt),]
        
        # Order Column to Top 
        out_dt <- out_dt[order(get(outcome), `hospital name`)]
        
        out_dt <- out_dt[,  .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
        
        if (num == "best"){
                return(out_dt[1,`hospital name`])
        }
        
        if (num == "worst"){
                return(out_dt[.N,`hospital name`])
        }
        
        return(out_dt[num,`hospital name`])
        
}

