# downloaded 25 October 2018 from http://www.dataanalytics.org.uk/Publications/S4E2e%20Support/data/Beta%20coeff%20calc.R

## Beta Coefficient Calculator
## Mark Gardener 2011 (Rev. 2015)
## www.dataanalytics.org.uk

## This is how you make a beta coeff:
## beta = coef * sd(x) / sd(y)

# ------------------------

beta.coef <- function(model, digits = 7) { # start function code
  
  # model  = linear model from lm()
  # digits = the decimal places to display (default = 7)
  # Code by: Mark Gardener 2015 www.dataanalytics.org.uk
  
  ## Error checking
  
  if(missing(model))                              # make sure model is given...
    stop('Please supply a linear model result\n') # ...give a message if not
  
  ## Set variables
  
  vars <- model$model                           # get model data
  nvar <- length(vars)                        # how many variables in model
  sdev <- apply(vars, 2, sd)				   # Get Std. Dev. for variables
  sdev <- as.data.frame(as.matrix(t(sdev))) # Make data.frame
  cf <- coef(model)				       # get coefficients
  cf <- as.data.frame(as.matrix(t(cf)))   # Make data.frame
  
  ## Do calculations and make results
  
  bc = data.frame() # make temp object to hold coefficients
  
  for (i in 2:nvar) bc[1,i-1] = cf[i] * sdev[i] / sdev[1] # creates frame of beta.coeff
  
  bc <- as.matrix(bc)               # make result into matrix
  rownames(bc) = 'Beta.Coef'      # set a row name
  
  cat('\nBeta Coefficients for:', deparse(substitute(model)), '\n\n') # a mesage
  
  print(bc, digits = digits)  # show result now
  cat('\n')                 # for tidiness!
  
  rownames(cf) = 'Coef'                    # make row names for coeff result
  
  result = list(bc, cf)                    # make container for all results
  names(result) = c('beta.coef', 'coef') # give names to components
  result$call = model$call             # add original call to result
  class(result) = 'bcoef'                # add a custom class
  invisible(result)                    # make result available for later
  
  # See print.bcoef and summary.bcoef for print() and summary() methods
  
} # end function code

# --------------------

## Summary method for 'bcoef' class object

summary.bcoef <- function(object, digits = 7, ...) { # start summary function code
  
  # object = a 'bcoef' object, result holding beta coefficients
  # digits = no. decimal places to display (default = 7)
  # Code by: Mark Gardener 2015 www.dataanalytics.org.uk
  
  cat('\nBeta coefficients and lm() model summary.\n\n')
  cat('Model call:\n')
  
  print(object$call) # the original lm() model call
  cat('\n')        # new line
  
  bc <- cbind(object$coef[1], object$beta) # make object of intercept and beta coef
  rownames(bc) <- 'Beta.Coef'            # set row name
  
  all.coef <- rbind(object$coef, bc)  # make summary using coef and beta coef
  all.coef[2,1] = NA                # set intercept for beta coef to NA
  all.coef <- as.matrix(all.coef) # make result a matrix
  
  print(all.coef, digits = digits)    # print the matrix of results
  cat('\n')                           # a tidy new line
  
  result = list(object$call, all.coef)    # make results object (a list)
  names(result) = c('call', 'all.coef') # make names
  
  invisible(result)                       # save result object for later
  
} # end summary function code

# ---------------------

## Print method for 'bcoef' class object

print.bcoef <- function(object, digits = 7, ...) { # start print function code
  
  # object = a 'bcoef' object, result holding beta coefficients
  # digits = no. decimal places to display, default = 7
  # Code by: Mark Gardener 2015 www.dataanalytics.org.uk
  
  cat('\nBeta coefficients:\n')              # a message
  print(object$beta.coef, digits = digits) # display the beta coefficients
  cat('\n')                              # for tidiness
  
} # end print function code

## END