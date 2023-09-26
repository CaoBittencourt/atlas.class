# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# [FUNCTIONS] ---------------------------
# - Classifier function -------------------------------------------------
fun_class_classifier <- function(
    dbl_var
    , dbl_scale_lb = 0
    , dbl_scale_ub = 1
    , int_levels = 5
    , chr_class_labels = NULL
){
  
  # Arguments validation
  stopifnot(
    "'dbl_var' must be numeric." = 
      is.numeric(dbl_var)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." = 
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." = 
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'int_levels' must be an integer." = 
      is.numeric(int_levels)
  )
  
  stopifnot(
    "'chr_class_labels' must be either NULL or a character vector with length equal to 'int_levels'." = 
      any(
        is.null(chr_class_labels),
        all(
          is.character(chr_class_labels),
          length(chr_class_labels) ==
            ceiling(int_levels[[1]])
        )
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  int_levels[[1]] -> int_levels
  ceiling(int_levels) -> int_levels
  
  # Classify variable
  findInterval(
    dbl_var
    , seq(
      dbl_scale_lb, 
      dbl_scale_ub, 
      length.out = 1 +
        int_levels
    )
    , all.inside = T
  ) -> int_class_id
  
  names(dbl_var) -> 
    names(int_class_id)
  
  if(!is.null(chr_class_labels)){
    
    factor(
      int_class_id
      , levels =
        1:int_levels
      , labels =
        chr_class_labels
      , ordered = T
    ) -> int_class_id
    
  }
  
  # Output
  return(int_class_id)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Classifier test -------------------------------------------------
# fun_class_classifier(
#   dbl_var = 
#     rnorm(100) |>
#     pmax(0) |>
#     pmin(1)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 1
#   , int_levels = 5
# )
# 
# fun_class_classifier(
#   dbl_var = 
#     rnorm(100) |>
#     pmax(0) |>
#     pmin(1)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 1
#   , int_levels = 5
#   , chr_class_labels = c(
#     'very low', 'low',
#     'medium',
#     'high', 'very high'
#   )
# )
