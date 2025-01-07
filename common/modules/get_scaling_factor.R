# Define scaling factor based on value_scale
getScalingFactor <- function(scale) {
  switch(scale,
         "thousands" = 1e3,
         "millions" = 1e6,
         "billions" = 1e9,
         1  # default case (no scaling)
  )
}