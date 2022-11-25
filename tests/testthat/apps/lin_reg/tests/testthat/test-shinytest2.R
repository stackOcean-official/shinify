library(shinytest2)

test_that("{shinytest2} recording: lin_reg", {
  app <- AppDriver$new(name = "lin_reg", height = 685, width = 899)
  app$expect_values()
  app$set_inputs(num1 = 1)
  app$set_inputs(num1 = character(0))
  app$set_inputs(num1 = 190)
  app$set_inputs(num2 = character(0))
  app$set_inputs(num2 = 70)
})

