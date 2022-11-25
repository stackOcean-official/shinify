library(shinytest2)

test_that("{shinytest2} recording: jumpstart", {
  app <- AppDriver$new(name = "jumpstart", height = 932, width = 899)
  app$expect_values()
  app$set_inputs(num1 = character(0))
  app$set_inputs(num1 = 120)
  app$set_inputs(num2 = character(0))
  app$set_inputs(num2 = 73)
})



test_that("{shinytest2} recording: jumpstart-2", {
  app <- AppDriver$new(name = "jumpstart-2", height = 932, width = 899)
  app$expect_values()
  app$set_inputs(num1 = 190)
  app$set_inputs(num2 = 70)
})

