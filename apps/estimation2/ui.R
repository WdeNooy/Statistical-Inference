library(shiny)
fig.width = 600
fig.height = 300

##This function sets the ticks of the standard error slider
##to be only possible values! 
##The output of the slider changes from the usual shiny behaviour 
##it outputs the values 0 to 29 for the positions of the slider. 
JScode <-
  "$(function() {
setTimeout(function(){
var vals = [.5];
var ntop = 30;
var nbot = 2;
for (i = nbot; i <= ntop; i++) {
var val = 0.5/Math.sqrt(i);
val = parseFloat(val.toFixed(3));
vals.push(val);
}
$('#seslider').data('ionRangeSlider').update({'values':vals})
}, 5)})"


shinyUI(fluidPage(
  tags$head(tags$script(HTML(JScode))),
  verticalLayout(
    fluidRow(align = "center",
             plotOutput("mainplot",
                        height = fig.height,
                        width = fig.width
                        )
             ),
    fluidRow(align = "center",
             inputPanel(
                        sliderInput("cfintslider",
                                    label = "Confidence Interval",
                                    value = 95,
                                    min = 50,
                                    max = 100,
                                    step = 1,
                                    post = "%",
                                    round = FALSE
                                    ),
                        sliderInput("nslider",
                                    label = "Sample size",
                                    value = 25,
                                    min = 1,
                                    max = 30,
                                    step = 1,
                                    round = FALSE
                                    ),
                        
                        sliderInput("seslider",
                                    label = "Standard error",
                                    value = 24,
                                    min = 0,
                                    max = 29,
                                    round = -3
                                    )
                       )
             )
    )
))
