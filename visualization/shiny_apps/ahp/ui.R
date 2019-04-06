shinyUI(
  fluidPage(
    
    titlePanel("Analytical Hierarchy Process (AHP) Modeler"),
    sidebarLayout(      
      sidebarPanel(
        
        textInput("model_name",
                  label = "Enter the Model Name",
                  value = "Default Example Model"),
        #verbatimTextOutput("value"),
        
        h3("Define the AHP Model Size"),
        
        helpText("The number of criteria must equal the length of criteria names. Similarly, the number of alternatives considered must equal the length of alternative names."),
        
        numericInput("criteria_count", 
                     label = "How many criteria are being considered? (max = 5)", 
                     2, min = 2, max = 5),
        
        textInput("critera_text",
                  label = "Type the different criteria titles, seperated by commas.",
                  value = "Example A, Example B"),
        #verbatimTextOutput("value"),
        
        numericInput("options_count",
                     label = "How many alternatives are being considered? (max = 4)",
                     2, min = 2, max = 4),
        
        textInput("options_text",
                  label = "Type the different alternatives' names, seperated by commas.",
                  value = "Option A, Option B"),
        #verbatimTextOutput("value"),
        
        
        #####################################
        #
        # MAIN CRITERIA COMPARISONS
        #
        #####################################
        
        h3("Criteria Prioritization"),
        
        helpText("Below, compare the importance of each criteria to each other. A \'9\' means that the first criteria is extremely more important than the second criteria."),
        
        conditionalPanel(condition = "input.criteria_count == 2 || input.criteria_count == 3 || input.criteria_count == 4 || input.criteria_count == 5",
                         uiOutput("SelectCriteria1")),
        
        conditionalPanel(condition = "input.criteria_count == 3 || input.criteria_count == 4 || input.criteria_count == 5",
                         uiOutput("SelectCriteria2")),
        
        conditionalPanel(condition = "input.criteria_count == 4 || input.criteria_count == 5",
                         uiOutput("SelectCriteria4")),
        
        conditionalPanel(condition = "input.criteria_count == 5",
                         uiOutput("SelectCriteria7")),
        
        conditionalPanel(condition = "input.criteria_count == 3 || input.criteria_count == 4 || input.criteria_count == 5",
                         uiOutput("SelectCriteria3")),
        
        conditionalPanel(condition = "input.criteria_count == 4 || input.criteria_count == 5",
                         uiOutput("SelectCriteria5")),
        
        conditionalPanel(condition = "input.criteria_count == 5",
                         uiOutput("SelectCriteria8")),
        
        conditionalPanel(condition = "input.criteria_count == 4 || input.criteria_count == 5",
                         uiOutput("SelectCriteria6")),
        
        conditionalPanel(condition = "input.criteria_count == 5",
                         uiOutput("SelectCriteria9")),
        
        conditionalPanel(condition = "input.criteria_count == 5",
                         uiOutput("SelectCriteria10")),
        
        
        #####################################
        #
        # OPTION COMPARISONS
        #
        #####################################
        
        ############
        # for 2 criteria
        ############
  
        
        h3("Alternative Comparisons"),
        
        helpText("Below, compare the importance of each alternative to each other, in every category. A \'9\' means that the first alternative is extremely more important than the second alternative for the criteria being judged on."),
        
        conditionalPanel(condition = "input.options_count == 2 & input.criteria_count == 2 ||
                         input.options_count == 2 & input.criteria_count == 3 ||
                         input.options_count == 2 & input.criteria_count == 4 ||
                         input.options_count == 2 & input.criteria_count == 5 ||
                         input.options_count == 3 & input.criteria_count == 2 ||
                         input.options_count == 3 & input.criteria_count == 3 ||
                         input.options_count == 3 & input.criteria_count == 4 ||
                         input.options_count == 3 & input.criteria_count == 5 ||
                         input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment1")),
        
        conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 2 ||
                         input.options_count == 3 & input.criteria_count == 3 ||
                         input.options_count == 3 & input.criteria_count == 4 ||
                         input.options_count == 3 & input.criteria_count == 5 ||
                         input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment2")),
        
        conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment4")),
        
        conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 2 ||
                         input.options_count == 3 & input.criteria_count == 3 ||
                         input.options_count == 3 & input.criteria_count == 4 ||
                         input.options_count == 3 & input.criteria_count == 5 ||
                         input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment3")),
        
        conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment5")),
        
        conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment6")),
    
        conditionalPanel(condition = "input.options_count == 2 & input.criteria_count == 2 ||
                         input.options_count == 2 & input.criteria_count == 3 ||
                         input.options_count == 2 & input.criteria_count == 4 ||
                         input.options_count == 2 & input.criteria_count == 5 ||
                         input.options_count == 3 & input.criteria_count == 2 ||
                         input.options_count == 3 & input.criteria_count == 3 ||
                         input.options_count == 3 & input.criteria_count == 4 ||
                         input.options_count == 3 & input.criteria_count == 5 ||
                         input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment1.a")),
        
        conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 2 ||
                         input.options_count == 3 & input.criteria_count == 3 ||
                         input.options_count == 3 & input.criteria_count == 4 ||
                         input.options_count == 3 & input.criteria_count == 5 ||
                         input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment2.a")),
        
        conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment4.a")),
        
        conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 2 ||
                         input.options_count == 3 & input.criteria_count == 3 ||
                         input.options_count == 3 & input.criteria_count == 4 ||
                         input.options_count == 3 & input.criteria_count == 5 ||
                         input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment3.a")),
        
        conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment5.a")),
        
        conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 2 ||
                         input.options_count == 4 & input.criteria_count == 3 ||
                         input.options_count == 4 & input.criteria_count == 4 ||
                         input.options_count == 4 & input.criteria_count == 5",
                         uiOutput("OptionAssessment6.a")),
      
      
      ############
      # for 3 criteria
      ############
      
      conditionalPanel(condition = "input.options_count == 2 & input.criteria_count == 3 ||
                       input.options_count == 2 & input.criteria_count == 4 ||
                       input.options_count == 2 & input.criteria_count == 5 ||
                       input.options_count == 3 & input.criteria_count == 3 ||
                       input.options_count == 3 & input.criteria_count == 4 ||
                       input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 3 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment1.b")),
      
      conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 3 ||
                       input.options_count == 3 & input.criteria_count == 4 ||
                       input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 3 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment2.b")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 3 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment4.b")),
      
      conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 3 ||
                       input.options_count == 3 & input.criteria_count == 4 ||
                       input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 3 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment3.b")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 3 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment5.b")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 3 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment6.b")),
      
      ############
      # for 4 criteria
      ############
      
      conditionalPanel(condition = "input.options_count == 2 & input.criteria_count == 4 ||
                       input.options_count == 2 & input.criteria_count == 5 ||
                       input.options_count == 3 & input.criteria_count == 4 ||
                       input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment1.c")),
      
      conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 4 ||
                       input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment2.c")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment4.c")),
      
      conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 4 ||
                       input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment3.c")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment5.c")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 4 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment6.c")),
      
      ############
      # for 5 criteria
      ############
      
      conditionalPanel(condition = "input.options_count == 2 & input.criteria_count == 5 ||
                       input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment1.d")),
      
      conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment2.d")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment4.d")),
      
      conditionalPanel(condition = "input.options_count == 3 & input.criteria_count == 5 ||
                       input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment3.d")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment5.d")),
      
      conditionalPanel(condition = "input.options_count == 4 & input.criteria_count == 5",
                       uiOutput("OptionAssessment6.d"))
      
      
    ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Decision Outcome",
                   uiOutput("modelName"),
                   h2("AHP Decision Outcome"),
                   plotOutput("decision_outcome"),
                   textOutput("cr_mc1"),
                   textOutput("cr_alt1"),
                   textOutput("cr_alt2"),
                   textOutput("cr_alt3"),
                   textOutput("cr_alt4"),
                   textOutput("cr_alt5"),
                   h3("Criteria Prioritization Values"),
                   tableOutput("matrix1"),
                   h3("Alternative Prioritization for Each Criteria"),
                   tableOutput("alt_priorities"),
                   h3("Comparison Matrix of Criteria"),
                   tableOutput("mc_matrix"),
                   textOutput("cr_mc2"),
                   uiOutput("c1m_name"),
                   tableOutput("alt1"),
                   textOutput("cr2_alt1"),
                   uiOutput("c2m_name"),
                   tableOutput("alt2"),
                   textOutput("cr2_alt2"),
                   uiOutput("c3m_name"),
                   tableOutput("alt3"),
                   textOutput("cr2_alt3"),
                   uiOutput("c4m_name"),
                   tableOutput("alt4"),
                   textOutput("cr2_alt4"),
                   uiOutput("c5m_name"),
                   tableOutput("alt5"),
                   textOutput("cr2_alt5")),
          tabPanel("About AHP",
                   helpText("The Analytical Hierarchy Process (AHP) is a decision-making tool used to rank different alternatives based on both quantitative and qualitative criteria. This AHP web application was developed for two reasons: (1) relieve the decision maker of the complex customizations that are associated with changing the number of criteria and/or alternatives and (2) automate the necessary algorithms. Although AHP has the ability to assess alternatives at multiple levels of sub-criteria, this free web app only allows the user to assess alternatives at a single level of criteria choices."),
                   helpText("Additional information about AHP can be found at the link below."),
                   helpText(a("AHP on Wikipedia", href = "https://en.wikipedia.org/wiki/Analytic_hierarchy_process", target = "_blank")),
                   helpText("The scripts used to create this application can be found at the GitHub link below."),
                   helpText(a("GitHub", href = "https://github.com/bshelton141/ahp_shiny_app", target = "_blank"))))
      )
      
    )
  )
)
