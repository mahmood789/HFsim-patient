# Heart Failure Simulator for Medical Education
# Based on ESC 2021 and NICE 2018 Heart Failure Guidelines

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(shinycssloaders)
library(dplyr)

# --- Helper function for mortality risk calculation (mimicking ML model) ---
# This function is placed outside the server function so it's defined once.
calculate_mortality_risk <- function(ef, age, gender, hypertension, diabetes, ckd, copd, afib,
                                     ace_arb_dose_pct, bb_dose_pct, mra_dose_pct, sglt2_dose_pct,
                                     current_co_l_min, current_bnp_pg_ml) {
  # Base risk score based on patient characteristics and severity
  base_risk_score <- 0

  # EF contribution (lower EF, higher risk)
  if (ef < 30) base_risk_score <- base_risk_score + 15
  else if (ef < 40) base_risk_score <- base_risk_score + 10
  else if (ef < 50) base_risk_score <- base_risk_score + 5

  # Age contribution (older age, higher risk)
  base_risk_score <- base_risk_score + (age - 50) * 0.4 # Each year above 50 adds risk

  # Gender contribution (males generally have higher risk for HFrEF)
  if (gender == "M") base_risk_score <- base_risk_score + 3

  # Comorbidity contributions (stronger impact for more severe comorbidities)
  if (hypertension) base_risk_score <- base_risk_score + 2
  if (diabetes) base_risk_score <- base_risk_score + 5
  if (ckd) base_risk_score <- base_risk_score + 10
  if (copd) base_risk_score <- base_risk_score + 4
  if (afib) base_risk_score <- base_risk_score + 5

  # Hemodynamic/Biomarker contribution (reflecting current state)
  if (current_co_l_min < 3.5) base_risk_score <- base_risk_score + 7
  if (current_bnp_pg_ml > 1000) base_risk_score <- base_risk_score + 8
  else if (current_bnp_pg_ml > 400) base_risk_score <- base_risk_score + 4

  # Convert score to an initial mortality probability using a sigmoid function
  initial_mortality_prob <- 1 / (1 + exp(-(base_risk_score - 25) / 10)) # Adjust curve as needed

  # Treatment effects (reducing risk proportionally based on dose)
  reduction_factor <- 1
  reduction_factor <- reduction_factor * (1 - ace_arb_dose_pct * 0.20) # Up to 20% relative risk reduction for ACE/ARB
  reduction_factor <- reduction_factor * (1 - bb_dose_pct * 0.25) # Up to 25% relative risk reduction for Beta Blockers
  reduction_factor <- reduction_factor * (1 - mra_dose_pct * 0.15)
  reduction_factor <- reduction_factor * (1 - sglt2_dose_pct * 0.20) # Significant new reduction

  final_mortality_prob <- initial_mortality_prob * reduction_factor

  # Clamp the probabilities for realism
  final_mortality_prob <- max(0.01, min(final_mortality_prob, 0.70)) # Between 1% and 70%

  return(final_mortality_prob)
}
# --- End of Helper Function ---

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Heart Failure Simulator"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Patient Parameters", tabName = "parameters", icon = icon("user")),
      menuItem("Hemodynamics", tabName = "hemodynamics", icon = icon("heartbeat")),
      menuItem("Treatment", tabName = "treatment", icon = icon("pills")),
      menuItem("Educational Info", tabName = "education", icon = icon("book"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .info-box {
          border-radius: 5px;
        }
      "))
    ),

    tabItems(
      # Patient Parameters Tab
      tabItem(tabName = "parameters",
        fluidRow(
          box(
            title = "Patient Demographics", status = "primary", solidHeader = TRUE,
            width = 6,
            sliderInput("age", "Age (years):", min = 18, max = 90, value = 65),
            selectInput("gender", "Gender:", choices = c("Male" = "M", "Female" = "F")),
            sliderInput("weight", "Weight (kg):", min = 40, max = 150, value = 70),
            sliderInput("height", "Height (cm):", min = 140, max = 200, value = 170)
          ),

          box(
            title = "Heart Failure Parameters", status = "warning", solidHeader = TRUE,
            width = 6,
            sliderInput("ejection_fraction", "Ejection Fraction (%):",
                        min = 10, max = 70, value = 35),
            sliderInput("preload", "Preload (mmHg):", min = 5, max = 25, value = 15),
            sliderInput("afterload", "Afterload (mmHg):", min = 60, max = 180, value = 100),
            sliderInput("contractility", "Contractility (%):", min = 20, max = 100, value = 60)
          )
        ),

        fluidRow(
          box(
            title = "Comorbidities", status = "info", solidHeader = TRUE,
            width = 12,
            checkboxInput("hypertension", "Hypertension", value = FALSE),
            checkboxInput("diabetes", "Diabetes", value = FALSE),
            checkboxInput("ckd", "Chronic Kidney Disease", value = FALSE),
            checkboxInput("copd", "COPD", value = FALSE),
            checkboxInput("afib", "Atrial Fibrillation", value = FALSE)
          )
        )
      ),

      # Hemodynamics Tab
      tabItem(tabName = "hemodynamics",
        fluidRow(
          valueBoxOutput("cardiac_output"),
          valueBoxOutput("stroke_volume"),
          valueBoxOutput("heart_rate")
        ),

        fluidRow(
          valueBoxOutput("blood_pressure"),
          valueBoxOutput("nyha_class"),
          valueBoxOutput("bnp_level")
        ),

        fluidRow(
          box(
            title = "Pressure-Volume Loop", status = "primary", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("pv_loop"))
          ),

          box(
            title = "Frank-Starling Curve", status = "success", solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("frank_starling"))
          )
        ),

        fluidRow(
          box(
            title = "Hemodynamic Trends", status = "info", solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("trends"))
          )
        )
      ),

      # Treatment Tab
      tabItem(tabName = "treatment",
        fluidRow(
          box(
            title = "Evidence-Based Medications (ESC/NICE Guidelines)",
            status = "warning", solidHeader = TRUE, width = 6,

            h4("ACE Inhibitors (1st Line)"),
            selectInput("ace_type", "Medication:",
                        choices = c("None" = "none", "Ramipril" = "ramipril",
                                    "Enalapril" = "enalapril", "Lisinopril" = "lisinopril")),
            conditionalPanel(
              condition = "input.ace_type != 'none'",
              sliderInput("ace_dose", "Dose:", min = 0, max = 100, value = 0,
                          post = "%", step = 25),
              textOutput("ace_dose_text")
            ),

            h4("ARBs (if ACE-I intolerant)"),
            selectInput("arb_type", "Medication:",
                        choices = c("None" = "none", "Candesartan" = "candesartan",
                                    "Valsartan" = "valsartan", "Losartan" = "losartan")),
            conditionalPanel(
              condition = "input.arb_type != 'none'",
              sliderInput("arb_dose", "Dose:", min = 0, max = 100, value = 0,
                          post = "%", step = 25),
              textOutput("arb_dose_text")
            ),

            h4("Beta Blockers (1st Line)"),
            selectInput("bb_type", "Medication:",
                        choices = c("None" = "none", "Bisoprolol" = "bisoprolol",
                                    "Carvedilol" = "carvedilol", "Metoprolol" = "metoprolol")),
            conditionalPanel(
              condition = "input.bb_type != 'none'",
              sliderInput("bb_dose", "Dose:", min = 0, max = 100, value = 0,
                          post = "%", step = 25),
              textOutput("bb_dose_text")
            ),

            h4("MRA (Mineralocorticoid Receptor Antagonists)"),
            selectInput("mra_type", "Medication:",
                        choices = c("None" = "none", "Spironolactone" = "spironolactone",
                                    "Eplerenone" = "eplerenone")),
            conditionalPanel(
              condition = "input.mra_type != 'none'",
              sliderInput("mra_dose", "Dose:", min = 0, max = 100, value = 0,
                          post = "%", step = 25),
              textOutput("mra_dose_text")
            ),

            h4("Diuretics (Symptom Relief)"),
            selectInput("diuretic_type", "Medication:",
                        choices = c("None" = "none", "Furosemide" = "furosemide",
                                    "Bumetanide" = "bumetanide", "Bendroflumethiazide" = "bendro")),
            conditionalPanel(
              condition = "input.diuretic_type != 'none'",
              sliderInput("diuretic_dose", "Dose:", min = 0, max = 100, value = 0,
                          post = "%", step = 25),
              textOutput("diuretic_dose_text")
            )
          ),

          box(
            title = "Advanced Therapies", status = "success", solidHeader = TRUE,
            width = 6,

            h4("SGLT2 Inhibitors (New Evidence)"),
            selectInput("sglt2_type", "Medication:",
                        choices = c("None" = "none", "Dapagliflozin" = "dapagliflozin",
                                    "Empagliflozin" = "empagliflozin")),
            conditionalPanel(
              condition = "input.sglt2_type != 'none'",
              sliderInput("sglt2_dose", "Dose:", min = 0, max = 100, value = 0,
                          post = "%", step = 50),
              textOutput("sglt2_dose_text")
            ),

            h4("Device Therapy (ESC Class I/IIa)"),
            checkboxInput("icd", "ICD - Primary Prevention (EF ≤35% on OMT)", value = FALSE),
            checkboxInput("crt", "CRT-D - QRS ≥130ms, LBBB, EF ≤35%", value = FALSE),
            checkboxInput("crt_p", "CRT-P - Alternative to CRT-D", value = FALSE),

            h4("Advanced Heart Failure"),
            checkboxInput("lvad", "LVAD (Bridge to Transplant/Destination)", value = FALSE),
            checkboxInput("transplant", "Heart Transplant Candidate", value = FALSE),

            h4("Guideline Adherence Score"),
            verbatimTextOutput("guideline_score")
          )
        ),

        fluidRow(
          box(
            title = "Treatment Response", status = "primary", solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("treatment_response"))
          )
        ),

        fluidRow(
          box(
            title = "Clinical Outcomes", status = "info", solidHeader = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("outcomes_table"))
          )
        )
      ),

      # Educational Info Tab
      tabItem(tabName = "education",
        fluidRow(
          box(
            title = "Heart Failure Classification", status = "primary", solidHeader = TRUE,
            width = 6,
            h4("NYHA Functional Classification:"),
            tags$ul(
              tags$li("Class I: No symptoms with ordinary activity"),
              tags$li("Class II: Symptoms with ordinary activity"),
              tags$li("Class III: Symptoms with less than ordinary activity"),
              tags$li("Class IV: Symptoms at rest")
            ),

            h4("Ejection Fraction Categories:"),
            tags$ul(
              tags$li("HFrEF: EF ≤ 40% (reduced)"),
              tags$li("HFmrEF: EF 41-49% (mildly reduced)"),
              tags$li("HFpEF: EF ≥ 50% (preserved)")
            )
          ),

          box(
            title = "Key Learning Points", status = "success", solidHeader = TRUE,
            width = 6,
            h4("Pathophysiology:"),
            tags$ul(
              tags$li("Reduced contractility leads to decreased stroke volume"),
              tags$li("Compensatory mechanisms: ↑HR, ↑preload, neurohormonal activation"),
              tags$li("Frank-Starling mechanism becomes less effective"),
              tags$li("Increased afterload worsens cardiac output")
            ),

            h4("Treatment Goals:"),
            tags$ul(
              tags$li("Improve symptoms and quality of life"),
              tags$li("Reduce hospitalizations"),
              tags$li("Improve survival"),
              tags$li("Slow disease progression")
            )
          )
        ),

        fluidRow(
          box(
            title = "Simulation Instructions", status = "info", solidHeader = TRUE,
            width = 12,
            h4("How to Use This Simulator:"),
            tags$ol(
              tags$li("Start by adjusting patient parameters (age, weight, comorbidities)"),
              tags$li("Modify heart failure parameters to see effects on hemodynamics"),
              tags$li("Observe changes in cardiac output, stroke volume, and pressure-volume loops"),
              tags$li("Try different treatment combinations and observe responses"),
              tags$li("Note how medications affect different hemodynamic parameters"),
              tags$li("Experiment with device therapies for advanced heart failure")
            )
          )
        ),
        hr(),
        fluidRow(
          box(
            title = "How to Improve Data Accuracy", status = "primary", solidHeader = TRUE,
            width = 12,
            h4("To make this simulator even more realistic and clinically relevant, here are key areas for improvement in data accuracy:"),
            tags$ul(
              tags$li(tags$strong("Incorporate Real-World Heart Failure Registries:")),
              tags$ul(
                tags$li("Utilize large datasets from sources like the American Heart Association (AHA) registries, the European Society of Cardiology (ESC) registries, or national heart failure databases."),
                tags$li("Benefits: This would move the simulator beyond simplified physiological models to reflect the complex variability seen in real-world patients.")
              ),
              tags$li(tags$strong("Implement Machine Learning Models:")),
              tags$ul(
                tags$li("Train predictive models using historical heart failure patient outcomes. Machine learning can identify intricate relationships between patient parameters, treatment choices, and clinical endpoints (e.g., mortality, rehospitalization, quality of life) that might be difficult to capture with rule-based systems.")
              ),
              tags$li(tags$strong("Add More Granular Medication Effects:")),
              tags$ul(
                tags$li("Integrate pharmacokinetics and pharmacodynamics: Instead of simple percentage-based dose effects, introduce more sophisticated models that simulate drug absorption, distribution, metabolism, and excretion."),
                tags$li("Consider individual variability: Account for factors like age, renal function, and liver function that influence how a patient responds to medication.")
              ),
              tags$li(tags$strong("Include Detailed Comorbidity Interactions:")),
              tags$ul(
                tags$li("Model the impact of comorbidities more explicitly: For instance, chronic kidney disease (CKD) not only affects drug clearance but can also worsen fluid overload and impact the effectiveness of diuretics."),
                tags$li("Reflect guideline recommendations for comorbidities: Ensure that the simulator's treatment suggestions align with guidelines for managing heart failure in the presence of specific co-existing conditions.")
              )
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive calculations for hemodynamics
  hemodynamics <- reactive({
    # Calculate BSA (Body Surface Area)
    # Using Mosteller formula: BSA = sqrt(height_cm * weight_kg / 3600)
    bsa <- sqrt((input$height * input$weight) / 3600)

    # Base calculations (consider age more granularly)
    # Younger patients often have higher maximum HR reserves, but resting HR can be stable
    base_hr <- 70 - (input$age - 60) * 0.2 # Slight age-related adjustment
    base_hr <- max(base_hr, 50) # Minimum HR

    # Adjust for heart failure severity (non-linear impact)
    # EF impact is crucial for stroke volume
    # Contractility is also a direct determinant
    ef_ratio <- input$ejection_fraction / 100 # Convert EF to a ratio (0-1)
    contractility_ratio <- input$contractility / 100

    # Calculate stroke volume (more complex interaction)
    # Base SV, then modify by EF, contractility, preload, and afterload
    # Using a conceptual model where higher EF/contractility means better SV,
    # higher preload means more SV (up to a point), higher afterload reduces SV.
    # The coefficients are illustrative, mimicking real physiological relationships.
    stroke_volume <- (80 * ef_ratio * contractility_ratio) + # Base SV scaled by EF and contractility
                     (input$preload * 1.5) -                   # Preload contribution (diminishing returns later)
                     (input$afterload * 0.15)                  # Afterload resistance

    # Add a floor to stroke volume to prevent negative/unrealistically low values
    stroke_volume <- max(stroke_volume, 20) # Minimum realistic SV (mL)

    # Heart rate compensation (inverse relationship with CO/SV, within limits)
    # If CO is low, HR tries to compensate, but limited by physiology and beta-blockers
    target_co_for_hr <- 5.0 # L/min
    hr_compensatory_factor <- 1
    if (stroke_volume * base_hr / 1000 < target_co_for_hr) {
      hr_compensatory_factor <- 1 + (target_co_for_hr - (stroke_volume * base_hr / 1000)) * 0.1
    }
    heart_rate <- base_hr * hr_compensatory_factor
    heart_rate <- min(heart_rate, 110) # Cap HR

    # Initial Cardiac Output
    cardiac_output <- (stroke_volume * heart_rate) / 1000 # L/min

    # Blood pressure - more nuanced calculation
    # MAP = CO * SVR (Mean Arterial Pressure = Cardiac Output * Systemic Vascular Resistance)
    # SVR is heavily influenced by afterload and body size
    # Let's approximate SVR from afterload (which represents systemic vascular resistance in our model)
    svr <- input$afterload / 80 # Conceptual SVR from afterload input
    mean_arterial_pressure <- cardiac_output * svr * 10 # Convert to mmHg for scaling

    # Systolic and Diastolic BP relation to MAP (approximate)
    # Pulse pressure is influenced by stroke volume and arterial stiffness
    pulse_pressure <- stroke_volume * 0.3 + input$age * 0.5 # Age adds stiffness
    systolic_bp <- mean_arterial_pressure + pulse_pressure / 2
    diastolic_bp <- mean_arterial_pressure - pulse_pressure / 2

    # --- Medication Effects (More Granular and Realistic) ---
    # Coefficients are illustrative, mimicking clinical trial/registry data effects.
    # Percentages are scaled by 0.01 to convert from 0-100 to 0-1.

    # ACE Inhibitors/ARBs: Primarily reduce afterload and preload, some direct CO benefit
    ace_arb_potency <- 0
    if (input$ace_type != "none") ace_arb_potency <- input$ace_dose / 100
    else if (input$arb_type != "none") ace_arb_potency <- input$arb_dose / 100

    if (ace_arb_potency > 0) {
      systolic_bp <- systolic_bp * (1 - ace_arb_potency * 0.15) # Up to 15% SBP reduction
      diastolic_bp <- diastolic_bp * (1 - ace_arb_potency * 0.10)
      cardiac_output <- cardiac_output * (1 + ace_arb_potency * 0.08) # Modest CO improvement
      stroke_volume <- stroke_volume * (1 + ace_arb_potency * 0.05)
    }

    # Beta Blockers: Reduce HR, improve contractility over time (reverse remodeling)
    bb_potency <- 0
    if (input$bb_type != "none") bb_potency <- input$bb_dose / 100

    if (bb_potency > 0) {
      heart_rate <- heart_rate * (1 - bb_potency * 0.25) # Significant HR reduction (up to 25%)
      heart_rate <- max(heart_rate, 55) # Prevent bradycardia
      # Long-term beta-blocker effect on EF/Contractility - simulate here
      # This is where an ML model would learn the *long-term* impact
      contractility_ratio_bb_adj <- contractility_ratio * (1 + bb_potency * 0.10) # 10% improvement
      stroke_volume <- (80 * ef_ratio * contractility_ratio_bb_adj) +
                       (input$preload * 1.5) -
                       (input$afterload * 0.15)
      stroke_volume <- max(stroke_volume, 20)
      cardiac_output <- (stroke_volume * heart_rate) / 1000 # Recalculate CO with new HR and SV
    }

    # Diuretics: Reduce preload, some SBP reduction
    diuretic_potency <- 0
    if (input$diuretic_type != "none") diuretic_potency <- input$diuretic_dose / 100

    if (diuretic_potency > 0) {
      systolic_bp <- systolic_bp * (1 - diuretic_potency * 0.08) # Moderate SBP reduction
      diastolic_bp <- diastolic_bp * (1 - diuretic_potency * 0.05)
      # Simulating preload reduction by direct effect on SV (volume status)
      stroke_volume <- stroke_volume * (1 - diuretic_potency * 0.05) # Reduce SV due to volume loss
      stroke_volume <- max(stroke_volume, 20)
      cardiac_output <- (stroke_volume * heart_rate) / 1000 # Recalculate CO
    }

    # MRA: Improve remodeling, modest impact on BP/CO
    mra_potency <- 0
    if (input$mra_type != "none") mra_potency <- input$mra_dose / 100

    if (mra_potency > 0) {
      cardiac_output <- cardiac_output * (1 + mra_potency * 0.03) # Modest CO improvement
      systolic_bp <- systolic_bp * (1 - mra_potency * 0.05) # Small BP effect
    }

    # SGLT2 Inhibitors: Complex benefits, improve CO, reduce BP, protect kidneys
    sglt2_potency <- 0
    if (input$sglt2_type != "none") sglt2_potency <- input$sglt2_dose / 100

    if (sglt2_potency > 0) {
      cardiac_output <- cardiac_output * (1 + sglt2_potency * 0.07) # Significant CO improvement
      systolic_bp <- systolic_bp * (1 - sglt2_potency * 0.08) # Modest BP reduction
      diastolic_bp <- diastolic_bp * (1 - sglt2_potency * 0.05)
    }

    # Device Therapy Impact (significant, often discrete jumps)
    if (input$crt && input$ejection_fraction <= 35) { # CRT is for LVEF < 35% with wide QRS
      cardiac_output <- cardiac_output * 1.15 # 15% increase for CRT responders
      stroke_volume <- stroke_volume * 1.10
      heart_rate <- heart_rate * 0.95 # Can optimize HR
    }

    # --- Adjust for Comorbidities (now interacting more with hemodynamics) ---
    # These mimic how comorbidities directly impact function
    if (input$hypertension) {
      systolic_bp <- systolic_bp * 1.05 # Higher baseline BP
      diastolic_bp <- diastolic_bp * 1.05
      cardiac_output <- cardiac_output * 0.95 # Slightly reduced CO efficiency
    }
    if (input$diabetes) {
      cardiac_output <- cardiac_output * 0.90 # Diabetes can impair cardiac function
      # BNP adjustment for diabetes is handled later, within the BNP calculation
    }
    if (input$ckd) {
      # CKD worsens volume status, higher preload, reduced drug clearance
      # For simplicity, we'll directly impact some values
      stroke_volume <- stroke_volume * 1.05 # Tendency for higher preload/volume
      # BNP adjustment for CKD is handled later, within the BNP calculation
    }
    if (input$copd) {
      cardiac_output <- cardiac_output * 0.95 # COPD can burden the heart
      heart_rate <- heart_rate * 1.03 # Often higher resting HR
    }
    if (input$afib) {
      # Afib primarily affects stroke volume variability and optimal filling
      stroke_volume <- stroke_volume * 0.90 # Less efficient ventricular filling
      heart_rate <- heart_rate * 1.10 # Tendency for higher, irregular HR
    }

    # Final clamping for realism
    cardiac_output <- max(2, min(cardiac_output, 8)) # L/min
    cardiac_index <- cardiac_output / bsa
    systolic_bp <- max(80, min(systolic_bp, 180)) # mmHg
    diastolic_bp <- max(40, min(diastolic_bp, 110)) # mmHg
    heart_rate <- max(45, min(heart_rate, 140)) # bpm

    # BNP levels (inversely related to EF, affected by volume, CKD, AFib)
    # Let's make BNP more responsive to multiple factors
    bnp <- 100 + (2000 * (1 - ef_ratio)^2) # Non-linear increase with decreasing EF
    bnp <- bnp + (input$preload * 10) # Preload adds to BNP
    if (input$ckd) bnp <- bnp * 1.8 # CKD elevates BNP significantly
    if (input$afib) bnp <- bnp * 1.3 # AFib can slightly elevate BNP
    if (input$diabetes) bnp <- bnp * 1.1 # Diabetes can slightly elevate BNP

    bnp <- max(50, min(bnp, 10000)) # Clamp BNP

    # NYHA Class based on EF and symptoms (now more dependent on CO/functional status)
    # NYHA is clinical, but we approximate it from CO, EF, and BNP
    nyha_score <- 0
    if (input$ejection_fraction < 40) nyha_score <- nyha_score + 1
    if (cardiac_output < 4) nyha_score <- nyha_score + 1.5
    if (systolic_bp < 90) nyha_score <- nyha_score + 1
    if (heart_rate > 100) nyha_score <- nyha_score + 0.5
    if (bnp > 800) nyha_score <- nyha_score + 1.5

    nyha_class <- if(nyha_score < 1.5) {
      "I"
    } else if(nyha_score < 3) {
      "II"
    } else if(nyha_score < 4.5) {
      "III"
    } else {
      "IV"
    }

    list(
      stroke_volume = stroke_volume,
      heart_rate = heart_rate,
      cardiac_output = cardiac_output,
      cardiac_index = cardiac_index,
      systolic_bp = systolic_bp,
      diastolic_bp = diastolic_bp,
      nyha_class = nyha_class,
      bnp = bnp,
      bsa = bsa
    )
  })

  # Value boxes
  output$cardiac_output <- renderValueBox({
    h <- hemodynamics()
    valueBox(
      value = paste(round(h$cardiac_output, 1), "L/min"),
      subtitle = "Cardiac Output",
      icon = icon("heartbeat"),
      color = if(h$cardiac_output < 4) "red" else if(h$cardiac_output < 5) "yellow" else "green"
    )
  })

  output$stroke_volume <- renderValueBox({
    h <- hemodynamics()
    valueBox(
      value = paste(round(h$stroke_volume, 0), "mL"),
      subtitle = "Stroke Volume",
      icon = icon("tint"),
      color = if(h$stroke_volume < 50) "red" else if(h$stroke_volume < 70) "yellow" else "green"
    )
  })

  output$heart_rate <- renderValueBox({
    h <- hemodynamics()
    valueBox(
      value = paste(round(h$heart_rate, 0), "bpm"),
      subtitle = "Heart Rate",
      icon = icon("pulse"),
      color = "blue"
    )
  })

  output$blood_pressure <- renderValueBox({
    h <- hemodynamics()
    valueBox(
      value = paste(round(h$systolic_bp, 0), "/", round(h$diastolic_bp, 0)),
      subtitle = "Blood Pressure (mmHg)",
      icon = icon("thermometer"),
      color = "purple"
    )
  })

  output$nyha_class <- renderValueBox({
    h <- hemodynamics()
    valueBox(
      value = h$nyha_class,
      subtitle = "NYHA Class",
      icon = icon("user"),
      color = if(h$nyha_class == "I") {
        "green"
      } else if(h$nyha_class == "II") {
        "yellow"
      } else if(h$nyha_class == "III") {
        "orange"
      } else {
        "red"
      }
    )
  })

  output$bnp_level <- renderValueBox({
    h <- hemodynamics()
    valueBox(
      value = paste(round(h$bnp, 0), "pg/mL"),
      subtitle = "BNP Level",
      icon = icon("flask"),
      color = if(h$bnp < 400) "green" else if(h$bnp < 1000) "yellow" else "red"
    )
  })

  # Medication dose text outputs
  output$ace_dose_text <- renderText({
    if(input$ace_type == "ramipril") {
      dose_mg <- round(10 * input$ace_dose / 100, 1)
      paste("Current dose:", dose_mg, "mg BD (Target: 10mg BD)")
    } else if(input$ace_type == "enalapril") {
      dose_mg <- round(20 * input$ace_dose / 100, 1)
      paste("Current dose:", dose_mg, "mg BD (Target: 20mg BD)")
    } else if(input$ace_type == "lisinopril") {
      dose_mg <- round(35 * input$ace_dose / 100, 1)
      paste("Current dose:", dose_mg, "mg OD (Target: 35mg OD)")
    }
  })

  output$arb_dose_text <- renderText({
    if(input$arb_type == "candesartan") {
      dose_mg <- round(32 * input$arb_dose / 100, 0)
      paste("Current dose:", dose_mg, "mg OD (Target: 32mg OD)")
    } else if(input$arb_type == "valsartan") {
      dose_mg <- round(320 * input$arb_dose / 100, 0) # 160mg BD means 320mg daily equivalent
      paste("Current dose:", dose_mg, "mg daily (Target: 320mg daily)")
    } else if(input$arb_type == "losartan") {
      dose_mg <- round(150 * input$arb_dose / 100, 0)
      paste("Current dose:", dose_mg, "mg OD (Target: 150mg OD)")
    }
  })

  output$bb_dose_text <- renderText({
    if(input$bb_type == "bisoprolol") {
      dose_mg <- round(10 * input$bb_dose / 100, 1)
      paste("Current dose:", dose_mg, "mg OD (Target: 10mg OD)")
    } else if(input$bb_type == "carvedilol") {
      dose_mg <- round(50 * input$bb_dose / 100, 1) # 25mg BD means 50mg daily equivalent
      paste("Current dose:", dose_mg, "mg daily (Target: 50mg daily)")
    } else if(input$bb_type == "metoprolol") {
      dose_mg <- round(200 * input$bb_dose / 100, 0)
      paste("Current dose:", dose_mg, "mg OD (Target: 200mg OD)")
    }
  })

  output$mra_dose_text <- renderText({
    if(input$mra_type == "spironolactone") {
      dose_mg <- round(50 * input$mra_dose / 100, 0)
      paste("Current dose:", dose_mg, "mg OD (Target: 50mg OD)")
    } else if(input$mra_type == "eplerenone") {
      dose_mg <- round(50 * input$mra_dose / 100, 0)
      paste("Current dose:", dose_mg, "mg OD (Target: 50mg OD)")
    }
  })

  output$diuretic_dose_text <- renderText({
    if(input$diuretic_type == "furosemide") {
      dose_mg <- round(80 * input$diuretic_dose / 100, 0)
      paste("Current dose:", dose_mg, "mg OD (Titrate to symptoms)")
    } else if(input$diuretic_type == "bumetanide") {
      dose_mg <- round(5 * input$diuretic_dose / 100, 1)
      paste("Current dose:", dose_mg, "mg OD (Titrate to symptoms)")
    } else if(input$diuretic_type == "bendro") {
      dose_mg <- round(5 * input$diuretic_dose / 100, 1)
      paste("Current dose:", dose_mg, "mg OD (Titrate to symptoms)")
    }
  })

  output$sglt2_dose_text <- renderText({
    if(input$sglt2_type == "dapagliflozin") {
      paste("Current dose: 10mg OD (Fixed dose)")
    } else if(input$sglt2_type == "empagliflozin") {
      paste("Current dose: 10mg OD (Fixed dose)")
    }
  })

  # Guideline adherence score
  output$guideline_score <- renderText({
    score <- 0
    max_score <- 0

    # ACE-I/ARB (25 points)
    max_score <- max_score + 25
    if(input$ace_type != "none" || input$arb_type != "none") {
      ace_arb_dose <- max(input$ace_dose, input$arb_dose)
      score <- score + (25 * ace_arb_dose / 100)
    }

    # Beta blocker (25 points)
    max_score <- max_score + 25
    if(input$bb_type != "none") {
      score <- score + (25 * input$bb_dose / 100)
    }

    # MRA (20 points)
    max_score <- max_score + 20
    if(input$mra_type != "none") {
      score <- score + (20 * input$mra_dose / 100)
    }

    # SGLT2 inhibitor (15 points)
    max_score <- max_score + 15
    if(input$sglt2_type != "none") {
      score <- score + (15 * input$sglt2_dose / 100)
    }

    # Device therapy (15 points, if indicated for EF <=35%)
    max_score <- max_score + 15 # Max points for devices
    if(input$ejection_fraction <= 35) {
      if(input$icd || input$crt || input$crt_p) { # Any indicated device
        score <- score + 15 # Full points if indicated and chosen
      }
    }


    percentage <- round((score / max_score) * 100, 0)

    paste("Guideline Adherence:", percentage, "%\n",
          "Score:", round(score, 0), "/", max_score, "\n",
          if(percentage >= 80) "Excellent - Optimal guideline-directed therapy" else
          if(percentage >= 60) "Good - Consider optimization" else
          if(percentage >= 40) "Moderate - Significant room for improvement" else
          "Poor - Major gaps in evidence-based therapy")
  })

  # Pressure-Volume Loop
  output$pv_loop <- renderPlotly({
    # Create PV loop data
    # Volumes now change more dynamically based on current stroke volume and assumed end-systolic volume
    h <- hemodynamics()
    esv <- 50 # End-systolic volume baseline
    edv <- esv + h$stroke_volume # End-diastolic volume

    # Generate volumes for the loop (e.g., from ESV to EDV and back)
    volumes_systole <- seq(esv, edv, length.out = 50)
    volumes_diastole <- seq(edv, esv, length.out = 50)
    volumes_loop <- c(volumes_systole, volumes_diastole)

    # Pressures for the loop (idealized shape)
    pressures_systole <- seq(h$diastolic_bp, h$systolic_bp, length.out = 50) # Isovolumic contraction & ejection
    pressures_diastole <- seq(h$systolic_bp, h$diastolic_bp, length.out = 50) # Isovolumic relaxation & filling
    pressures_loop <- c(pressures_systole, pressures_diastole)

    # Baseline for a "normal" heart
    normal_ef <- 60 # Assume normal EF for comparison
    normal_sv <- 70 # Normal SV
    normal_esv <- 50
    normal_edv <- normal_esv + normal_sv
    normal_hr <- 70

    normal_volumes_systole <- seq(normal_esv, normal_edv, length.out = 50)
    normal_volumes_diastole <- seq(normal_edv, normal_esv, length.out = 50)
    normal_volumes_loop <- c(normal_volumes_systole, normal_volumes_diastole)

    normal_systolic_bp <- 120
    normal_diastolic_bp <- 80
    normal_pressures_systole <- seq(normal_diastolic_bp, normal_systolic_bp, length.out = 50)
    normal_pressures_diastole <- seq(normal_systolic_bp, normal_diastolic_bp, length.out = 50)
    normal_pressures_loop <- c(normal_pressures_systole, normal_pressures_diastole)


    p <- plot_ly() %>%
      add_trace(x = normal_volumes_loop, y = normal_pressures_loop, type = 'scatter', mode = 'lines',
                name = 'Normal Heart', line = list(color = 'green', width = 2)) %>%
      add_trace(x = volumes_loop, y = pressures_loop, type = 'scatter', mode = 'lines',
                name = 'Current Patient', line = list(color = 'red', width = 2)) %>%
      layout(
        title = "Pressure-Volume Loop",
        xaxis = list(title = "Volume (mL)", range = c(min(normal_esv, esv)-10, max(normal_edv, edv)+10)),
        yaxis = list(title = "Pressure (mmHg)", range = c(min(normal_diastolic_bp, h$diastolic_bp)-10, max(normal_systolic_bp, h$systolic_bp)+10)),
        showlegend = TRUE
      )

    p
  })

  # Frank-Starling Curve
  output$frank_starling <- renderPlotly({
    preloads <- seq(0, 25, 0.5)

    # Normal Frank-Starling curve
    normal_co <- 2 + (preloads * 0.3) - (preloads^2 * 0.005)

    # Heart failure curve (shifted down and right, influenced by contractility and EF)
    # The 'hf_factor' now accounts for contractility and EF
    hf_contractility_factor <- input$contractility / 100
    hf_ef_factor <- input$ejection_fraction / 60 # Relative to a 'normal' EF of 60%
    overall_hf_impact <- (hf_contractility_factor + hf_ef_factor) / 2 # Average impact

    # The HF curve is scaled down by overall_hf_impact, and shifted right for higher preload dependence
    hf_co <- normal_co * overall_hf_impact
    hf_co[hf_co < 0.5] <- 0.5 # Minimum CO

    # Current patient point
    h <- hemodynamics()
    current_preload <- input$preload
    current_co <- h$cardiac_output

    p <- plot_ly() %>%
      add_trace(x = preloads, y = normal_co, type = 'scatter', mode = 'lines',
                name = 'Normal', line = list(color = 'green', width = 2)) %>%
      add_trace(x = preloads, y = hf_co, type = 'scatter', mode = 'lines',
                name = 'Heart Failure', line = list(color = 'red', width = 2)) %>%
      add_trace(x = current_preload, y = current_co, type = 'scatter', mode = 'markers',
                name = 'Current Patient', marker = list(color = 'blue', size = 10)) %>%
      layout(
        title = "Frank-Starling Curve",
        xaxis = list(title = "Preload (mmHg)"),
        yaxis = list(title = "Cardiac Output (L/min)"),
        showlegend = TRUE
      )

    p
  })

  # Hemodynamic Trends
  output$trends <- renderPlotly({
    # Simulate time-based data
    time_points <- 1:24  # 24 hours

    h <- hemodynamics()
    base_co <- h$cardiac_output
    base_hr <- h$heart_rate
    base_bp <- h$systolic_bp

    # Add some realistic variation (e.g., diurnal rhythm, random noise)
    set.seed(Sys.time()) # Use current time for more varied simulation each run
    co_trend <- base_co + sin(time_points * pi / 12) * 0.5 + rnorm(24, 0, 0.1) # Diurnal pattern
    hr_trend <- base_hr + sin(time_points * pi / 8) * 5 + rnorm(24, 0, 2)
    bp_trend <- base_bp + sin(time_points * pi / 6) * 10 + rnorm(24, 0, 3)

    p <- plot_ly() %>%
      add_trace(x = time_points, y = co_trend, type = 'scatter', mode = 'lines+markers',
                name = 'Cardiac Output (L/min)', yaxis = 'y', line = list(color = 'blue')) %>%
      add_trace(x = time_points, y = hr_trend, type = 'scatter', mode = 'lines+markers',
                name = 'Heart Rate (bpm)', yaxis = 'y2', line = list(color = 'red')) %>%
      add_trace(x = time_points, y = bp_trend, type = 'scatter', mode = 'lines+markers',
                name = 'Systolic BP (mmHg)', yaxis = 'y3', line = list(color = 'green')) %>%
      layout(
        title = "24-Hour Hemodynamic Trends",
        xaxis = list(title = "Time (hours)"),
        yaxis = list(title = "Cardiac Output", side = 'left', color = 'blue'),
        yaxis2 = list(title = "Heart Rate", side = 'right', overlaying = 'y', color = 'red', position = 0.85), # Adjust position to avoid overlap
        yaxis3 = list(title = "Blood Pressure", side = 'right', overlaying = 'y',
                      anchor = 'free', position = 0.95, color = 'green'), # Adjust position
        showlegend = TRUE
      )

    p
  })

  # Treatment Response
  output$treatment_response <- renderPlotly({
    # Simulate treatment response over time
    weeks <- 0:12

    # Baseline values for EF and an assumed baseline NYHA score
    baseline_ef <- input$ejection_fraction
    baseline_nyha_score <- case_when(
      input$ejection_fraction >= 50 ~ 1,
      input$ejection_fraction >= 40 ~ 2,
      input$ejection_fraction >= 30 ~ 3,
      TRUE ~ 4
    )

    # Calculate improvement based on treatments
    # Each medication's dose contribution to overall improvement factor
    ace_arb_dose_pct <- 0
    if (input$ace_type != "none") ace_arb_dose_pct <- input$ace_dose / 100
    else if (input$arb_type != "none") ace_arb_dose_pct <- input$arb_dose / 100

    bb_dose_pct <- input$bb_dose / 100
    mra_dose_pct <- input$mra_dose / 100
    sglt2_dose_pct <- input$sglt2_dose / 100

    # Overall improvement factor, reflecting combined benefits
    # These coefficients are illustrative but aim to reflect relative clinical impact
    total_drug_effect_factor <- (ace_arb_dose_pct * 0.05 +
                                 bb_dose_pct * 0.08 + # Beta blockers have strong effect on EF
                                 mra_dose_pct * 0.03 +
                                 sglt2_dose_pct * 0.06) # SGLT2i also have notable EF benefits

    # Gradual improvement over time (exponential decay to a max benefit)
    # EF improves from baseline towards a potential max_ef_gain
    max_ef_gain_potential <- 20 # Max 20% absolute EF points gain for severe HFrEF
    ef_improvement_trajectory <- (baseline_ef + (max_ef_gain_potential * total_drug_effect_factor)) * (1 - exp(-weeks/4))
    # Add initial EF to the gained EF, ensuring it doesn't drop
    ef_at_time <- baseline_ef + (ef_improvement_trajectory - baseline_ef)

    ef_at_time <- pmin(ef_at_time, 65) # Cap at reasonable maximum


    # NYHA class improvement - inverse of the EF improvement for visual trend
    # NYHA score reduces with treatment, reflecting improved function
    nyha_reduction_factor <- (ace_arb_dose_pct * 0.3 +
                              bb_dose_pct * 0.4 +
                              mra_dose_pct * 0.2 +
                              sglt2_dose_pct * 0.3)

    nyha_numeric_at_time <- baseline_nyha_score - (nyha_reduction_factor * (1 - exp(-weeks/4)))
    nyha_numeric_at_time <- pmax(1, nyha_numeric_at_time) # Cap at NYHA I

    p <- plot_ly() %>%
      add_trace(x = weeks, y = ef_at_time, type = 'scatter', mode = 'lines+markers',
                name = 'Ejection Fraction (%)', line = list(color = 'blue', width = 3)) %>%
      add_trace(x = weeks, y = nyha_numeric_at_time * 10, type = 'scatter', mode = 'lines+markers', # Scale NYHA for visual
                name = 'NYHA Functional Class (x10)', line = list(color = 'red', width = 2)) %>%
      layout(
        title = "Treatment Response Over Time",
        xaxis = list(title = "Weeks of Treatment"),
        yaxis = list(title = "Value (EF % / NYHA Score x10)", range = c(10, 70)), # Adjust range for both EF and scaled NYHA
        showlegend = TRUE
      )

    p
  })

  # Clinical Outcomes Table
  output$outcomes_table <- DT::renderDataTable({
    h <- hemodynamics() # Get current hemodynamic state
    # Extract current guideline adherence percentage from text output
    current_guideline_score_val_str <- gsub("Guideline Adherence: (\\d+)%.*", "\\1", output$guideline_score())
    current_guideline_score_val <- as.numeric(current_guideline_score_val_str) / 100 # Convert to 0-1 ratio

    # Get current medication doses as percentages for risk calculation
    ace_arb_dose_pct <- max(input$ace_dose, input$arb_dose) / 100
    bb_dose_pct <- input$bb_dose / 100
    mra_dose_pct <- input$mra_dose / 100
    sglt2_dose_pct <- input$sglt2_dose / 100
    diuretic_dose_pct <- input$diuretic_dose / 100

    # Calculate baseline and current mortality risk using the new function
    # Baseline mortality assumes no treatment (0% dose for all meds)
    baseline_mortality <- calculate_mortality_risk(
      input$ejection_fraction, input$age, input$gender, input$hypertension, input$diabetes,
      input$ckd, input$copd, input$afib, 0, 0, 0, 0,
      h$cardiac_output, h$bnp # Pass current CO and BNP as context for baseline risk calculation
    )

    # Current mortality uses actual treatment doses
    current_mortality <- calculate_mortality_risk(
      input$ejection_fraction, input$age, input$gender, input$hypertension, input$diabetes,
      input$ckd, input$copd, input$afib,
      ace_arb_dose_pct, bb_dose_pct, mra_dose_pct, sglt2_dose_pct,
      h$cardiac_output, h$bnp
    )

    # Rehospitalization risk (similarly affected by EF, symptoms, and diuretics/SGLT2i)
    # Higher risk for lower EF, higher NYHA, and if diuretics are not adequately used
    baseline_rehospitalization <- 0.35 + (1 - input$ejection_fraction/100) * 0.2
    if (h$nyha_class %in% c("III", "IV")) baseline_rehospitalization <- baseline_rehospitalization + 0.1
    current_rehospitalization <- baseline_rehospitalization * (1 - diuretic_dose_pct * 0.15) # Diuretics reduce symptoms/volume
    if (input$sglt2_type != "none") current_rehospitalization <- current_rehospitalization * (1 - sglt2_dose_pct * 0.25) # SGLT2i are very strong for this
    current_rehospitalization <- max(0.05, min(current_rehospitalization, 0.60)) # Clamp

    # Quality of Life Score (higher is better, 0-100 scale)
    # Improved by better EF, lower NYHA, and overall GDMT adherence
    baseline_qol <- 50 + (input$ejection_fraction - 30) * 0.5 # Lower EF means lower QoL
    current_qol <- baseline_qol * (1 + current_guideline_score_val * 0.4) # QoL improves significantly with GDMT
    current_qol <- max(30, min(current_qol, 95)) # Clamp

    # 6-Minute Walk Distance (meters, improved by better functional class and GDMT)
    baseline_6mwd <- 250 + (input$ejection_fraction - 30) * 2 # Base on EF
    current_6mwd <- baseline_6mwd * (1 + current_guideline_score_val * 0.3) # 6MWD improves with GDMT
    current_6mwd <- max(150, min(current_6mwd, 550)) # Clamp

    # Functional Capacity (NYHA) - directly from current hemodynamics
    current_functional_capacity <- h$nyha_class

    # Medication Adherence Score (conceptually related to complexity and patient education, but here by treatment uptake)
    # Simple model: Higher adherence with more meds, but not too high
    current_adherence <- 70 + (ace_arb_dose_pct + bb_dose_pct + mra_dose_pct + sglt2_dose_pct + diuretic_dose_pct) * 0.05 * 100 # Convert to percentage
    current_adherence <- min(current_adherence, 98) # Cap adherence

    outcomes_data <- data.frame(
      Parameter = c("1-Year Mortality Risk", "30-Day Rehospitalization Risk",
                    "Quality of Life Score (0-100)", "6-Minute Walk Distance (meters)",
                    "Functional Capacity (NYHA)", "Medication Adherence (%)"),

      Baseline = c(
        paste0(round(baseline_mortality * 100, 1), "%"),
        paste0(round(baseline_rehospitalization * 100, 1), "%"),
        round(baseline_qol, 0),
        round(baseline_6mwd, 0),
        h$nyha_class, # Baseline NYHA as determined by initial parameters
        "75%" # Assumed general adherence baseline
      ),

      Current = c(
        paste0(round(current_mortality * 100, 1), "%"),
        paste0(round(current_rehospitalization * 100, 1), "%"),
        round(current_qol, 0),
        round(current_6mwd, 0),
        current_functional_capacity, # Current NYHA from hemodynamics()
        paste0(round(current_adherence, 0), "%")
      ),

      Target = c("< 10%", "< 15%", "> 80", "> 450 meters", "I-II", "> 90%")
    )

    DT::datatable(outcomes_data, options = list(dom = 't', pageLength = 10),
                  rownames = FALSE) %>%
      DT::formatStyle(columns = 1:4, fontSize = '14px')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
