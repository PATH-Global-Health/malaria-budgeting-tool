tab0UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("DEMO Malaria Budget Generation and Comparison Tool"),

    h6("This tool supports National Malaria Programs in generating, reviewing, and comparing malaria intervention budgets across different operational scenarios. It is part of the Costed Optimised Operational Plan initiative, which aims to develop a unified, optimized budget that aligns with country priorities and supports multiple funding requests."),

    h6("The process streamlines national planning by aligning funding proposals to key donors, including the Global Fund, while strengthening national ownership and reducing administrative burden. CO-OP is a core component of the RBM Partnership’s 'Big Push' and is supported by stakeholders across the malaria ecosystem."),

    h6("Use the instructions below to navigate the tool. Each section provides guidance on how to enter data, view outputs, and generate reports."),

    div(
      style = "background-color: #fff3cd; border-left: 6px solid #ffa500; padding: 15px; margin-bottom: 20px;",
      strong("IMPORTANT: This is a demonstration version of the tool. "),
      "Data uploading functionality has been suspended. The values and outputs presented here are illustrative only, intended to showcase the tool's features. They should not be used for any decision-making or extrapolation.
      In addition, the data presented here is not representative of any real-world scenarios or costs.

      Our tool is in active development and therefore the version presented here is meant to be illustrative of the types of functionality that we are building out. We still have many features in progress and can't wait to share in the near future. Please reach out to hthompson@path.org with any suggestions or feedback you may have too we'd love to gain any insights from our community!",
    ),

    h3("Instructions"),

    accordion(
      id = ns("instructions"),
      open = NULL,

      accordion_panel(
        title = "User Inputs",
        tagList(
          p("This section allows the user to upload or download malaria intervention plans for budgeting. Each new spreadsheet represents a plan for specific years and locations, detailing which interventions are planned to be implemented."),

          p("The tool is designed for a specific country context and includes the following pre-specified data needed for budget calculations:"),
          tags$ul(
            tags$li("Administrative boundaries and names"),
            tags$li("Landmass in Km2"),
            tags$li("Indicator of urban status of the smallest spatial unit used for intervention planning."),
            tags$li("Target population data"),
            tags$li("Number of health facilities by type (primary, secondary, tertiary) per spatial unit"),
            tags$li("Under 5 parasite prevalence data from DHS surveys"),
            tags$li("Historic intervention coverage data")
          ),

          tags$b("Steps to Use – No previous data uploaded:"),
          p("Follow these steps if you have yet used the tool – see the number referenced screen shot to reference between steps."),

          tags$ol(
            tags$li("📆️ Select planning years to define the scope of your scenario"),
            tags$li("📄 Click 'Download Empty Scenario Template'"),
            tags$li("📊 Fill out the Excel template: each sheet corresponds to a year, and each row represents the smallest spatial unit used for intervention planning. Each column details a specific type of malaria intervention preceded by the word ‘code_’ Enter 1 for planned interventions and 0 otherwise. In the corresponding ‘type_’ column for each intervention select the type of intervention that is planned for. For example, in the ‘type_vaccine’ column select from “R21” or “RTSS”. If the intervention type is not included in the drop down list you can overwrite with free text to include this."),
            tags$li("💾 Once a plan has been specified by indicating what interventions are to be targeted where each year the user can save a local copy of this file."),
            tags$li("📤 Return to the web application and upload the completed Excel file using the form, and give the scenario a short name: e.g. Plan A/ Plan 1 etc and a description: e.g. “Fully scaled up plan”/ “Restricted vaccine roll out plan” – make sure these are informative descriptions as it will be helpful when comparing plans."),
            tags$li("📂 Press “Submit Scenario” button and the spreadsheet will be uploaded in to the tool."),
            tags$li("📋 Uploaded plans will appear in a summary table with details like plan name, years covered, and upload date."),
            tags$li("💵 Now we also need to specify the unit costs that will be critical for the tool to generate our budgets."),
            tags$li("📄 Click 'Download Empty Cost Template'"),
            tags$li("📊 Fill out the Excel template: The Unit Cost template allows for the user to have flexibility in specifying the exact unit costs relevant to the country context the template has the following columns:"),
            tags$ul(
              tags$li("resource_name: Long name description of unit cost data e.g. “Procurement cost of malaria vaccine per dose”"),
              tags$li("code_intervention: Select from the drop down list the type of intervention the unit cost related to e.g. vaccine/ itn_campaign/ smc"),
              tags$li("type_intervention: Select from the drop down list the specific type of intervention unit cost relates to e.g. r21/ rtss/ pyrethroid_net/ PBO net"),
              tags$li("cost_class: Select from the drop down list the class of cost: e,g Procurement/ Distribution/ Operational/ Supportive"),
              tags$li("unit: specify the exact unit e.g. per dose, per ITN, per child etc"),
              tags$li("local_currency_cost: Specific cost in local currency"),
              tags$li("usd_cost: Specific cost in USD"),
              tags$li("cost_year: Year that cost relates to"),
              tags$li("source: Reference the cost source used to generate unit cost data")
            ),
            tags$li("Ensure that for each intervention being delivered there are unit costs for the specific intervention and intervention type."),
            tags$li("💾 Once unit cost data has been specified, the user can save a local copy of this file."),
            tags$li("📤 Return to the web application and upload the completed Excel file using the form and give the cost sheet a name: e.g. “Cost Assumptions 1” etc and a description: e.g. “Including a higher cost estimate of procurement for PBO nets” – make sure these are informative descriptions as it will be helpful when generating and comparing plans."),
            tags$li("📂 Press “Submit Cost Sheet” button and the spreadsheet will be uploaded in to the tool."),
            tags$li("📋 Uploaded cost data will appear in a summary table with details like cost name, description, and upload date.")
          ),

          tags$b("Steps to Use – Previous data uploaded:"),
          p("Once a spreadsheet has been uploaded into the tool the user is capable downloading a template based on a specific scenario uploaded – this can make it easier to quickly populate a new scenario without having to replicate every item but make sure to input a new scenario name and description when re-uploading."),

          p("Once both sets of data have been uploaded the user can move on to the next stage of the application.")
        )
      ),

      accordion_panel(
        title = "Check Scenarios",
        tagList(
          p("Once a plan has been uploaded the user can move onto the ‘Check Scenario’ tab. This section allows users to validate their uploaded intervention plans and flag any inconsistencies or logic errors. It provides a quick visual and tabular summary of intervention coverage by state and year."),
          p("The section will also flag or potential delivery errors using warning alerts"),
          tags$b("Steps to Use:"),
          tags$ol(
            tags$li("📂 Select a previously uploaded plan from the dropdown menu."),
            tags$li("📅 Select the year of interest from the dropdown list. The data will update accordingly."),
            tags$li("💉 Click on an intervention tab (e.g., SMC, Vaccine, IRS, etc.) to view targeting information."),
            tags$li("📊 View headline coverage statistics shown as colored summary boxes: ✅ Full Coverage, 🟧 Partial Coverage, ❌ No Coverage."),
            tags$li("📋 Review the interactive table below to see detailed counts by state."),
            tags$li("🔍 Use the search bar to quickly find specific states or values."),
            tags$li("⚠️ If an intervention targeting error is spotted by the tool this will be flagged to the user. Example: <screen shot>")
          ),
          p("📝 Tip: Use this section to verify the accuracy of your uploaded scenario before generating budgets.")
        )
      ),

      accordion_panel(
        title = "Generate Budgets",
        tagList(
          p("This section allows users to generate full intervention budgets by combining previously uploaded plans with a selected cost sheet. The tool calculates total budgets based on the selected intervention mix, target population, and budget assumptions defined for each intervention."),
          tags$b("Steps to Use:"),
          tags$ol(
            tags$li("📝 Select one or more intervention plans from the drop down."),
            tags$li("💵 Select a cost sheet from the dropdown."),
            tags$li("📐 Select budget quantification assumptions for each intervention."),
            tags$li("Select the procurement buffer assumption to be used."),
            tags$li("⚠️ Review and confirm selections. If not, the tool will flag missing data."),
            tags$li("⚙️ Click the “Generate Budgets” button."),
            tags$li("⏳ Wait for the confirmation message. Generated budget data will appear under the Plan Visualization and Plan Comparisons tabs.")
          ),
          p("📝 Tip: Ensure your cost sheet includes values for all intervention types and subtypes planned for use (e.g., PBO vs. standard nets, RTSS vs. R21 vaccines).")
        )
      ),

      accordion_panel(
        title = "Plan Visualization",
        tagList(
          p("This section allows users to view detailed outputs from a selected budgeted plan. It includes maps, tables, and visual summaries showing the spatial distribution, cost breakdown, and overall budget profile of the selected intervention plan."),
          tags$b("Steps to Use:"),
          tags$ol(
            tags$li("🧾 Select inputs at the top of the page (plan, scale, year, currency)."),
            tags$li("🧭 Review the Plan Overview section."),
            tags$li("🗺️ Explore the maps: Full Intervention Mix and Intervention-Specific."),
            tags$li("👥 View headline population and budget figures."),
            tags$li("📋 Review the Budget Summary Table."),
            tags$li("📈 Explore budget visualizations by item, category, and geography."),
            tags$li("🔍 Use filters and interactive tools. Hover and use ℹ️ icons for explanations.")
          ),
          p("📝 Tip: Use this tab to verify intervention coverage across geographies and assess budget concentration by intervention or region before comparing plans.")
        )
      ),
      accordion_panel(
        title = "Plan Comparisons",
        tagList(
          p("This section allows users to compare the budgets of different intervention plans side by side. Comparisons are conducted at the national level only and help users understand the differences in intervention mix, budget allocation, and total cost across plans."),
          tags$b("Steps to Use:"),
          tags$ol(
            tags$li("📂 Select the Baseline Plan"),
            tags$li("📋 Select one or more plans to compare"),
            tags$li("🗓️ Select the Year of Interest and Currency"),
            tags$li("🗺️ View side-by-side comparison maps"),
            tags$li("📈 Review the Total Cost Comparison Chart"),
            tags$li("📊 Review the Intervention-Specific Budget Comparison")
          ),
          p("📝 Tip: Use this page to highlight trade-offs across funding scenarios and identify which interventions or geographies drive cost differences.")
        )
      ),
      accordion_panel(
        title = "Report Generation",
        tagList(
          p("This section allows users to download a comprehensive report summarizing the selected budget plan, including inputs, assumptions, methods, and outputs. It also provides access to all figures and raw data used for budget generation to support full transparency and reproducibility."),
          tags$b("Steps to Use:"),
          tags$ol(
            tags$li("📂 Select the Plan, Year(s), and Currency"),
            tags$li("📝 Customize Report Metadata"),
            tags$li("📥 Download Options: Report (Word format), Figures, Data")
          ),
          p("📝 Tip: This section is ideal for preparing draft reports for submission to funders (e.g., Global Fund concept notes or PMI MOPs), or for national program documentation.")
        )
      ),
      accordion_panel(
        title = "Methods",
        tagList(
          p("This section provides a detailed reference on the data sources, calculation logic, and assumptions used throughout the budget generation process. It is intended to support interpretation, reproducibility, and citation of results produced by the tool."),
          tags$b("What You’ll Find in This Section:"),
          tags$ul(
            tags$li("📌 Budgeting Assumptions"),
            tags$li("📊 Costing Methodology"),
            tags$li("🗂️ Data Sources"),
            tags$li("⚙️ Tool Logic and Calculation Flow")
          ),
          p("📝 Tip: Refer to this section if you need to explain or justify the output values in a funding application or policy discussion.")
        )
      )
    )
  )
}
