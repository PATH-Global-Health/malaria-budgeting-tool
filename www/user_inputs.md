## User Inputs

This section allows the user to upload or download malaria intervention plans for budgeting. Each new spreadsheet represents a plan for specific years and locations, detailing which interventions are planned to be implemented.

The tool is designed for a specific country context and includes the following pre-specified data needed for budget calculations:

- Administrative boundaries and names
- Landmass in Km²
- Indicator of urban status of the smallest spatial unit used for intervention planning
- Target population data
- Number of health facilities by type (primary, secondary, tertiary) per spatial unit
- Under 5 parasite prevalence data from DHS surveys
- Historic intervention coverage data

### Steps to Use – No previous data uploaded:

Follow these steps if you have yet used the tool – see the number referenced screen shot to reference between steps.  

📆️ Select planning years to define the scope of your scenario

📄 Click 'Download Empty Scenario Template'  
< screen shot >

📊 Fill out the Excel template: each sheet corresponds to a year, and each row represents the smallest spatial unit used for intervention planning. Each column details a specific type of malaria intervention preceded by the word ‘code_’. Enter 1 for planned interventions and 0 otherwise. In the corresponding ‘type_’ column for each intervention, select the type of intervention planned. For example, in the ‘type_vaccine’ column, select from “R21” or “RTSS”. You may overwrite dropdown values with free text.

< screen shot >

💾 Save the specified plan file locally.

📤 Return to the web application and upload the completed Excel file using the form. Provide a short name (e.g., “Plan A”) and description (e.g., “Fully scaled up plan”) — clear descriptions are useful for comparisons.

📂 Press “Submit Scenario” to upload the spreadsheet.

📋 Uploaded plans will appear in a summary table with details like plan name, years covered, and upload date.  
< screen shot >

💵 Next, specify the unit costs required for budgeting.

📄 Click 'Download Empty Cost Template'

📊 Fill out the Excel template. Columns include:

- `resource_name`: Description of the cost item
- `code_intervention`: Type of intervention (e.g., vaccine, itn_campaign)
- `type_intervention`: Specific subtype (e.g., r21, PBO net)
- `cost_class`: Procurement, Distribution, Operational, Supportive
- `unit`: e.g., per dose, per ITN
- `local_currency_cost`, `usd_cost`, `cost_year`, `source`

Ensure every delivered intervention has matching unit costs.

💾 Save the cost file locally.

📤 Upload the file in the tool. Provide a name and description (e.g., “Cost Assumptions 1”).

📂 Press “Submit Cost Sheet”.

📋 Uploaded cost data will appear in a summary table with name, description, and upload date.

### Steps to Use – Previous data uploaded:

You may download a pre-populated scenario template for a previously uploaded plan to build a new one more easily. Remember to update the name and description before re-uploading.

Once both plans and costs are uploaded, proceed to the next stage.
