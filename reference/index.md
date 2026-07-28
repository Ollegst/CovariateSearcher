# Package index

## Main Workflow Functions

Core functions for running SCM

- [`initialize_covariate_search()`](https://ollegst.github.io/CovariateSearcher/reference/initialize_covariate_search.md)
  : Initialize covariate search state with validation and setup
- [`run_automated_scm_testing()`](https://ollegst.github.io/CovariateSearcher/reference/run_automated_scm_testing.md)
  : Execute full automated stepwise covariate modeling workflow from
  scratch
- [`run_stepwise_covariate_modeling()`](https://ollegst.github.io/CovariateSearcher/reference/run_stepwise_covariate_modeling.md)
  : Execute complete stepwise covariate modeling algorithm
- [`run_scm_selective_forward()`](https://ollegst.github.io/CovariateSearcher/reference/run_scm_selective_forward.md)
  : Execute proper stepwise forward selection with cumulative model
  building
- [`run_backward_elimination()`](https://ollegst.github.io/CovariateSearcher/reference/run_backward_elimination.md)
  : Execute backward elimination from a forward selection result

## Covariate Table and Formulas

Building the covariate search table and defining covariate effects

- [`build_covariate_reference_table()`](https://ollegst.github.io/CovariateSearcher/reference/build_covariate_reference_table.md)
  : Build a covariate parameter-search reference table
- [`add_covariates_to_search()`](https://ollegst.github.io/CovariateSearcher/reference/add_covariates_to_search.md)
  : Extend a running search with covariates it did not start with
- [`validate_param_transformations()`](https://ollegst.github.io/CovariateSearcher/reference/validate_param_transformations.md)
  : Validate parameter transformations for population covariates

## Model Operations

Functions for model creation and modification

- [`add_covariate_to_model()`](https://ollegst.github.io/CovariateSearcher/reference/add_covariate_to_model.md)
  : Add Covariate to Model
- [`remove_covariate_from_model()`](https://ollegst.github.io/CovariateSearcher/reference/remove_covariate_from_model.md)
  : Remove covariate using tag name with functional state update
- [`prepare_search_base_model()`](https://ollegst.github.io/CovariateSearcher/reference/prepare_search_base_model.md)
  : Create one prepared base model with multiple covariates
- [`create_retry_model()`](https://ollegst.github.io/CovariateSearcher/reference/create_retry_model.md)
  : Create retry model with modified initial estimates

## Resume and Checkpoints

Continuing an interrupted search

- [`continue_search()`](https://ollegst.github.io/CovariateSearcher/reference/continue_search.md)
  : Resume a stepwise covariate search from the last completed step
- [`get_step_models()`](https://ollegst.github.io/CovariateSearcher/reference/get_step_models.md)
  : Reconstruct one SCM step from the database
- [`reconstruct_step_from_disk()`](https://ollegst.github.io/CovariateSearcher/reference/reconstruct_step_from_disk.md)
  : Reconstruct one SCM step's database rows from the model files on
  disk

## Simulation and Forest Plots

Covariate effect simulation, forest plots and boxplots

- [`sample_individual_thetas()`](https://ollegst.github.io/CovariateSearcher/reference/sample_individual_thetas.md)
  : Sample THETA Parameter Vectors from Estimation Uncertainty
- [`create_covariate_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_table.md)
  : Create a Covariate Table (Null Patient + One-at-a-Time Variations)
- [`apply_covariate_model()`](https://ollegst.github.io/CovariateSearcher/reference/apply_covariate_model.md)
  : Apply a Model's Covariate Relationships to Individual Thetas
- [`build_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/build_scenario_parameters.md)
  : Build Parameter Sets for the Typical Subject and Each Covariate
  Scenario
- [`stack_scenario_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/stack_scenario_parameters.md)
  : Stack Scenario Parameter Tables into One Long, Scenario-Tagged Data
  Frame
- [`simulate_scenario_profiles()`](https://ollegst.github.io/CovariateSearcher/reference/simulate_scenario_profiles.md)
  : Simulate Concentration-Time Profiles for Each Covariate Scenario
- [`plot_exposure_forest()`](https://ollegst.github.io/CovariateSearcher/reference/plot_exposure_forest.md)
  : Forest Plot of a Covariate Effect on an Exposure Metric
- [`plot_parameter_forests()`](https://ollegst.github.io/CovariateSearcher/reference/plot_parameter_forests.md)
  : Save a Covariate Parameter Forest for Every Structural Parameter
- [`create_covariate_boxplots()`](https://ollegst.github.io/CovariateSearcher/reference/create_covariate_boxplots.md)
  : Create Covariate Boxplots for AUC / Cmax / Cmin
- [`theme_forest()`](https://ollegst.github.io/CovariateSearcher/reference/theme_forest.md)
  : Forest-plot ggplot2 Theme
- [`decode_dataset()`](https://ollegst.github.io/CovariateSearcher/reference/decode_dataset.md)
  : Decode Categorical Columns Using a Variable Specification

## Evaluation and Selection

Functions for model evaluation and selection

- [`select_best_model()`](https://ollegst.github.io/CovariateSearcher/reference/select_best_model.md)
  : Evaluate models and select the best one based on statistical
  criteria
- [`update_model_status_from_files()`](https://ollegst.github.io/CovariateSearcher/reference/update_model_status_from_files.md)
  : Updates search database with results from NONMEM output files
- [`get_param2()`](https://ollegst.github.io/CovariateSearcher/reference/get_param2.md)
  : Get Model Parameters and Statistics
- [`get_significant_models_from_step()`](https://ollegst.github.io/CovariateSearcher/reference/get_significant_models_from_step.md)
  : Extract models that showed significant improvement in a specific
  step
- [`get_model_covariates_from_db()`](https://ollegst.github.io/CovariateSearcher/reference/get_model_covariates_from_db.md)
  : Get all covariates in a model by tracing hierarchy

## Validation

Functions for model validation

- [`validate_base_model_for_search()`](https://ollegst.github.io/CovariateSearcher/reference/validate_base_model_for_search.md)
  : Validate base model readiness for covariate search
- [`validate_covariate_search_table()`](https://ollegst.github.io/CovariateSearcher/reference/validate_covariate_search_table.md)
  : Validate covariate search table
- [`validate_covariate_parameter_mapping()`](https://ollegst.github.io/CovariateSearcher/reference/validate_covariate_parameter_mapping.md)
  : Ensure covariate search parameters exist in model code
- [`validate_base_model_parameters()`](https://ollegst.github.io/CovariateSearcher/reference/validate_base_model_parameters.md)
  : Check base model parameter formatting during SCM initialization
- [`validate_parameter_blocks()`](https://ollegst.github.io/CovariateSearcher/reference/validate_parameter_blocks.md)
  : Check THETA, OMEGA, and SIGMA block formatting for SCM compatibility

## State Management

Functions for saving and loading search state

- [`save_search_state()`](https://ollegst.github.io/CovariateSearcher/reference/save_search_state.md)
  : Save Search State to Models Folder
- [`load_search_state()`](https://ollegst.github.io/CovariateSearcher/reference/load_search_state.md)
  : Load previously saved search state
- [`discover_existing_models()`](https://ollegst.github.io/CovariateSearcher/reference/discover_existing_models.md)
  : Discover existing models and set up relationships

## Reporting and Visualization

Functions for generating reports and tables

- [`generate_scm_report()`](https://ollegst.github.io/CovariateSearcher/reference/generate_scm_report.md)
  : Create SCM report using existing search database
- [`create_scm_results_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_scm_results_table.md)
  : Generate comprehensive SCM results table with evaluation comments
- [`view_comprehensive_table()`](https://ollegst.github.io/CovariateSearcher/reference/view_comprehensive_table.md)
  : View Comprehensive Table
- [`plot_nonmem_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/plot_nonmem_iterations.md)
  : Plot NONMEM Iteration Data

## Monitoring and Reporting

Functions for tracking progress and status

- [`get_model_status_from_files()`](https://ollegst.github.io/CovariateSearcher/reference/get_model_status_from_files.md)
  : Determine overall model status with detailed error reporting
- [`update_all_model_statuses()`](https://ollegst.github.io/CovariateSearcher/reference/update_all_model_statuses.md)
  : Updates all models with robust error handling and concise progress
  reporting
- [`get_model_status()`](https://ollegst.github.io/CovariateSearcher/reference/get_model_status.md)
  : Get current status of a model from database

## Internal Functions

Internal helper functions (advanced users only)

- [`CovariateSearcher-package`](https://ollegst.github.io/CovariateSearcher/reference/CovariateSearcher-package.md)
  [`CovariateSearcher`](https://ollegst.github.io/CovariateSearcher/reference/CovariateSearcher-package.md)
  : CovariateSearcher: Automated Stepwise Covariate Modeling for NONMEM
- [`adjust_theta_for_covariate()`](https://ollegst.github.io/CovariateSearcher/reference/adjust_theta_for_covariate.md)
  : Perturb the initial estimate of a covariate THETA for a retry
- [`calculate_covariate_df()`](https://ollegst.github.io/CovariateSearcher/reference/calculate_covariate_df.md)
  : Determine degrees of freedom for a covariate's likelihood-ratio test
- [`clean_dir()`](https://ollegst.github.io/CovariateSearcher/reference/clean_dir.md)
  : Remove all files starting with "WK\_" from all model subdirectories
- [`create_comprehensive_table()`](https://ollegst.github.io/CovariateSearcher/reference/create_comprehensive_table.md)
  : Create Comprehensive Table
- [`detect_estimation_problems()`](https://ollegst.github.io/CovariateSearcher/reference/detect_estimation_problems.md)
  : Monitor multiple models for estimation problems
- [`evaluate_removal_impacts()`](https://ollegst.github.io/CovariateSearcher/reference/evaluate_removal_impacts.md)
  : Evaluate the impact of removing each covariate
- [`extract_covariate_name_from_tag()`](https://ollegst.github.io/CovariateSearcher/reference/extract_covariate_name_from_tag.md)
  : Parse covariate name from tag string
- [`extract_model_params()`](https://ollegst.github.io/CovariateSearcher/reference/extract_model_params.md)
  : Extract Model Parameters from Control File
- [`extract_nonmem_timestamps()`](https://ollegst.github.io/CovariateSearcher/reference/extract_nonmem_timestamps.md)
  : Parse LST file to extract actual NONMEM execution timestamps
- [`extract_params()`](https://ollegst.github.io/CovariateSearcher/reference/extract_params.md)
  : Extract Parameters from NONMEM Control File
- [`find_model_file()`](https://ollegst.github.io/CovariateSearcher/reference/find_model_file.md)
  : Find the actual file path for a NONMEM model
- [`fix_theta_renumbering()`](https://ollegst.github.io/CovariateSearcher/reference/fix_theta_renumbering.md)
  : Renumber THETA parameters after removing some
- [`force_update_models()`](https://ollegst.github.io/CovariateSearcher/reference/force_update_models.md)
  : Force update one or more models with fresh file reads
- [`generate_changes_display()`](https://ollegst.github.io/CovariateSearcher/reference/generate_changes_display.md)
  : Generate Changes Display
- [`generate_recovery_report()`](https://ollegst.github.io/CovariateSearcher/reference/generate_recovery_report.md)
  : Generate comprehensive recovery statistics and summary
- [`generate_step_description()`](https://ollegst.github.io/CovariateSearcher/reference/generate_step_description.md)
  : Generate Step Description (VECTORIZED VERSION)
- [`generate_step_display()`](https://ollegst.github.io/CovariateSearcher/reference/generate_step_display.md)
  : Generate Step Display
- [`generate_tags_from_covariate_search()`](https://ollegst.github.io/CovariateSearcher/reference/generate_tags_from_covariate_search.md)
  : Generate or Update tags.yaml File from Covariate Search Table
- [`get_covariate_tag_from_name()`](https://ollegst.github.io/CovariateSearcher/reference/get_covariate_tag_from_name.md)
  : Convert covariate name back to tag
- [`get_covariates_from_models()`](https://ollegst.github.io/CovariateSearcher/reference/get_covariates_from_models.md)
  : Extract covariate names from specific models
- [`get_excluded_covariates()`](https://ollegst.github.io/CovariateSearcher/reference/get_excluded_covariates.md)
  : Get list of covariates excluded from current step with details
- [`get_fixed_covariates()`](https://ollegst.github.io/CovariateSearcher/reference/get_fixed_covariates.md)
  : Identify covariates with FIX status
- [`get_model_covariates_from_files()`](https://ollegst.github.io/CovariateSearcher/reference/get_model_covariates_from_files.md)
  : Extract covariates from model using BBR tags
- [`get_model_max_rse()`](https://ollegst.github.io/CovariateSearcher/reference/get_model_max_rse.md)
  : Extract maximum RSE from model using existing functionality
- [`get_model_ofv_from_database()`](https://ollegst.github.io/CovariateSearcher/reference/get_model_ofv_from_database.md)
  : Extract OFV from completed model database entry
- [`get_remaining_covariates()`](https://ollegst.github.io/CovariateSearcher/reference/get_remaining_covariates.md)
  : Get list of covariate tags that haven't been tested from base model
- [`handle_failed_retry()`](https://ollegst.github.io/CovariateSearcher/reference/handle_failed_retry.md)
  : Handle failed retry model by excluding covariate from step
- [`initialize_search_config()`](https://ollegst.github.io/CovariateSearcher/reference/initialize_search_config.md)
  : Initialize search configuration parameters
- [`initialize_search_database_core()`](https://ollegst.github.io/CovariateSearcher/reference/initialize_search_database_core.md)
  : Initialize Search Database
- [`load_existing_search()`](https://ollegst.github.io/CovariateSearcher/reference/load_existing_search.md)
  : Load existing models and recreate search state
- [`load_tags()`](https://ollegst.github.io/CovariateSearcher/reference/load_tags.md)
  : Load covariate tags from YAML configuration
- [`model_add_cov()`](https://ollegst.github.io/CovariateSearcher/reference/model_add_cov.md)
  : Core functionality to add covariate to NONMEM model file with
  enhanced logging
- [`model_report()`](https://ollegst.github.io/CovariateSearcher/reference/model_report.md)
  : Generate Parameter Table Report for Multiple Models
- [`` `%||%` ``](https://ollegst.github.io/CovariateSearcher/reference/null-coalesce.md)
  : NULL-coalescing operator
- [`print_parameter_validation()`](https://ollegst.github.io/CovariateSearcher/reference/print_parameter_validation.md)
  : Display parameter validation results in readable format
- [`print_scm_results_table()`](https://ollegst.github.io/CovariateSearcher/reference/print_scm_results_table.md)
  : Display SCM results table with RSE and evaluation comments
- [`process_estimation_issues()`](https://ollegst.github.io/CovariateSearcher/reference/process_estimation_issues.md)
  : Process detected estimation issues with smart retry/exclusion logic
- [`pvalue_to_threshold()`](https://ollegst.github.io/CovariateSearcher/reference/pvalue_to_threshold.md)
  : Calculate ΔOFV threshold from p-value for likelihood ratio test
- [`read_ext_file()`](https://ollegst.github.io/CovariateSearcher/reference/read_ext_file.md)
  : Parse .ext files and detect OFV \> 10^10 and other estimation
  problems
- [`read_ext_iterations()`](https://ollegst.github.io/CovariateSearcher/reference/read_ext_iterations.md)
  : Read NONMEM Extended Output File
- [`read_model_file()`](https://ollegst.github.io/CovariateSearcher/reference/read_model_file.md)
  : Read NONMEM control file with proper path handling
- [`read_nonmem_ext()`](https://ollegst.github.io/CovariateSearcher/reference/read_nonmem_ext.md)
  : Extract OFV and parameters from NONMEM .ext file
- [`read_nonmem_lst()`](https://ollegst.github.io/CovariateSearcher/reference/read_nonmem_lst.md)
  : Robust LST file reader with comprehensive error handling
- [`run_univariate_step()`](https://ollegst.github.io/CovariateSearcher/reference/run_univariate_step.md)
  : Run Univariate Step
- [`submit_and_wait_for_step()`](https://ollegst.github.io/CovariateSearcher/reference/submit_and_wait_for_step.md)
  : Submit models and wait for all to complete with status tracking
- [`theme_pps_table()`](https://ollegst.github.io/CovariateSearcher/reference/theme_pps_table.md)
  : Apply PPS Theme to Flextable
- [`update_model_counter()`](https://ollegst.github.io/CovariateSearcher/reference/update_model_counter.md)
  : Update model counter excluding retry models
- [`update_tags_yaml()`](https://ollegst.github.io/CovariateSearcher/reference/update_tags_yaml.md)
  : Generate Tags YAML with Search State Integration
- [`validate_setup()`](https://ollegst.github.io/CovariateSearcher/reference/validate_setup.md)
  : Validate initialized search setup
- [`view_exclusion_status()`](https://ollegst.github.io/CovariateSearcher/reference/view_exclusion_status.md)
  : Display current exclusion status with details
- [`write_model_file()`](https://ollegst.github.io/CovariateSearcher/reference/write_model_file.md)
  : Write modified NONMEM control file back to disk
