sample_raw_eEPSC_df <- make_normalized_EPSC_data(
  filename = load_ext_data("sample_eEPSC_data.csv"),
  current_type = "eEPSC",
  min_time_value = 0,
  max_time_value = 25,
  interval_length = 5,
  baseline_length = 5,
  negative_transform_currents = "yes"
)

usethis::use_data(sample_raw_eEPSC_df, overwrite = TRUE)


