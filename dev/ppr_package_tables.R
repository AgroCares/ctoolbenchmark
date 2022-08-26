# generate package tables

  # get internal copies of CROP table
  ctb_crop <- as.data.table(carboncastr::cc.crops)

  # select only relevant columns
  ctb_crop <- ctb_crop[,.(b_lu_brp = crop_code,
                          b_lu_name = crop_name,
                          b_lu_eoc = 0.5 * crop_eom,
                          b_lu_eoc_residue = 0.5 * crop_eom_residue,
                          hc,
                          fr_dpm_rpm)]

  # save updated crop table
  usethis::use_data(ctb_crop,overwrite = TRUE)
