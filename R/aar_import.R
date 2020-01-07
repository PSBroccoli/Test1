#' Import AAR Data
#'
#' A set of functions to convert AAR data into tidy dataframes for use in R.
#'
#' @param file_name path to a .csv file containing AAR data in the following format:
#'
#'  - Sample name/ID is in the first column, labelled sample_name
#'
#'   - All other info columns follow sample_name
#'
#'   - The last column before concentration and D/L data is the total concentration, labelled total_conc
#'
#'   - Amino acid concentration columns are in the form form asx_c etc. (all lowercase)
#'
#'   - L-Thr, L-His and L-Arg should just be named thr_c, his_c, arg_c for HPLC
#'
#'   - Amino acid D/L columns are in the form asx, glx, ser etc.
#'
#'   - All instances of #DIV/0! have been converted to NA
#'
#' @param machine "UHPLC" or "HPLC"
#' @param conc_unit if your concentration data is in pmol / mg (the default for NE_aar spreadsheets), use "nmol" to convert to nmol / mg
#' @param na_rep replace all values of 0 with NA
#'
#' @return Returns a tidy table for use in ggplot2
#' @export
#'
#' @examples
#' aar_data <- aar_import("/R/Data/HGAB perforam.csv", machine = "UHPLC", conc_unit = "nmol", na_rep = TRUE)
aar_import <- function(file_name, machine = "HPLC", conc_unit = "pmol", na_rep = FALSE){

  ## Import table
  aar_wide <- tbl_df(read.csv(file_name))

  ## Replace instances of 0 with NA if na_rep is "yes"
  if(na_rep == TRUE){
    aar_wide[aar_wide == 0] <- NA
  }

  ## Make DL table - select DL variables and tidy
  aar_dl <- aar_wide %>%
    select(-c(asx_c:ile_c)) %>%
    gather(amino_acid, dl, -c(sample_name:total_conc))

  ## Make conc table - select conc variables
  aar_conc <- aar_wide %>%
    select(-c(asx:ile))

  ## Rename conc variables according to which machine was used
  if(machine == "UHPLC"){
    aar_conc <- aar_conc %>%
      rename(asx = asx_c, glx = glx_c, ser = ser_c, thr = thr_c, his = his_c,
             gly = gly_c, arg = arg_c, ala = ala_c, tyr = tyr_c, val = val_c,
             met = met_c, phe = phe_c, leu = leu_c, ile = ile_c)
  }else if(machine == "HPLC"){
    aar_conc <- aar_conc %>%
      rename(asx = asx_c, glx = glx_c, ser = ser_c, thr = thr_c, his = his_c,
             gly = gly_c, arg = arg_c, ala = ala_c, tyr = tyr_c, val = val_c,
             phe = phe_c, leu = leu_c, ile = ile_c)
  }

  ## Tidy conc table
  aar_conc <- aar_conc %>%
    gather(amino_acid, conc, -c(sample_name:total_conc))

  ## Join conc and DL tables
  aar <- aar_conc %>%
    left_join(aar_dl, by = NULL)

  ## Capitalise amino acids
  substr(aar$amino_acid, 1, 1) <- toupper(substr(aar$amino_acid, 1, 1))

  ## Convert select column names into factors and enforce their order
  aar$amino_acid <- aar$amino_acid %>% factor() %>% fct_inorder()

  if("sample_no" %in% colnames(aar)){
    aar$sample_no <- aar$sample_no %>% factor() %>% fct_inorder()
  }
  if("heat_time" %in% colnames(aar)){
    aar$heat_time <- aar$heat_time %>% factor() %>% fct_inorder()
  }

  ## Return tidy table to variable name, converting concs to nmol/mg if chosen
  if(conc_unit == "pmol"){
    return(aar)
  }else if(conc_unit == "nmol"){
    aar$total_conc <- as.numeric(as.character(aar$total_conc))
    aar$conc <- as.numeric(as.character(aar$conc))
    aar <- mutate(aar, conc = (conc / 1000),
                  total_conc = (total_conc / 1000))
    return(aar)
  }
}
