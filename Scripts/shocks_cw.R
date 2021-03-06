shock_cw <- tibble::tribble(
     ~shock_alt,      ~shock, ~code,
           "ag",        "ag",  102,
    "livestock",        "ag",  103,
    "livestock",        "ag",  104,
     "conflict",  "conflict",  119,
     "conflict",  "conflict",  120,
     "violence",  "conflict",  123,
     "violence",  "conflict",  124,
    "financial", "financial",  105,
    "financial", "financial",  106,
    "financial", "financial",  107,
    "financial", "financial",  117,
       "hazard",    "hazard",  101,
  "water_short",    "hazard",  111,
       "hazard",    "hazard",  118,
  "demographic",    "health",  112,
  "demographic",    "health",  113,
  "demographic",    "health",  114,
  "demographic",    "health",  115,
       "health",    "health",  125,
  "demographic",     "other",  116,
        "other",     "other",  121,
        "other",     "other",  122,
        "other",     "other",  126,
        "other",     "other",  127,
   "crop_price",     "price",  108,
   "food_price",     "price",  109,
  "input_price",     "price",  110
  )

write_csv(shock_cw, file.path(kihbspath, "shocks_cw.csv"))
haven::write_dta(shock_cw, file.path(kihbspath, "shocks_cw.dta"))
