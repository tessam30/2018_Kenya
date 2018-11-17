# Crosswalk for improved sanitation
#impr_sanit_cw.R

impr_sanit_cw <- tibble::tribble(
                         ~toilet_type, ~code,         ~improved,
        "flush to piped sewer system",   11,        "improved",
               "flush to septic tank",   12,        "improved",
             "flush to pit (latrine)",   13,        "improved",
            "flush to somewhere else",   14,        "improved",
             "flush to unknown place",   15,        "improved",
    "ventilated improved pit latrine",   21,        "improved",
              "pit latrine with slab",   22,        "improved",
  "pit latrine without slab/open pit",   23,      "unimproved",
                  "composting toilet",   31,        "improved",
                      "bucket toilet",   41,      "unimproved",
                     "hanging toilet",   51,      "unimproved",
                        "no facility",   61, "open defecation",
                              "other",   96,      "unimproved"
  )
