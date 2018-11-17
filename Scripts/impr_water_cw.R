# IMPROVED WATER CROSSWAK

impr_water_cw <- tibble::tribble(
                                                 ~watersource, ~code, ~improved,
                                        "piped into dwelling",  1,  1,
                                       "piped into plot/yard",  2,  1,
                                      "public tap/stand pipe",  3,  1,
                                "tubewell/borehole with pump",  4,  1,
                                             "protected well",  5,  1,
                                           "unprotected well",  6,  0,
                                           "protected spring",  7,  1,
                                         "unprotected spring",  8,  0,
                                      "rain water collection",  9,  1,
                                              "tankers-truck", 10,  0,
                          "cart with small tank/drum/buckets", 11,  0,
                                       "bicyles with buckets", 12,  0,
  "river, stream, pond, dam, lake, canal, irrigation channel", 13,  0,
                                              "bottled water", 14,  0,
                                                      "other", 96,  0
  )
