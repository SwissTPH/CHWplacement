setwd(".")
source("create_sh_scenarios.R")

# scenario A
batch_CHW_scenario( radius=60, buffer=0, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioA",
                    dpt_init=1, dpt_end=13
                    , time = "1-00:00:00"
                    , mem = "50000"
                    , qos_sub = "1day"
)

# scenario A, aire metropolitaine
batch_CHW_scenario( radius=60, buffer=0, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioAmetro",
                    dpt_init=14, dpt_end=14
                    , time = "1-00:00:00"
                    , mem = "50000"
                    , qos_sub = "1day"
)


# scenario B
batch_CHW_scenario( radius=60, buffer=30, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioB",
                    dpt_init=1, dpt_end=14
                    , time = "1-00:00:00"
                    , mem = "50000"
                    , qos_sub = "1day"
)


# scenario C out
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                                rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                                name="scenarioCout",
                                dpt_init=2, dpt_end=14
                                , time = "1-00:00:00"
                                , mem = "50000"
                                , qos_sub = "1day"
)

# scenario C in
batch_CHW_scenario( radius=60, buffer=60, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCin",
                    dpt_init=1, dpt_end=14
                    , time = "1-00:00:00"
                    , mem = "50000"
                    , qos_sub = "1day"
)

# scenario C2 in
batch_CHW_scenario( radius=60, buffer=60, capaurb=4000, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioC2in",
                    dpt_init=1, dpt_end=14
                    , time = "1-00:00:00"
                    , mem = "50000"
                    , qos_sub = "1day"
)



########
###############
# Changing parameters in scenario C, 10% plus and minus

###
# radius
# scenario C out
batch_CHW_scenario( radius=54, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_rad")
batch_CHW_scenario( radius=54, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_rad",dpt_init = 13, dpt_end = 13)
# scenario C in
batch_CHW_scenario( radius=54, buffer=60, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCin_rad")

# scenario C out
batch_CHW_scenario( radius=66, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_rad")
# scenario C in
batch_CHW_scenario( radius=66, buffer=60, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCin_rad")

###
# buffer
# scenario C out
batch_CHW_scenario( radius=60, buffer=54, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_buf")
batch_CHW_scenario( radius=60, buffer=54, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_buf", dpt_init = 5,dpt_end = 5)
# scenario C in
batch_CHW_scenario( radius=60, buffer=54, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCout_buf")

# scenario C out
batch_CHW_scenario( radius=60, buffer=66, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_buf")
batch_CHW_scenario( radius=60, buffer=66, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_buf", dpt_init = 13,dpt_end = 13)
# scenario C in
batch_CHW_scenario( radius=60, buffer=66, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCout_buf")

###
# capacity
# scenario C out
batch_CHW_scenario( radius=60, buffer=60, capaurb=2250, caparur=900,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_cap")
batch_CHW_scenario( radius=60, buffer=60, capaurb=2250, caparur=900,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_cap", dpt_init = 5,dpt_end = 5)

batch_CHW_scenario( radius=60, buffer=60, capaurb=2250, caparur=900,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_cap", dpt_init = 13,dpt_end = 13)
# scenario C in
batch_CHW_scenario( radius=60, buffer=60, capaurb=3600, caparur=3600,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCout_cap")

# scenario C out
batch_CHW_scenario( radius=60, buffer=60, capaurb=2750, caparur=1100,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_cap")
batch_CHW_scenario( radius=60, buffer=60, capaurb=2750, caparur=1100,
                    rururb_cutoff=300, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_cap", dpt_init = 5,dpt_end = 5)
# scenario C in
batch_CHW_scenario( radius=60, buffer=60, capaurb=4400, caparur=4400,
                    rururb_cutoff=300, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCout_cap")

###
# rururb_cutoff
# scenario C out
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=270, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_cut")
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=270, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_cut", dpt_init = 5,dpt_end = 5)
# scenario C in
batch_CHW_scenario( radius=60, buffer=60, capaurb=4000, caparur=4000,
                    rururb_cutoff=270, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCin_cut")

# scenario C out
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=330, minurbsize =2000, isinside=0,gurobi_gap=5,
                    name="scenarioCout_cut")
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=330, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_cut", dpt_init = 5, dpt_end = 5)
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=330, minurbsize =2000, isinside=0,gurobi_gap=6,
                    name="scenarioCout_cut", dpt_init = 13, dpt_end = 13)
# scenario C in
batch_CHW_scenario( radius=60, buffer=60, capaurb=4000, caparur=4000,
                    rururb_cutoff=330, minurbsize =2000, isinside=1,gurobi_gap=5,
                    name="scenarioCin_cut")

###
# min urban size
# scenario C out
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =1800, isinside=0,gurobi_gap=5,
                    name="scenarioCout_mus")
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =1800, isinside=0,gurobi_gap=6,
                    name="scenarioCout_mus", dpt_init = 5, dpt_end = 5)
# scenario C in
batch_CHW_scenario( radius=60, buffer=60, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =1800, isinside=1,gurobi_gap=5,
                    name="scenarioCout_mus")

# scenario C out
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2200, isinside=0,gurobi_gap=5,
                    name="scenarioCout_mus")
batch_CHW_scenario( radius=60, buffer=60, capaurb=2500, caparur=1000,
                    rururb_cutoff=300, minurbsize =2200, isinside=0,gurobi_gap=6,
                    name="scenarioCout_mus", dpt_init = 5, dpt_end = 5)
# scenario C in
batch_CHW_scenario( radius=60, buffer=60, capaurb=4000, caparur=4000,
                    rururb_cutoff=300, minurbsize =2200, isinside=1,gurobi_gap=5,
                    name="scenarioCout_mus")

