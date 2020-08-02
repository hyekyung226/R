## Connecting R to Teradata 


# set proxy
Sys.setenv(HTTP_PROXY="http://outbound.proxy.devdmz.sdghs-engineering.internal:3401")
Sys.setenv(HTTPS_PROXY="http://outbound.proxy.devdmz.sdghs-engineering.internal:3401")
Sys.setenv(NO_PROXY="169.254.169.254")

#install remotes & getPass
install.packages(c("remotes", "getPass"))

install.packages("remotes")

remotes::install_bitbucket("ShopDirect/connectr",
                           auth_user = "owen.garrity@shopdirect.com",
                           password = getPass::getPass())

# install teradata jar files
# install_teradata_jar()

# connection to teradata
con <- src_connectR(drv = TJDBC(),
                    url = "jdbc:teradata://dbccop1/")

#-----------------------------------------------------------------

db_get_query(con, "Opt_Ranking_History") %>% show_query()


tbl(con,SQL("select * from tmg.nd_1819_starters")) %>% head() %>% show_query()
tbl(con,SQL("select * from tmg.nd_supp_t1"))
tbl(con, "nd_supp_t1")
tbl(con,SQL("select * from tmg.nd_master_table"))
tbl(con,SQL("select * from tmg.nd_current_opt_in"))
tbl(con,SQL("select * from tmg.nd_previous_opt_out"))
tbl(con,SQL("select * from tmg.nd_prev_optout_now_in"))

base %>% mutate(model_end_dt=as.Date(model_end_dt, format="%Y-%m-%d")) %>% mutate(model_end_dt=ymd(model_end_dt)) %>% mutate_at(vars(model_end_dt), funs(year, month))
base %>% mutate(month=paste(base$year, base$month, sep="-")) %>% select(-c(year)) %>% mutate(month=str_replace(month, "2018-1", "18-1"))
