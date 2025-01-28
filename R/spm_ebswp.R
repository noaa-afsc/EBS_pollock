here::here()
df <- readr::read_csv(here::here("2024","runs","m23","proj","spm_summary.csv"))
#df <- readr::read_csv(here::here("2024","runs","m23","proj","spm_detail.csv"))
names(df)
df |> filter(is.na(Alt), variable %in% c("F_abc","F_ofl")) |> pull(value)

df |> filter(Alt %in% c(1,5),Year==2037) |> group_by(Alt) |> summarize(mean(SPR_Implied) )
df |> filter(Alt %in% c(1,4),Year==2037) |> group_by(Alt) |> summarize(mean(ABC) )
unique(df$variable)
df |> filter(variable=="C_abc")
df |> filter(!is.na(Year),str_starts(variable,"F"),
             str_ends(variable,"ub")|
               str_ends(variable,"lb")|
               str_ends(variable,"ean")
) |>
  pivot_wider(names_from=variable,values_from=value) |> head()
var="SSB";alt=c(1,4,5);mytitle=NULL
var="C";alt=c(1,5,4);mytitle=NULL
plotSPM <- function(df=df, var="SSB",alt=c(1,4,5,7),mytitle=NULL) {
  df |> filter(!is.na(Year),
               Alt %in% alt,
               #str_starts(variable,var),
               str_ends(variable,"ub")|
                 str_ends(variable,"lb")|
                 str_ends(variable,"ean")
  ) |> mutate(Alt=as.factor(Alt)) |>
    pivot_wider(names_from=variable,values_from=value) |>
    ggplot(aes_string(x="Year",y=paste0(var,"_mean"),ymin=paste0(var,"_lb"),
                      ymax=paste0(var,"_ub"),color="Alt",fill="Alt")) +
    geom_line() + ylim(0,NA) + geom_ribbon(color=0,alpha=.2) + theme_minimal() +
    labs(title=mytitle, x="Year",y=paste(var ))
}
plotSPM(var=c("SSB","F"),alt=c(1,4,5,7),mytitle="GOA Flathead Sole")

mydf <- readr::read_csv(here::here("examples","goa_flathead","spm_summary.csv"))
plotSPM2 <- function(df, alt=c(1,4,5,7),mytitle=NULL) {
  df |> filter(!is.na(Year),
               Alt %in% alt,
               str_ends(variable,"ub")|
                 str_ends(variable,"lb")|
                 str_ends(variable,"ean")
  ) |> mutate(
    type = str_extract(variable, "^[^_]+"),
    kind = str_extract(variable, "(?<=_).*"),
    Alt  =  as.factor(Alt)) |>
    pivot_wider(id_cols=-variable, names_from=kind,values_from=value) |>
    ggplot(aes(x=Year,y=mean,ymin=lb, ymax=ub, color=Alt,fill=Alt)) +
    geom_line() + ylim(0,NA) + geom_ribbon(color=0,alpha=.2) + theme_minimal() +
    labs(title=mytitle, x="Year",y=paste(var )) +
    facet_grid(type~Alt,scales="free")
}
plotSPM2(df=mydf)
