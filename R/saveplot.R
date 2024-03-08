saveplot = function(df, df_bbl, var) {
  
  p = decision_maker2(df,df_bbl, var)
  
  var_type = df_bbl[df_bbl$Variable...Field.name == var, "Field.type"]
  
  var_stype = df_bbl[df_bbl$Variable...Field.name == var, "Field.input.type"]
  
  if ((var_type != "comment") || (var_type != "text" & var_stype != "text" )){
    
    name = paste0("plot/",var,".png")
    
    ggsave(name,p, width = 7, height = 4)
    
  } 
}