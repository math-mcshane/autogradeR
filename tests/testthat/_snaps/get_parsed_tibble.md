# Able to parse a script.

    Code
      get_parsed_tibble(filename = example_script("generic_script"))
    Output
      # A tibble: 120 x 9
         line1  col1 line2  col2    id parent token                terminal text      
         <int> <int> <int> <int> <int>  <int> <chr>                <lgl>    <chr>     
       1     1     1     1    23     1    -26 COMMENT              TRUE     "# This i~
       2     3     1     3    18     6    -26 COMMENT              TRUE     "# regula~
       3     4     1     4    26    26      0 expr                 FALSE    ""        
       4     4     1     4    13    12     26 expr                 FALSE    ""        
       5     4     1     4     5     9     12 SYMBOL_PACKAGE       TRUE     "dplyr"   
       6     4     6     4     7    10     12 NS_GET               TRUE     "::"      
       7     4     8     4    13    11     12 SYMBOL_FUNCTION_CALL TRUE     "select"  
       8     4    14     4    14    13     26 '('                  TRUE     "("       
       9     4    15     4    18    14     16 SYMBOL               TRUE     "cars"    
      10     4    15     4    18    16     26 expr                 FALSE    ""        
      # i 110 more rows

