-- start local variable with "let", end with "in"
foo = let a = 1 
      in let b = 2
         in a + b

-- shadowing, legal but not wise
bar = let x = 1
      in ((let x = "foo" in x), x)


lol a = let a = "foo"
        in a ++ "eek"