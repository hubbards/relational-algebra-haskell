import Test.DocTest ( doctest )

main :: IO ()
main = doctest [
    "src/Pair.hs"
  , "src/Tuple.hs"
  , "src/Relation.hs"
  ]
