import Test.DocTest

main = doctest
  [ "-isrc"
  , "src/ForSyDe/Shallow/Core/Vector.hs"
  , "src/ForSyDe/Shallow/Utility/Matrix.hs"
  ]
