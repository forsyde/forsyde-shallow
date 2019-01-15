import Test.DocTest

main = doctest
  [ "-isrc"
  , "src/ForSyDe/Shallow/MoC/Synchronous/Lib.hs"
  , "src/ForSyDe/Shallow/MoC/SDF.hs"
  , "src/ForSyDe/Shallow/MoC/CSDF.hs"
  , "src/ForSyDe/Shallow/MoC/SADF.hs"
  , "src/ForSyDe/Shallow/Core/Vector.hs"
  , "src/ForSyDe/Shallow/Utility/Matrix.hs"
  ]
