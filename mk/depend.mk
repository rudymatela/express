bench/tiers.o: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs \
  bench/tiers.hs
bench/tiers: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  bench/tiers.hs \
  mk/toplibs
mk/All.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Core.hs \
  mk/Toplibs.hs
Setup.o: \
  Setup.hs
Setup: \
  Setup.hs \
  mk/toplibs
src/Data/Haexpress/Core.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress/Core.hs
src/Data/Haexpress/Fixtures.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
src/Data/Haexpress/Name.o: \
  src/Data/Haexpress/Name.hs
src/Data/Haexpress.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Core.hs
src/Data/Haexpress/Utils/String.o: \
  src/Data/Haexpress/Utils/String.hs
test/fixtures.o: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  test/fixtures.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
test/fixtures: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  test/fixtures.hs \
  mk/toplibs
test/listable.o: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  test/listable.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
test/listable: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  test/listable.hs \
  mk/toplibs
test/main.o: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  test/main.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
test/main: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  test/main.hs \
  mk/toplibs
test/Test/ListableExpr.o: \
  test/Test/ListableExpr.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
test/Test.o: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Name.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
test/Test: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  mk/toplibs
