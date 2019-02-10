mk/All.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Core.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
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
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
src/Data/Haexpress.o: \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Core.hs
src/Data/Haexpress/Utils/String.o: \
  src/Data/Haexpress/Utils/String.hs
test/main.o: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  test/main.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
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
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
test/Test.o: \
  test/Test.hs \
  test/Test/ListableExpr.hs \
  src/Data/Haexpress/Utils/String.hs \
  src/Data/Haexpress.hs \
  src/Data/Haexpress/Fixtures.hs \
  src/Data/Haexpress/Core.hs
