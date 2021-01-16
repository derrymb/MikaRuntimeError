--  Test Points solution for test drivers
--  foo is the package name where the subprogram under test is situated
--To reuse this file:   0 change separate(...) to separate(foo) below 
--	                1 save this file as 'foo-mika_test_point.adb'  in foo's directory or in your working directory
--	                2 add in foo.ads the declaration:	procedure Mika_Test_Point(Test_number : in Integer);
--	                3 add in foo.adb the declaration:	procedure Mika_Test_Point(Test_number : in Integer) is separate;
separate (array_date)
procedure Mika_Test_Point(Test_number : in Integer) is
begin
  Null;
end Mika_Test_Point;