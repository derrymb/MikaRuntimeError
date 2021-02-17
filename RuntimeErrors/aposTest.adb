with Ada.Text_IO; use Ada.Text_IO;
procedure AposTest is
    type Custom_Int is new Integer range 0 .. 50;
    type Index is range 1 .. 10;
    --                  ^ Low range can be any value, not just 0
    type Custom_Int_Array is array (Index) of Custom_Int;
    Array1: Custom_Int_Array := (1,2,3,4,5,6,7,8,9,10);
    X: Custom_Int := 4;
    Y: Custom_Int := 1;
    W: Index := 2;
    R: Index := 4;
    T: Custom_Int := 42;
begin
    if 10 /= 0 then
            T := 10;
        end if;
    Array1 (R) := T;
    Put(Array1\'First);
    --for I in Index loop
      --  Put_Line(Custom_Int'Image (Array1(I)));
    --end loop;
    --Put_Line("Array1'First = " & Index'Image (Array1'First) & " Array1'Last = " & Index'Image (Array1'Last));
    --Put_Line("Next we will attempt to enter 5 into Array(10) which is outside of the range of the array");
    --Array1 (13) := 50;
end AposTest;