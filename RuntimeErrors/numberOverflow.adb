with Ada.Text_IO; use Ada.Text_IO;
procedure numberOverflow is
    type Custom_Int is new Integer range 0 .. 50;
    X: Custom_Int := 4;
    Y: Custom_Int := 1;
    Z: Custom_Int := 10;
    T: Custom_Int := 42;
begin
    Y := (X - Z) + T;
    Put_Line(Custom_Int'Image(Y));
end numberOverflow;




