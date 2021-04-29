package body vsCodeExtensionTest is

procedure Test(X, Y : in out Integer) 
is 
type Basic_Record is
    record
        A : Integer;
    end record;
type Primary_Color is (Red, Green, Blue);
type Index is range 1 .. 5;
type My_Int_Array is array (Index) of Integer;
Z : Integer; 
Record_Test : Basic_Record;
Color : Primary_Color;
My_Array : My_Int_Array;
begin 
    Z := 500;
    Record_Test := (A => 42);
    Color := Red;
    My_Array := (12,13,5,17,57);
    if  X > Z then
        Z := Z * Y;
        Color := Blue;
    elsif Y > Z then
        Z := Z * X;
        Color := Green;
    else
        Z := X+Y;
        --#MIKA Z = 175 and Y = 37
    end if;
    if Color = Green then
        My_Array(2) := 35;
        Record_Test := (A => My_Array(5));
    end if;
    ----#MIKA Record_test.A = 57 and Color = Blue
    ----#MIKA Color = Green or 
end Test;

end vsCodeExtensionTest;