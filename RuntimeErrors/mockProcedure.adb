package body mockProcedure is

procedure SecretMikaCall(E : in Boolean) is
begin
    null;
end SecretMikaCall;

procedure Constraint is
    type Custom_Int is new Integer range 0 .. 50;
    type Index is range 1 .. 10;
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
    SecretMikaCall(Y < 50);
    Array1 (R) := T;
end Constraint;

end mockProcedure;