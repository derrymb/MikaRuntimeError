package body Runtime is
    procedure DivisionError (A , B : in out Integer) is
        X : Integer := 1;
        Y : Integer := 0;
    begin
        if A /= 0 then
            Y := 3 + X;
        end if;
        if B = 0  then
            X := 2 * (A + B);
        end if;
        A := 100/(X - Y); 
    end DivisionError;

    procedure Error is
        A : Integer := 1; 
        B : Integer := 0; 
    begin
        DivisionError(A, B);
    end Error;
end Runtime;