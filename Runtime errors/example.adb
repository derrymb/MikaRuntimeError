package body Example is
    procedure Foo (A , B : in out Integer) is
        X : Integer := 1;
        Y : Integer := 0;
    begin
        if A /= 0 then
            Y := 3 + X;
        end if;
        if B = 0  then
            X := 2 * (A + B);
        end if;
        A := 100 / (X - Y); 
    end Foo;

    procedure Bar is
        A : Integer := 2; 
        B : Integer := 0; 
    begin
        Foo(A, B);
    end Bar;
end Example;