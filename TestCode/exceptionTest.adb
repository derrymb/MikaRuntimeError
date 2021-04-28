package body exceptionTest is

procedure Test(X, Y : in out Integer) 
is 
T : Integer; 
begin 
    T := X/Y;
end Test;

procedure Test2(X: in out Integer)
is
Number : Integer;
Number2 : Integer;
Number3 : Integer; 
begin
Number := 100;
Number2 := 7;
Number3 := Number2-X;
X := Number/Number3;
end Test2;

procedure arrayAccess1(A: integer) is
    B : array (Integer range 1..42) of Integer := (others => 42);
begin
    B(A) := 1000;
end arrayAccess1;

procedure arrayAccess2(A: integer; X: out Integer) is
    B : array (Integer range 1..42) of Boolean := (others => false);
begin
    if (A >= 1 or A <= 42) then B(A) := True;
    else B(1) := false;
    end if;
    X := A;
end arrayAccess2;

end exceptionTest;