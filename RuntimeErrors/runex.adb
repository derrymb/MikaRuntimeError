package body runex is

function firstCheckCFG(A, B : Integer) return Integer is
  AX, BX: Integer;
  X : Integer;
begin
  AX := A;
  BX := B;
  if (AX=2) 
    then BX := BX*2;
    else BX := BX+1;
  end if;
  if (A > B) then 
  begin
    X := 1;
    X := 1/(A-43);
    X := 2;
    X := 1/(A-42);
  end;  
    else X := 1/(A+42);
  end if;
  return X;
end firstCheckCFG;

procedure noBranch(A: in Integer; X: out Integer) is
begin
  X := A + 2;
  X := 1 / X;
end noBranch;

procedure noTrueRune(A: in Integer; X: out Integer) is
begin
  X := A + 2;
  if (X = 0) then X := -42;
  else X := 1 / X;
  end if;
end noTrueRune;



procedure Mika_Test_Point(Test_number : in Integer) is separate;

end runex;