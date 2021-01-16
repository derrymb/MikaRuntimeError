package body maximum is

procedure Exchange(X, Y : in out my_float) 
is 
T : my_float; 
begin 
  T := Y; 
  Y := X; 
  X := T; 
end Exchange;

procedure Maximum(X, Y : in out my_float) 
is 
begin 
  if X > Y then 
    Max := X; 
  else 
    Exchange(X, Y); 
    Max := X; 
  end if; 
end Maximum;

procedure Mika_Test_Point(Test_number : in Integer) is separate;

begin
  null;
end maximum;