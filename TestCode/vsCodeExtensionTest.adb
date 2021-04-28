package body vsCodeExtensionTest is

procedure Test(X, Y : in out Integer) 
is 
Z : Integer; 
begin 
    Z := 500;
    if  X > Z then
        Z := Z * Y;
    elsif Y > Z then
        Z := Z * X;
    else
        Z := X+Y;
        --#MIKA Z = 175 and Y = 37
    end if;
end Test;

end vsCodeExtensionTest;