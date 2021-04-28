with Ada.Text_IO;

procedure test is
    subtype my_int is Integer range 1 .. 10;
    X : Integer;
    Y : my_int;
begin
    X:=2;
    Y:=5;
    if X < 5 then
        Ada.Text_IO.Put_Line("Hello");
    else
        Ada.Text_IO.Put_Line("Goodbye");
    end if;

    if Y = 5 then
        Ada.Text_IO.Put_Line("Custom types Baby!!!");
    end if;
end test;
