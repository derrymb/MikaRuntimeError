package maximum is
  subtype my_float is float range -100000.0 .. 100000.0;
  Max : my_float;
  procedure Maximum(X, Y : in out my_float);
  procedure Mika_Test_Point(Test_number : in Integer);
end maximum;