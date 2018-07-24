with Ada.Text_IO; use Ada.Text_IO;

procedure Solution_12a is

   Register_A : Integer := 0;
   Register_B : Integer := 0;
   Register_C : Integer := 1;
   Register_D : Integer := 0;

begin -- Solution_12a
   --   1: cpy 1 a
   Register_A :=  1;
   --   2: cpy 1 b
   Register_B :=  1;
   --   3: cpy 26 d
   Register_D :=  26;
   --   4: jnz c 2
   if Register_C /= 0 then
      goto Label_6;
   end if;
   --   5: jnz 1 5
   if  1 /= 0 then
      goto Label_10;
   end if;
   --   6: cpy 7 c
   <<Label_6>>
   Register_C :=  7;
   --   7: inc d
   <<Label_7>>
   Register_D := Register_D + 1;
   --   8: dec c
   Register_C := Register_C - 1;
   --   9: jnz c -2
   if Register_C /= 0 then
      goto Label_7;
   end if;
   --  10: cpy a c
   <<Label_10>>
   Register_C := Register_A;
   --  11: inc a
   <<Label_11>>
   Register_A := Register_A + 1;
   --  12: dec b
   Register_B := Register_B - 1;
   --  13: jnz b -2
   if Register_B /= 0 then
      goto Label_11;
   end if;
   --  14: cpy c b
   Register_B := Register_C;
   --  15: dec d
   Register_D := Register_D - 1;
   --  16: jnz d -6
   if Register_D /= 0 then
      goto Label_10;
   end if;
   --  17: cpy 16 c
   Register_C :=  16;
   --  18: cpy 12 d
   <<Label_18>>
   Register_D :=  12;
   --  19: inc a
   <<Label_19>>
   Register_A := Register_A + 1;
   --  20: dec d
   Register_D := Register_D - 1;
   --  21: jnz d -2
   if Register_D /= 0 then
      goto Label_19;
   end if;
   --  22: dec c
   Register_C := Register_C - 1;
   --  23: jnz c -5
   if Register_C /= 0 then
      goto Label_18;
   end if;
   Put_Line ("Register_A:" & Integer'Image (Register_A));
end Solution_12a;
