with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;

procedure December_19 is

   Input_File : File_Type;
   My_Input : Positive;

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);
   use Positive_IO;

   function Winning_Elf (My_Input : in Positive) return Positive is

      subtype Elf_Indices is Positive range 1 .. My_input;
      Elf : array (Elf_Indices) of Natural := (others => 1);

      Current : Elf_Indices := 1;
      Left : Elf_Indices;

      procedure Next_Elf (Elf_Index : in out Elf_Indices) is

      begin -- Next_Elf
         if Elf_Index < Elf_Indices'Last then
            Elf_Index := Elf_Index + 1;
         else
            Elf_Index := Elf_Indices'First;
         end if; -- Elf_Index < Elf_Indices'Last
      end Next_Elf;

   begin --  Winning_Elf
      loop -- around the circle
         while Elf (Current) = 0 loop
            Next_Elf (Current);
         end loop; -- Elf (Current) = 0
         Left := Current;
         Next_Elf (Left);
         while Elf (Left) = 0 and Left /= Current loop
            Next_Elf (Left);
         end loop; --  Elf (Left) = 0 and Left /= Current
         if Current = Left then
            assert (Elf (Current) = My_Input, "wrong number of presents" &
                      Natural'Image (Elf (Current)));
            return Current;
            -- only one elf has presents
         else
            Elf (Current) := Elf (Current) + Elf (Left);
            Elf (Left) := 0;
            Current := Left;
         end if; -- Current = Left
      end loop; -- around the circle
   end  Winning_Elf;

begin -- December_19
   Open (Input_File, In_file, "20161219.txt");
   Get (Input_File, My_Input);
   Close (Input_File);
   Put_Line ("Elf with presents:" & Positive'Image (Winning_Elf (My_Input)));
end December_19;
