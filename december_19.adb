with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Unchecked_Deallocation;

procedure December_19 is

   Input_File : File_Type;
   My_Input : Positive;

   package Positive_IO is new Ada.Text_IO.Integer_IO (Positive);
   use Positive_IO;

   type Elves;
   type Elf_Pointers is access all Elves;
   type Elves is record
      Number : Positive;
      Present_Count : Natural;
      Next_Elf, Previous_Elf : Elf_Pointers;
   end record; -- Elves

   Current_Elf : Elf_Pointers;

   procedure Create_Elf_Circle (My_Input : in Positive;
                                First_Elf : out Elf_Pointers) is

      Current_Elf : Elf_Pointers;

   begin -- Create_Elf_Circle
      First_Elf := new Elves'(1, 1, null, null);
      Current_Elf := First_Elf;
      for I in Positive range 2 .. My_Input loop
         Current_Elf.Next_Elf := new Elves'(I, 1, null, Current_Elf);
         Current_Elf := Current_Elf.Next_Elf;
      end loop; -- I in Positive range 2 .. My_Input
      -- close circle
      Current_Elf.Next_Elf := First_Elf;
      First_Elf.Previous_Elf := Current_Elf;
   end Create_Elf_Circle;

   procedure Delete_Elf is new Ada.Unchecked_Deallocation (Elves, Elf_Pointers);

   procedure Winning_Elf (Current_Elf : in out Elf_Pointers;
                          My_Input : in Positive) is

      -- Uses Global My_Input

      Unlink : Elf_Pointers;

   begin -- Winning_Elf
      while Current_Elf.Next_Elf /= Current_Elf loop
         -- more than one elf in circle
         Unlink := Current_Elf.Next_Elf;
         Current_Elf.Present_Count := Current_Elf.Present_Count +
           Unlink.Present_Count;
         Current_Elf.Next_Elf := Unlink.Next_Elf;
         Current_Elf.Next_Elf.Previous_Elf := Current_Elf;
         Delete_Elf (Unlink);
         Current_Elf := Current_Elf.Next_Elf;
      end loop; -- Current_Elf.Next_Elf /= Current_Elf
      assert (Current_Elf.Previous_Elf = Current_Elf, "Inconsistent back link");
      assert (Current_Elf.Present_Count = My_Input, "wrong Present_Count" &
                Natural'Image (Current_Elf.Present_Count));
   end Winning_Elf;

   procedure Cross_Table_Winning_Elf (Current_Elf : in out Elf_Pointers;
                                      My_Input : in Positive) is

      Unlink, Next_to_Unlink : Elf_Pointers;
      Elf_Count : Natural := My_Input;

   begin -- Cross_Table_Winning_Elf
      Unlink := Current_Elf;
      for I in Positive range 1 .. My_Input / 2 loop
         Unlink := Unlink.Next_Elf;
      end loop; -- I in Positive range 1 .. Elf_Count / 2
      while Current_Elf.Next_Elf /= Current_Elf loop
         -- more than one elf in circle
         Current_Elf.Present_Count := Current_Elf.Present_Count +
           Unlink.Present_Count;
         Unlink.Previous_Elf.Next_Elf := Unlink.Next_Elf;
         Unlink.Next_Elf.Previous_Elf := Unlink.Previous_Elf;
         Elf_Count := Elf_Count - 1;
         if Elf_Count mod 2 = 1 then
            Next_to_Unlink := Unlink.Next_Elf;
         else
            Next_to_Unlink := Unlink.Next_Elf.Next_Elf;
         end if; -- Elf_Count mod 2 = 1
         Delete_Elf (Unlink);
         Unlink := Next_to_Unlink;
         Current_Elf := Current_Elf.Next_Elf;
      end loop; -- Current_Elf.Next_Elf /= Current_Elf
      assert (Current_Elf.Previous_Elf = Current_Elf, "Inconsistent back link");
      assert (Current_Elf.Present_Count = My_Input, "wrong Present_Count" &
                Natural'Image (Current_Elf.Present_Count));
   end Cross_Table_Winning_Elf;

begin -- December_19
   Open (Input_File, In_file, "20161219.txt");
   Get (Input_File, My_Input);
   Close (Input_File);
   Create_Elf_Circle (My_Input, Current_Elf);
   Winning_Elf (Current_Elf, My_Input);
   Put_Line ("Elf with presents:" & Positive'Image (Current_Elf.Number));
   Delete_Elf (Current_Elf);
   Create_Elf_Circle (My_Input, Current_Elf);
   Cross_Table_Winning_Elf (Current_Elf, My_Input);
   Put_Line ("Part two elf with presents:" &
               Positive'Image (Current_Elf.Number));
   Delete_Elf (Current_Elf);
end December_19;
