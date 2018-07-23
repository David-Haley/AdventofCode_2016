with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with December_16_Library; use December_16_Library;

procedure December_16 is

   Input_File : File_Type;
   My_Input : Unbounded_String;

   function Expand_Data (Seed : in Unbounded_String;
                         Required_Length : in Positive)
                         return Unbounded_String is

      A : Unbounded_String := Seed;
      B : Unbounded_String;

   begin -- Expand_Data
      while Length (A) < Required_Length loop
         B := Null_Unbounded_String;
         for I in reverse Positive range 1 .. Length (A) loop
            Append (B, Element (A, I));
         end loop; --  I in reverse Positive range 1 .. Length (A)
         B := Translate (B, Invert'Access);
         A := A & "0" & B;
      end loop; -- Length (A) < Required_Length
      return Delete (A, Required_Length + 1, Length (A));
   end Expand_Data;

   function Checksum (Data : in Unbounded_String) return Unbounded_String is

      Working : Unbounded_String := Data;
      Result : Unbounded_String;
      Index : Positive;

   begin -- Checksum
      loop -- calculate checksum
         Result := Null_Unbounded_String;
         Index := 1;
         while Index < Length (Working) loop
            if Element (Working, Index) = Element (Working, Index + 1) then
               Append (Result, '1');
            else
               Append (Result, '0');
            end if; -- lement (Working, Index) = Element (Working, Index + 1)
            Index := Index + 2;
         end loop; -- Index < Length (Working)
         exit when Length (Result) mod 2 = 1;
         -- Result is an odd number of characters
         Working := Result;
      end loop; -- calculate checksum
      return Result;
   end Checksum;

begin -- December_16
   Open (Input_File, In_File, "20161216.txt");
   Get_Line (Input_File, My_Input);
   Close (Input_File);
   Put_Line ("Checksum: " & Checksum (Expand_Data (My_Input, 272)));
   Put_Line ("Part two checksum: " & Checksum (Expand_Data (My_Input,
             35651584)));
end December_16;
