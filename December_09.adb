with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_09 is

   function Inflator (Input_Line : in Unbounded_String;
                      Version_Two : in Boolean) return Long_Long_Integer is

      Result : Long_Long_Integer := 0;
      Start_At, First, Slice_Length : Positive;
      Last : Natural;
      Repetitions : Long_Long_Integer;

   begin -- Inflator
      --Put_Line (To_String (Input_Line));
      Start_At := 1;
      while Start_At <= Length (Input_Line) loop
         if Element (Input_Line, Start_At) = '(' then
            Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Slice_Length := Positive'Value (Slice (Input_Line, First, Last));
            Start_At := Last + 1;
            Assert (Element (Input_Line, Start_At) = 'x', "expected 'x' found: "
                    & Element (Input_Line, Start_At));
            Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Repetitions :=
              Long_Long_Integer'Value (Slice (Input_Line, First, Last));
            Start_At := Last + 1;
            Assert (Element (Input_Line, Start_At) = ')', "expected ')' found: "
                    & Element (Input_Line, Start_At));
            Start_At := Start_At + 1; -- start of sub string to bs repeated
            if Version_Two then
               Result := Result +
                 Inflator (Unbounded_Slice (Input_Line, Start_At,
                           Start_At + Slice_Length - 1), Version_Two) *
                 Repetitions;
            else
               Result := Result + Long_Long_Integer (Slice_Length) *
                 Repetitions;
            end if; -- Version_Two
            Start_At := Start_At + Slice_Length;
         else
            Result := Result + 1;
            Start_At := Start_At + 1;
         end if; -- Element (Input_Line, Start_At) = '('
      end loop; -- Start_At <= Length
      return Result;
   end Inflator;

   Input_File : File_Type;
   Input_Line : Unbounded_String;

begin  -- December_09
   Open (Input_File, In_File, "20161209.txt");
   Input_Line := To_Unbounded_String (Get_Line (Input_File));
   Close (Input_File);
   Put_Line ("Uncompressed string length: " &
              Long_Long_Integer'Image (Inflator (Input_Line, False)));
   Put_Line ("Uncompressed string length (part 2): " &
               Long_Long_Integer'Image (Inflator (Input_Line, True)));
end December_09;
