with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.MD5;

procedure December_05 is

   Input_File : File_Type;
   Input_Line : Unbounded_String;

   Hash : String ( 1 .. 32);
   Code : String (1 .. 8);
   I : Natural;
   Digit_Position : Positive;

begin -- December_05
   Open (Input_File, In_File, "20161205.txt");
   Input_Line := To_Unbounded_String (Get_Line (Input_File));
   Close (Input_File);
   I := 0;
   for Code_index in Positive range 1 .. Code'Length loop
      loop
         Hash := GNAT.MD5.Digest (To_string (Input_Line) &
                                    Trim (Natural'Image (I), Left));
         I := I + 1;
         exit when Hash (1 .. 5) = "00000";
      end loop; -- find 00000
      Code (Code_Index) := Hash (6);
   end loop; -- Code_index in Positive range 1 to Length (Code)
   Put_Line ("Easter Bunny HQ door code: " & Code);
   I := 0;
   Code := (others => ' ');
   while Index (Code, " ") > 0 loop
      loop
         Hash := GNAT.MD5.Digest (To_string (Input_Line) &
                                    Trim (Natural'Image (I), Left));
         I := I + 1;
         exit when Hash (1 .. 5) = "00000";
      end loop; -- find 00000
      Digit_Position := Natural'Value ("16#" & Hash (6) & '#') + 1;
      if Digit_Position <= Code'Length and then Code (Digit_Position) = ' ' then
         Code (Digit_Position) := Hash (7);
      end if; -- within the string and the first value for that location
   end loop; -- Index (Code, " ") > 0
   Put_Line ("Easter Bunny HQ second door code: " & Code);
end December_05;
