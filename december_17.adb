with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with GNAT.MD5; use GNAT.MD5;

procedure December_17 is

   subtype Coordinates is Positive range 1 .. 4;

   type Saved_Paths is record
      Short_Path : Unbounded_String := Null_Unbounded_String;
      Short_Length : Positive := Positive'Last;
      Long_Length : Positive := Positive'First;
   end record; -- Saved_Path

   Saved_Path : Saved_Paths;

   subtype Hashes is String (1 .. 32);
   subtype Hex_Digits is Character
     with Static_Predicate => Hex_Digits in '0' .. '9' | 'a' .. 'f';

   subtype Directions is Character
     with Static_Predicate => Directions in 'U' | 'D' | 'L' | 'R';

   Input_File : File_Type;
   My_Input : Unbounded_String;

   function Is_Open (Hash : in Hashes;
                     Direction : Directions) return Boolean is

      Hex_Digit : Hex_Digits;

   begin -- Is_Open
      case Direction is
         when 'U' =>
            Hex_Digit := Hash (1);
         when 'D' =>
            Hex_Digit := Hash (2);
         when 'L' =>
            Hex_Digit := Hash (3);
         when 'R' =>
            Hex_Digit := Hash (4);
      end case; -- Direction
      case Hex_Digit is
         when 'b' .. 'f' =>
            return True;
         when others =>
            return False;
      end case; -- Hex_Digit
   end Is_Open;

   procedure Find_Target (X, Y : in Coordinates;
                          Path : in String;
                          Saved_Path : in out Saved_Paths) is

      -- Global My_Input is used

      Target_X : Coordinates := Coordinates'Last;
      Target_Y : Coordinates := Coordinates'Last;
      Hash : Hashes := Digest (To_String (My_Input) & Path);

   begin -- Find_Target
      if X = Target_X and Y = Target_Y then
         -- reached target
         if Path'Length < Saved_Path.Short_Length then
            Saved_Path.Short_Length := Path'Length;
            Saved_Path.Short_Path := To_Unbounded_String (Path);
         end if; -- Path'Length < Saved_Path.Short_Length
         if Path'Length > Saved_Path.Long_Length then
            Saved_Path.Long_Length := Path'Length;
         end if; -- Path'Length < Saved_Path.ShortLength
      else
         if X > Coordinates'First and then Is_Open (Hash, 'L') then
            Find_Target (X - 1, Y, Path & 'L', Saved_Path);
         end if; -- X > Coordinates'First and then Is_Open (Hash, 'L')
         if Y > Coordinates'First and then  Is_Open (Hash, 'U') then
            Find_Target (X, Y - 1, Path & 'U', Saved_Path);
         end if; -- Y > Coordinates'First and then  Is_Open (Hash, 'U')
         if X < Coordinates'Last and then Is_Open (Hash, 'R') then
            Find_Target (X + 1, Y, Path & 'R', Saved_Path);
         end if; -- X < Coordinates'Last and then Is_Open (Hash, 'R')
         if Y < Coordinates'Last and then Is_Open (Hash, 'D') then
            Find_Target (X, Y + 1, Path & 'D', Saved_Path);
         end if; -- Y < Coordinates'Last and then (Is_Open (Hash, 'D')
      end if; -- X = Target_X and Y = Target_Y
   end Find_Target;

begin -- December_17
   Open (Input_File, In_file, "20161217.txt");
   Get_Line (Input_File, My_Input);
   Close (Input_File);
   Find_Target (Coordinates'First, Coordinates'First, "", Saved_Path);
   Put_Line ("Shortest Path: " & To_String (Saved_Path.Short_Path));
   Put_Line ("Longest Path Length:" & Positive'Image (Saved_Path.Long_Length));
end December_17;
