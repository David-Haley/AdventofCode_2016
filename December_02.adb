with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_02 is

   subtype Coordinates is Integer range -3 .. 3;

   type Locations is record
      X, Y : Coordinates;
   end record; -- Locations

   type Keypads is array (Coordinates, Coordinates) of Character;

   -- Part 1 keypad
   --    -1 0 1

   --  1  1 2 3
   --  0  4 5 6
   -- -1  7 8 9

   Keypad1 : constant Keypads :=
     (-1 => (1 => '1', 0 => '4', -1 => '7', others => ' '),
      0 =>  (1 => '2', 0 => '5', -1 => '8', others => ' '),
      1 =>  (1 => '3', 0 => '6', -1 => '9', others => ' '),
      others => (others => ' '));

   -- Part 2 keypad
   --   -2-1 0 1 2

   --  2     1
   --  1   2 3 4
   --  0 5 6 7 8 9
   -- -1   A B C
   -- -2     D

   Keypad2 : constant Keypads :=
     (-2 =>                      (0 => '5', others => ' '),
      -1 =>            (1 => '2', 0 => '6', -1 => 'A', others => ' '),
      0 =>  (2 => '1',  1 => '3', 0 => '7', -1 => 'B', -2 => 'D', others => ' '),
      1 =>             (1 => '4', 0 => '8', -1 => 'C', others => ' '),
      2 =>                       (0 => '9', others => ' '),
      others => (others => ' '));

   function Find_Code (Keypad : in Keypads;
                      Start : in Locations) return String is

      Location : Locations := Start;
      Input_File : File_Type;
      Step : Character;
      Code : Unbounded_String := Null_Unbounded_String;

   begin -- Find_Code
      Open (Input_File, In_File, "20161202.txt");
      for Y in reverse Coordinates loop
         for X in Coordinates loop
            Put (Keypad (X, Y));
         end loop; -- X in Coordinates
         New_Line;
      end loop; --  Y in reverse Coordinates
      while not End_Of_File (Input_File) loop
         while not End_Of_Line (Input_File) loop
            Get (Input_File, Step);
            case Step is
            when 'U' | 'u' =>
               if Keypad (Location.X, Location.Y + 1) /= ' ' then
                  Location.Y := Location.Y + 1;
               end if; -- Keypad (Location.X, Location.Y) /= ' '
            When 'R' | 'r' =>
               if Keypad (Location.X + 1, Location.Y) /= ' ' then
                  Location.X := Location.X + 1;
               end if; -- Keypad (Location.X, Location.Y) /= ' '
            when 'D' | 'd' =>
               if Keypad (Location.X, Location.Y - 1) /= ' ' then
                  Location.Y := Location.Y - 1;
               end if; -- Keypad (Location.X, Location.Y) /= ' '
            when 'L' | 'l' =>
               if Keypad (Location.X - 1, Location.Y) /= ' ' then
                  Location.X := Location.X - 1;
               end if; -- Keypad (Location.X, Location.Y) /= ' '
            when others =>
               Assert (False, "Unexpected character '" & Step & "' (" &
                         Positive_Count'Image (Col (Input_File)) & ')');
            end case; -- Step
         end loop; -- not End_Of_Line (Input_File)
         Code := Code & Keypad (Location.X, Location.Y);
         Skip_Line (Input_File);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      return To_String (Code);
   end Find_Code;

begin  -- December_02
   Put_Line ("Bath Room Code: " & Find_Code (Keypad1, (0, 0)));
   Put_Line ("Bath Room Code (Part 2): " & Find_Code (Keypad2, (-2, 0)));
end December_02;
