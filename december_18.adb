with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with GNAT.MD5; use GNAT.MD5;

procedure December_18 is

   Input_File : File_Type;
   My_Input : Unbounded_String;

   function Count_Safe (My_Input : in Unbounded_String;
                        Last_Row : in Positive) return Natural is

      Safe : constant Character := '.';
      Trap : constant Character := '^';

      subtype Tiles is Character
        with Static_Predicate => Tiles in Safe | Trap;

      subtype Tile_Indices is Positive range 1 .. Length (My_Input);

      subtype Row_Strings is String (Tile_Indices'Range);
      Current_Row : Row_Strings := To_String (My_Input);
      Next_Row : Row_Strings;
      Number_Safe : Natural;
      subtype Rows is Positive range 2 .. Last_Row;
      -- first row is My_Input
      Safe_Set : Character_Set := To_Set (Safe);

      function New_Tile (Row : in Row_Strings; Index : in Tile_Indices)
                      return Tiles is

         Left, Centre, Right : Tiles;

      begin -- New_Tile
         if Index = Tile_Indices'First then
            Left := Safe;
         else
            Left := Row (Index - 1);
         end if; -- Index = Tile_Indices'First
         if Index = Tile_Indices'Last then
            Right := Safe;
         else
            Right := Row (Index + 1);
         end if; -- Index = Tile_Indices'Last
         Centre := Row (Index);
         if Left = Trap and Centre = Trap and Right /= Trap then
            return Trap;
         elsif Left /= Trap and Centre = Trap and Right = Trap then
            return Trap;
         elsif Left = Trap and Centre /= Trap and Right /= Trap then
            return Trap;
         elsif  Left /= Trap and Centre /= Trap and Right = Trap then
            return Trap;
         else
            return Safe;
         end if; -- apply rules
      end New_Tile;

   begin -- Count_Safe
      Number_Safe := Ada.Strings.Fixed.Count (Current_Row, Safe_Set);
      for Row in Rows loop
         for Index in Tile_Indices loop
            Next_Row (Index) := New_Tile (Current_Row, Index);
         end loop; -- Index in Tile_Indices
         Number_Safe := Number_Safe +
           Ada.Strings.Fixed.Count (Next_Row, Safe_Set);
         Current_Row := Next_Row;
      end loop; -- Row in Rows
      return Number_Safe;
   end Count_Safe;

begin -- December_18
   Open (Input_File, In_file, "20161218.txt");
   Get_Line (Input_File, My_Input);
   Close (Input_File);
   Put_Line ("Safe tiles:" & Natural'Image (Count_Safe (My_Input, 40)));
   Put_Line ("Part two safe tiles:" &
               Natural'Image (Count_Safe (My_Input, 400000)));
end December_18;
