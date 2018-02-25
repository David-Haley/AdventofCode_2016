with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_06 is

   subtype Letters is Character range 'a' .. 'z';

   subtype Messages is String (1 .. 8);
   subtype Position_Indices is Positive range 1 .. Messages'Length;

   type Histograms is array (Letters) of Natural;
   type Histogram_Tables is array (Position_Indices) of Histograms;

   procedure Build_Histogram_Table (Histogram_Table : out Histogram_Tables) is

      Input_File : File_Type;
      Message : Messages;

   begin -- Build_Histogram_Table
      Open (Input_File, In_File, "20161206.txt");
      Histogram_Table := (others => (others => 0));
      while not End_Of_File (Input_File) loop
         Message := Get_Line (Input_File);
         for Position_Index in Position_Indices loop
            Histogram_Table (Position_index) (Message (Position_index)) :=
              Histogram_Table (Position_index) (Message (Position_index)) + 1;
         end loop; -- Position_Index in Position_Indices
      end loop; -- Position_Index in Position_Indices
      Close (Input_File);
   end Build_Histogram_Table;

   function Calculate_Message (Histogram_Table : in Histogram_Tables)
                               return Messages is

      Most_Frequent : Natural;
      Result : Messages;

   begin -- Calculate_Message
      for Position_Index in Position_Indices loop
         Most_Frequent := 0;
         for I in Letters loop
            if Most_Frequent < Histogram_Table (Position_Index) (I) then
               Most_Frequent := Histogram_Table (Position_Index) (I);
            end if; -- Most_Frequent < Histogram (I)
         end loop; -- I in Letters
         for I in Letters loop
            if Most_Frequent = Histogram_Table (Position_Index) (I) then
               Result (Position_Index) := I;
               exit;
            end if; -- Most_Frequent = Histogram_Table (Position_Index) (I)
         end loop; -- I in Letters
      end loop; -- Position_Index in Position_Indices
      return Result;
   end Calculate_Message;

   function Calculate_Modified_Message (Histogram_Table : in Histogram_Tables)
                               return Messages is

      Least_Frequent : Natural;
      Result : Messages;

   begin -- Calculate_Modified_Message
      for Position_Index in Position_Indices loop
         Least_Frequent := Natural'Last;
         for I in Letters loop
            if Least_Frequent > Histogram_Table (Position_Index) (I) then
               Least_Frequent := Histogram_Table (Position_Index) (I);
            end if; -- Least_Frequent < Histogram (I)
         end loop; -- I in Letters
         for I in Letters loop
            if Least_Frequent = Histogram_Table (Position_Index) (I) then
               Result (Position_Index) := I;
               exit;
            end if; -- Least_Frequent = Histogram (I) ...
         end loop; -- I in Letters
      end loop; -- Position_Index in Position_Indices
      return Result;
   end Calculate_Modified_Message;

   Histogram_Table : Histogram_Tables;

begin  -- December_06
   Build_Histogram_Table (Histogram_Table);
   Put_Line ("Corrected Mesaeg: " & Calculate_Message (Histogram_Table));
   Put_Line ("Corrected Mesaeg (modified protocol): " &
               Calculate_Modified_Message (Histogram_Table));
end December_06;
