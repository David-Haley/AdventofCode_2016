with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure December_04 is

   subtype Code_Letters is Character range 'a' .. 'z';

   type Histograms is array (Code_Letters) of Natural;

   subtype Sectors is Positive range 1 .. 9999;

   subtype Checksums is String (1 .. 5);

   procedure Build_Histogram (Input_Line : in  Unbounded_String;
                              Histogram : out Histograms;
                              Last : out Natural) is

      First : Positive;

   begin -- Build_Histogram
      Histogram := (others => 0);
      Find_Token (Input_Line, Decimal_Digit_Set, Outside, First, Last);
      for Index in Positive range 1 .. last loop
         if Element (Input_Line, Index) in Code_Letters then
            Histogram (Element (Input_Line, Index)) :=
              Histogram (Element (Input_Line, Index)) + 1;
         end if; -- Element (Input_Line, Index) in Code_Letters
      end loop; -- Index in Positive range 1 .. last
   end Build_Histogram;

   procedure Get_Sector (Input_Line : in  Unbounded_String;
                         Sector : out Sectors;
                         Start_At : in Positive;
                         Last : out Natural) is

      First : Positive;

   begin -- Get_Sector
      Find_Token (Input_Line, Decimal_Digit_Set, Start_At, Inside, First,
                  Last);
      Sector := Sectors'Value (Slice (Input_Line, First, Last));
   end Get_Sector;

   function Get_Checksum (Input_Line : in  Unbounded_String;
                          Start_At : in Positive) return Checksums is

      First, Last : Natural;

   begin -- Get_Checksum
      First := Index (Input_Line, "[", Start_At, Forward) + 1;
      Last := Index (Input_Line, "]", First, Forward) - 1;
      return Slice (Input_Line, First, Last);
   end Get_Checksum;

   function Calculate_Checksum (Histogram : in Histograms) return Checksums is

      Maximum : Natural := 0;
      Result : Checksums;
      Checksum_Index : Positive := 1;

   begin -- Calculate_Checksum
      for I in Code_Letters loop
         if Maximum < Histogram (I) then
            Maximum := Histogram (I);
         end if; -- Maximum < Histogram (I)
      end loop; -- I in Code_Letters
      while Maximum > 0 and Checksum_Index <= Checksums'Last loop
         for I in Code_Letters loop
            if Maximum = Histogram (I) and
              Checksum_Index <= Checksums'Last then
               Result (Checksum_Index) := I;
               Checksum_Index := Checksum_Index + 1;
            end if; -- Maximum = Histogram (I) ...
         end loop; -- I in Code_Letters
         Maximum := Maximum - 1;
      end loop; -- Maximum > 0 and Checksum_Index <= Checksums'Last
      return Result;
   end Calculate_Checksum;

   function Sector_Sum return Natural is

      Input_File : File_Type;
      Input_Line : Unbounded_String;
      Start_At : Positive;
      Last : Natural;
      Histogram : Histograms;
      Sector : Sectors;
      Checksum : Checksums;
      Result : Natural := 0;

   begin --  Sector_Sum
      Open (Input_File, In_File, "20161204.txt");
      while not End_Of_File (Input_File) loop
         Input_Line := To_Unbounded_String (Get_Line (Input_File));
         Build_Histogram (Input_Line, Histogram, Last);
         Start_At := Last + 1;
         Get_Sector (Input_Line, Sector, Start_At, Last);
         Start_At := Last + 1;
         Checksum := Get_Checksum (Input_Line, Start_At);
         if Checksum = Calculate_Checksum (Histogram) then
            Result := Result + Sector;
         end if; -- Checksum = Calculate_Checksum (Histogram)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      return Result;
   end  Sector_Sum;

   function Find_North_Pole_Objects return Sectors is

      function Substitute (Ch : in Character; Sector : in Sectors)
                           return Character is

      begin -- Substitute
         return Character'Val
           (Character'Pos ('a') + (Character'Pos (Ch) - Character'Pos ('a') +
                Sector) mod 26);
      end Substitute;

      Input_File : File_Type;
      Input_Line, Room_Name : Unbounded_String;
      Start_At : Positive;
      Last : Natural;
      Histogram : Histograms;
      Sector : Sectors;
      Checksum : Checksums;
      Room_Name_End : Positive;

   begin -- Find_North_Pole_Objects
      Open (Input_File, In_File, "20161204.txt");
      while not End_Of_File (Input_File) loop
         Input_Line := To_Unbounded_String (Get_Line (Input_File));
         Build_Histogram (Input_Line, Histogram, Last);
         Room_Name_End := Last;
         Start_At := Last + 1;
         Get_Sector (Input_Line, Sector, Start_At, Last);
         Start_At := Last + 1;
         Checksum := Get_Checksum (Input_Line, Start_At);
         if Checksum = Calculate_Checksum (Histogram) then
            Room_Name := Null_Unbounded_String;
            for Index in Positive range 1 .. Room_Name_End loop
               if Element (Input_Line, Index) in Code_Letters then
                  Room_Name := Room_Name &
                    Substitute (Element (Input_Line, Index), Sector);
               else
                  Room_Name := Room_Name & ' ';
               end if; -- Element (Input_Line, Index) in Code_Letters
            end loop; -- Index in Positive range 1 .. Room_Name_End
            exit when Index (Room_Name, "north") > 0 and
              Index (Room_Name, "pole") > 0 and
              Index (Room_Name, "object") > 0;
         end if; -- Checksum = Calculate_Checksum (Histogram)
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      Put_Line (To_String (Room_Name));
      return Sector;
   end Find_North_Pole_Objects;

begin  -- December_04
   Put_Line ("Sector sum:" &
               Natural'Image (Sector_Sum));
   Put_Line ("North pole objects sector" &
               Sectors'Image (Find_North_Pole_Objects));
end December_04;
